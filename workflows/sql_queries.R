# ==============================================================================
# Download the raw fisheries data used by the seafisheries workflows
#
# One file for every query. Each query downloads every species (and, where the
# table holds several, every gear). Species and gear filtering happens
# downstream, in the gear-specific qmd files.
#
# Location: workflows/sql_queries.R. Run from the package root. Outputs go to <data_dir>/<request_tag>/:
#   <name>.RDS      the query result, unmodified apart from duplicated column
#                   names (see dedupe_columns())
#   <name>.sql      the exact SQL that produced it
#   query_log.csv   one line per download: rows, columns, duration, date
#
# Catch columns produced by pivot_species_sql(), for each species in `species`
# and for "oth" (any other species code):
#   <sp>_nrec      number of catch rows for that species (0 = not recorded)
#   <sp>_n         number of fish (sum over rows, NULL rows ignored)
#   <sp>_n_na      number of rows with a NULL number
#   <sp>_<unit>    weight, unit kg or mt depending on the source (NULL rows ignored)
#   <sp>_<unit>_na number of rows with a NULL weight
#   nosp_nrec      number of catch rows with a NULL species code
# The *_na columns let downstream code detect partial weights: a set with 10 YFT
# of which 3 have no weight has a yft_kg summed over 7 fish only.
# ==============================================================================

library(DBI)
library(odbc)

# ---- Parameters ---------------------------------------------------------------

data_dir    <- "data-raw/sql"                  # gitignored (data-raw/*)
request_tag <- format(Sys.Date(), "%Y-%m-%d")  # one folder per download campaign
overwrite   <- FALSE                           # TRUE re-downloads existing files
queries_to_run <- c("EC_LL_captain", "EC_LL_obs", "EC_LL_rai", "LF_tufman", "LF_agg")

# Species pivoted into their own columns; every other code goes to "oth"
species <- c("YFT", "SKJ", "BET", "ALB")

# Filters on the aggregated LF table (lf.LF_MASTER). NULL = no filter.
lf_agg_years   <- NULL       # e.g. c(1961, 2025)
lf_agg_gears   <- NULL       # e.g. "L"
lf_agg_species <- NULL       # e.g. c("YFT", "ALB")
lf_agg_wcpfc   <- NULL       # NULL = all, TRUE = WCPFC 1x1 cells only, FALSE = outside only

# ODBC driver: the first SQL Server driver found, unless set here
sql_driver <- NULL
if (is.null(sql_driver)) {
	drivers <- unique(odbc::odbcListDrivers()$name)
	sql_driver <- drivers[grepl("SQL Server", drivers)][1]
	if (is.na(sql_driver)) stop("No SQL Server ODBC driver found. Installed: ",
								paste(drivers, collapse = ", "))
}
message("ODBC driver: ", sql_driver)

out_dir <- file.path(data_dir, request_tag)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Helpers ------------------------------------------------------------------

sql_in <- function(x) paste0("('", paste(x, collapse = "', '"), "')")

# SQL select items pivoting catch rows by species, for use inside a GROUP BY.
# sp_col: species code column. n_expr: number of fish of one row (NULL when one
# row = one fish). w_expr: weight of one row. w_unit: suffix of weight columns.
# Returns the select items, the column names and which columns are counts
# (to be coalesced to 0 when the outer join finds no catch row).
pivot_species_sql <- function(sp_col, n_expr, w_expr, w_unit, species) {
	groups <- c(stats::setNames(sprintf("%s = '%s'", sp_col, species), tolower(species)),
				oth = sprintf("%s not in %s", sp_col, sql_in(species)))
	items <- character(0); counts <- character(0)
	add <- function(expr, name, is_count) {
		items  <<- c(items, sprintf("%s as %s", expr, name))
		if (is_count) counts <<- c(counts, name)
	}
	for (g in names(groups)) {
		cond <- groups[[g]]
		add(sprintf("sum(case when %s then 1 else 0 end)", cond), paste0(g, "_nrec"), TRUE)
		if (is.null(n_expr)) {
			add(sprintf("sum(case when %s then 1 else 0 end)", cond), paste0(g, "_n"), TRUE)
		} else {
			add(sprintf("sum(case when %s then %s end)", cond, n_expr), paste0(g, "_n"), FALSE)
			add(sprintf("sum(case when %s and %s is null then 1 else 0 end)", cond, n_expr),
				paste0(g, "_n_na"), TRUE)
		}
		add(sprintf("sum(case when %s then %s end)", cond, w_expr), paste0(g, "_", w_unit), FALSE)
		add(sprintf("sum(case when %s and %s is null then 1 else 0 end)", cond, w_expr),
			paste0(g, "_", w_unit, "_na"), TRUE)
	}
	add(sprintf("sum(case when %s is null then 1 else 0 end)", sp_col), "nosp_nrec", TRUE)
	list(select = paste(items, collapse = ",\n\t\t"),
		 names = sub(".* as ", "", items),
		 counts = counts)
}

# Outer select of the pivoted columns, from CTE `alias`; count columns -> 0 when
# the entity has no catch row at all.
outer_catch_sql <- function(piv, alias = "catch") {
	paste(ifelse(piv$names %in% piv$counts,
				 sprintf("coalesce(%s.%s, 0) as %s", alias, piv$names, piv$names),
				 sprintf("%s.%s", alias, piv$names)),
		  collapse = ",\n\t")
}

# `s.*, t.*` style selects can return duplicated column names. Identical
# duplicates are dropped; differing ones are renamed <name>_2, <name>_3, ...
dedupe_columns <- function(df, query_name) {
	nm <- names(df)
	dup_names <- unique(nm[duplicated(nm)])
	if (length(dup_names) == 0) return(df)
	drop <- integer(0)
	for (d in dup_names) {
		idx <- which(nm == d)
		for (j in idx[-1]) {
			if (identical(df[[idx[1]]], df[[j]])) {
				drop <- c(drop, j)
			} else {
				new <- make.unique(c(nm, paste0(d, "_2")))[length(nm) + 1]
				warning(query_name, ": column '", d, "' duplicated with different values, kept as '", new, "'")
				nm[j] <- new
			}
		}
	}
	names(df) <- nm
	if (length(drop)) {
		message(query_name, ": dropped ", length(drop), " identical duplicated column(s): ",
				paste(unique(names(df)[drop]), collapse = ", "))
		df <- df[, -drop, drop = FALSE]
	}
	df
}

# Runs one query, saves result + SQL, appends to the log.
# key: column that must be unique (one row per set / record); NULL = no check.
run_query <- function(name, server, database, sql, key = NULL) {
	if (!name %in% queries_to_run) return(invisible(NULL))
	out_rds <- file.path(out_dir, paste0(name, ".RDS"))
	if (file.exists(out_rds) && !overwrite) {
		message(name, ": ", out_rds, " exists, skipped (set overwrite = TRUE to redo)")
		return(invisible(NULL))
	}
	message(name, ": querying ", server, "/", database, " ...")
	con <- DBI::dbConnect(odbc::odbc(), Driver = sql_driver, Server = server,
						  Database = database, TrustServerCertificate = "yes",
						  Trusted_Connection = "yes")
	on.exit(DBI::dbDisconnect(con), add = TRUE)

	t0  <- Sys.time()
	dat <- DBI::dbGetQuery(con, sql)
	sec <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
	dat <- dedupe_columns(dat, name)

	if (!is.null(key)) {
		n_dup <- sum(duplicated(dat[[key]]))
		if (n_dup > 0) warning(name, ": ", n_dup, " duplicated ", key,
							   " (a join is not 1:1, catch may be double counted)")
	}

	saveRDS(dat, out_rds)
	writeLines(sql, file.path(out_dir, paste0(name, ".sql")))
	log_file <- file.path(out_dir, "query_log.csv")
	utils::write.table(data.frame(query = name, server = server, database = database,
								  rows = nrow(dat), cols = ncol(dat), seconds = sec,
								  downloaded = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
					   log_file, sep = ",", row.names = FALSE,
					   col.names = !file.exists(log_file), append = file.exists(log_file))
	message(name, ": ", nrow(dat), " rows x ", ncol(dat), " cols in ", sec, " s -> ", out_rds)
	invisible(dat)
}

# ---- EC longline, captain logbooks --------------------------------------------
# One row per set (log_set_id), all activity types (filter on activity_id
# downstream; the previous query kept activity_id = 1 only).
# Catch in numbers (SP_N) and kg (SP_KG); effort in hooks.

piv <- pivot_species_sql("c.SP_CODE", "c.SP_N", "c.SP_KG", "kg", species)
run_query("EC_LL_captain", "OFP_DBS", "FISH_MASTER", key = "log_set_id", sql = sprintf("
with catch as (
	select
		c.log_set_id,
		%s
	from log.catch_ll c
	group by c.log_set_id
)
select
	s.*,
	sets_ll.*,
	%s
from log.vw_sets_ll s
	left join log.sets_ll sets_ll on s.log_set_id = sets_ll.log_set_id
	left join catch on s.log_set_id = catch.log_set_id
", piv$select, outer_catch_sql(piv)))

# ---- EC longline, observers ---------------------------------------------------
# One row per set (l_set_id). Position = start of set (stend_id = 83).
# One l_setcatch row = one fish, so <sp>_n = <sp>_nrec. Weight = wt_est (kg).

piv <- pivot_species_sql("lc.sp_code", NULL, "lc.wt_est", "kg", species)
run_query("EC_LL_obs", "OFP_DBS", "obsv_master", key = "l_set_id", sql = sprintf("
with catch as (
	select
		lc.l_set_id,
		%s
	from obsv.l_setcatch lc
	group by lc.l_set_id
)
select
	trip.dep_date as trip_dep_date,
	trip.vessel_id as trip_vessel_id,
	vessel_instance.flag_id as vessel_flag_id,
	vessel_instance.vesselname as vessel_name,
	l_sethaullog.lat as set_start_lat,
	l_sethaullog.lon as set_start_lon,
	l_set.*,
	%s
from obsv.trip trip
	join obsv.l_set l_set on trip.obstrip_id = l_set.obstrip_id
	join obsv.l_sethaullog l_sethaullog on l_set.l_set_id = l_sethaullog.l_set_id
		and l_sethaullog.sethaul = 'SET' and l_sethaullog.stend_id = 83
	left join ref.vessel_instances vessel_instance on trip.vessel_id = vessel_instance.vessel_id
		and trip.dep_date between vessel_instance.start_date and vessel_instance.calculated_end_date
	left join catch on l_set.l_set_id = catch.l_set_id
", piv$select, outer_catch_sql(piv)))

# ---- EC longline, raised ------------------------------------------------------
# One row per l_best_id. The previous query grouped by flag/fleet/yy/mm/cell/hhooks,
# which merged records sharing those values: their catch was summed while their
# effort (hhooks, a grouping key) was counted once. Aggregate in R if needed.
# Catch in numbers and mt; effort in hundred hooks.

piv <- pivot_species_sql("c.sp_code", "c.sp_n", "c.sp_mt", "mt", species)
run_query("EC_LL_rai", "OFP_DBS", "FISH_MASTER", key = "l_best_id", sql = sprintf("
with catch as (
	select
		c.l_best_id,
		%s
	from best.l_best_agg_catch c
	group by c.l_best_id
)
select
	b.*,
	fleet.fleet_desc,
	%s
from best.l_best_agg b
	left join ref.fleet fleet on b.flag_code = fleet.flag_code
		and coalesce(b.fleet_code, '') = coalesce(fleet.subfleet_code, '')
		and fleet.gear_code = 'L'
	left join catch on b.l_best_id = catch.l_best_id
", piv$select, outer_catch_sql(piv)))

# ---- LF operational (Tufman2): port sampling, LL observer, PS observer ----------
# One row per measured fish. Every species (sp_code) and gear.
# set_id: l_set_id (longline observer) or s_set_id (purse seine observer), NULL
# for port sampling. Cast to text so the three branches of the union share one
# type whatever the id types (port has none). Use set_id with data_source.

run_query("LF_tufman", "OFP_DBS", "tufman2", sql = "
select
	length_data.id,
	length_data.set_id,
	length_data.data_source,
	vessel_instance.flag_id,
	vessel_instance.vesselname,
	vessel.gear,
	length_data.sp_code,
	length_data.len,
	length_data.len_code,
	length_data.measure_date,
	length_data.lat_from,
	length_data.lon_from,
	length_data.lat_to,
	length_data.lon_to,
	length_data.all_tuna_sampled,
	length_data.all_tuna_unloaded
from (
	-- Port sampling
	select
		'port sampling' as data_source,
		sample.sample_id as id,
		cast(null as varchar(36)) as set_id,
		sample.vessel_id,
		coalesce(sample.sample_date, sample.return_date) as measure_date,
		sample_catch.sp_code,
		sample_catch.len,
		sample_catch.len_code,
		nullif(sample.lat_from, '') as lat_from,
		nullif(sample.lon_from, '') as lon_from,
		nullif(sample.lat_to, '') as lat_to,
		nullif(sample.lon_to, '') as lon_to,
		sample.all_tun_sampled as all_tuna_sampled,
		case json_value(sample.extra_info_json, '$.ExtraInfo.AllTunaUnloaded')
			when 'Y' then 1
			when 'N' then 0
			else null
		end as all_tuna_unloaded
	from port.samples sample
		join port.sample_catch sample_catch on sample.sample_id = sample_catch.sample_id

	union all

	-- Longline observer
	select
		'longline observer',
		trip.obstrip_id,
		cast(l_set.l_set_id as varchar(36)),
		trip.vessel_id,
		l_set.set_date,
		l_setcatch.sp_code,
		l_setcatch.len,
		l_setcatch.len_code,
		l_sethaullog.lat,
		l_sethaullog.lon,
		null, null, null, null
	from obsv.trip trip
		join obsv.l_set l_set on trip.obstrip_id = l_set.obstrip_id
		join obsv.l_sethaullog l_sethaullog on l_set.l_set_id = l_sethaullog.l_set_id
			and l_sethaullog.stend_id = 1
		join obsv.l_setcatch l_setcatch on l_set.l_set_id = l_setcatch.l_set_id

	union all

	-- Purse seine observer
	select
		'purse seine observer',
		trip.obstrip_id,
		cast(s_set.s_set_id as varchar(36)),
		trip.vessel_id,
		s_set.vessellog_date,
		s_lfmeas.sp_code,
		s_lfmeas.len,
		s_lf.len_code,
		s_daylog.lat,
		s_daylog.lon,
		null, null, null, null
	from obsv.trip trip
		join obsv.s_day s_day on trip.obstrip_id = s_day.obstrip_id
		join obsv.s_daylog s_daylog on s_day.s_day_id = s_daylog.s_day_id
		join obsv.s_set s_set on s_daylog.s_daylog_id = s_set.s_daylog_id
		join obsv.s_lf s_lf on s_set.s_set_id = s_lf.s_set_id
		join obsv.s_lfmeas s_lfmeas on s_lf.s_lf_id = s_lfmeas.s_lf_id
) length_data
	left join ref.vessel_instances vessel_instance on length_data.vessel_id = vessel_instance.vessel_id
		and length_data.measure_date between vessel_instance.start_date and vessel_instance.calculated_end_date
	left join ref.vessels vessel on vessel_instance.vessel_id = vessel.vessel_id
")

# ---- LF aggregated (lf.LF_MASTER) ---------------------------------------------

lf_where <- c("1 = 1",
	if (!is.null(lf_agg_years))   sprintf("yr between %d and %d", as.integer(lf_agg_years[1]), as.integer(lf_agg_years[2])),
	if (!is.null(lf_agg_gears))   sprintf("gr in %s", sql_in(lf_agg_gears)),
	if (!is.null(lf_agg_species)) sprintf("sp_id in %s", sql_in(lf_agg_species)),
	if (!is.null(lf_agg_wcpfc))   sprintf("ref.inwcpfc_1x1(ref.getlatdfromlat(lat_short), ref.getlondfromlon(lon_short)) = %d",
										  as.integer(lf_agg_wcpfc)))
run_query("LF_agg", "NOUFAMESQL04", "FISH_MASTER", sql = paste0(
	"select *\nfrom FISH_MASTER.lf.LF_MASTER\nwhere ", paste(lf_where, collapse = "\n\tand ")))
