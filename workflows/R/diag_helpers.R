# ==============================================================================
# Diagnostics helpers shared by the workflow documents.
# Sourced at the top of each qmd: creates a fresh .diag environment.
# The document saves list(steps = stepsTable(), messages = .diag$messages,
# warnings = .diag$warnings, tables = .diag$tables, ...) to an RDS that
# seafisheries::processing_report() turns into a PDF.
# ==============================================================================

# ---- Diagnostics -------------------------------------------------------------
# Everything is collected in .diag, saved by the document to an RDS read by
# seafisheries::processing_report():
#   steps     one row per cleaning step: dataset, step, rows in, rows out
#   messages  console output of each step (what the package functions print)
#   warnings  warnings raised by the steps and by the checks of this document
#   tables    stage comparisons, coordinate conversion summaries, catch quality,
#             length matching coverage, ...
# Dataset names are keys: tables named stage_<dataset>, catch_quality_<dataset>
# and coords_<dataset>... are shown with their dataset in the report.
.diag <- new.env()
.diag$steps <- list(); .diag$messages <- list(); .diag$warnings <- list(); .diag$tables <- list()

diagAdd <- function(slot, name, value) {
	x <- .diag[[slot]]; x[[name]] <- value; .diag[[slot]] <- x
	invisible(value)
}

# Records a warning in the diagnostics and raises it.
diagWarn <- function(where, ...) {
	msg <- paste0(...)
	.diag$warnings[[where]] <- c(.diag$warnings[[where]], msg)
	warning(msg, call. = FALSE)
}

# Runs fun(df, ...), records rows in/out, the printed output and the warnings.
# x: a data.frame, or a list with $data. Returns what fun returns.
# Re-running a step (same dataset and step) replaces its previous record.
runStep <- function(x, dataset, step, fun, ...) {
	df_in <- if (is.data.frame(x)) x else x$data
	key <- paste(dataset, step, sep = " | ")
	out <- NULL
	warns <- character(0)
	msg <- withCallingHandlers(
		utils::capture.output(out <- fun(df_in, ...)),
		warning = function(w) warns <<- c(warns, conditionMessage(w)))
	df_out <- if (is.data.frame(out)) out else out$data
	.diag$steps[[key]] <- data.frame(dataset = dataset, step = step,
									 n_in = nrow(df_in), n_out = nrow(df_out))
	.diag$warnings[[key]] <- if (length(warns)) warns else NULL
	.diag$messages[[key]] <- if (length(msg)) msg else NULL
	if (verbose && length(msg)) cat(paste0("[", key, "]"), msg, sep = "\n")
	out
}

stepsTable <- function(ds = NULL) {
	if (!length(.diag$steps))
		return(data.frame(dataset = character(0), step = character(0), n_in = integer(0),
						  n_out = integer(0), removed = integer(0), pct_removed = numeric(0)))
	st <- do.call(rbind, unname(.diag$steps))
	if (!is.null(ds)) st <- st[st$dataset == ds, ]
	st$removed <- st$n_in - st$n_out
	st$pct_removed <- round(100 * st$removed / st$n_in, 2)
	st
}

showSteps <- function(ds) {
	st <- stepsTable(ds)
	knitr::kable(st[, -1], row.names = FALSE, caption = paste("Cleaning steps:", ds))
}

# ==============================================================================
# Cleaning and input-vs-processed helpers shared by the gear workflows
# ==============================================================================

# Exact duplicates ignoring the record id (an id is unique by construction,
# so remove_duplicates() on a table with its id never removes anything).
removeContentDuplicates <- function(df, id_col) {
	n0 <- nrow(df)
	out <- dplyr::distinct(df, dplyr::across(-dplyr::all_of(id_col)), .keep_all = TRUE)
	report_removal(n0 - nrow(out), n0, paste0("exact duplicates ignoring ", id_col))
	out
}

# ---- Input vs processed tables -----------------------------------------------
# "Input" value/% relative to the RAW record count. "Processed" % = processed
# records (or ids) with a valid value / raw records (or ids) * 100.
pctRetained <- function(raw_valid_count, raw_total) {
	sprintf("%.2f%%", 100 * raw_valid_count / raw_total)
}

safeCol <- function(df, col) if (!is.null(col) && col %in% names(df)) df[[col]] else NULL

fmtCoord <- function(lat, lon) {
	if (is.null(lat) || is.null(lon)) return(NA_character_)
	lat_num <- suppressWarnings(as.numeric(lat))
	lon_num <- suppressWarnings(as.numeric(lon))
	if (all(is.na(lat_num)) || all(is.na(lon_num))) {
		return(sprintf("%s-%s ; %s,%s", min(lon, na.rm = TRUE), max(lon, na.rm = TRUE),
					   max(lat, na.rm = TRUE), min(lat, na.rm = TRUE)))
	}
	sprintf("%.1f-%.1fE ; %.1fN,%.1fS", min(lon_num, na.rm = TRUE), max(lon_num, na.rm = TRUE),
			max(lat_num, na.rm = TRUE), min(lat_num, na.rm = TRUE))
}

ecStageCompare <- function(raw_df, proc_df,
						   catch_n_col = sp_n, catch_w_col = sp_w,
						   effort_col = "E", hbf_col = "hbf",
						   flag_col = "flag", vessel_col = "vesselname", id_label = "Vessels",
						   date_col_raw = "ymd", date_col_proc = "ymd",
						   lat_col_raw, lon_col_raw, lat_col_proc, lon_col_proc,
						   n_unit_raw = "count", w_unit_raw = "kg", effort_unit_raw = "count",
						   w_unit_proc = "mt", effort_unit_proc = "hundred hooks",
						   coord_unit_raw = "raw (pre-conversion)", coord_unit_proc = "1 deg x 1 deg") {
	n_raw <- nrow(raw_df)
	fields <- "Total"; input_vals <- as.character(n_raw); input_units <- "records"
	proc_vals <- pctRetained(nrow(proc_df), n_raw); proc_units <- "-"

	add_row <- function(field, raw_x, proc_x, in_unit, out_unit, count_type = TRUE) {
		fields <<- c(fields, field)
		if (count_type) {
			input_vals <<- c(input_vals, pctRetained(sum(!is.na(raw_x)), n_raw))
			proc_vals  <<- c(proc_vals,  pctRetained(sum(!is.na(proc_x)), n_raw))
		} else {
			input_vals <<- c(input_vals, as.character(dplyr::n_distinct(raw_x, na.rm = TRUE)))
			proc_vals  <<- c(proc_vals,  pctRetained(dplyr::n_distinct(proc_x, na.rm = TRUE),
													 dplyr::n_distinct(raw_x, na.rm = TRUE)))
		}
		input_units <<- c(input_units, in_unit); proc_units <<- c(proc_units, out_unit)
	}

	if (!is.null(catch_n_col))
		add_row(paste0("Catch ", species, " (n)"), safeCol(raw_df, catch_n_col), safeCol(proc_df, catch_n_col), n_unit_raw, n_unit_raw)
	if (!is.null(catch_w_col))
		add_row(paste0("Catch ", species, " (w)"), safeCol(raw_df, catch_w_col), safeCol(proc_df, catch_w_col), w_unit_raw, w_unit_proc)
	add_row("Effort", safeCol(raw_df, effort_col), safeCol(proc_df, effort_col), effort_unit_raw, effort_unit_proc)
	if (!is.null(hbf_col) && hbf_col %in% names(raw_df))
		add_row("Hooks Between Floats", safeCol(raw_df, hbf_col), safeCol(proc_df, hbf_col), "count", "count")
	add_row("Countries", safeCol(raw_df, flag_col), safeCol(proc_df, flag_col), "flag", "flag", count_type = FALSE)
	if (!is.null(vessel_col))
		add_row(id_label, safeCol(raw_df, vessel_col), safeCol(proc_df, vessel_col), "id", "id", count_type = FALSE)

	date_raw <- safeCol(raw_df, date_col_raw); date_proc <- safeCol(proc_df, date_col_proc)
	fields <- c(fields, "Date Range")
	input_vals <- c(input_vals, if (!is.null(date_raw)) paste(format(range(date_raw, na.rm = TRUE)), collapse = " - ") else NA_character_)
	proc_vals  <- c(proc_vals,  if (!is.null(date_proc)) paste(format(range(date_proc, na.rm = TRUE)), collapse = " - ") else NA_character_)
	input_units <- c(input_units, "yyyy-mm-dd"); proc_units <- c(proc_units, "yyyy-mm-dd")

	fields <- c(fields, "Coordinates")
	input_vals <- c(input_vals, fmtCoord(safeCol(raw_df, lat_col_raw), safeCol(raw_df, lon_col_raw)))
	proc_vals  <- c(proc_vals,  fmtCoord(safeCol(proc_df, lat_col_proc), safeCol(proc_df, lon_col_proc)))
	input_units <- c(input_units, coord_unit_raw); proc_units <- c(proc_units, coord_unit_proc)

	tibble::tibble(Field = fields,
				   `Input value/range` = input_vals, `Input unit/format` = input_units,
				   `Processed value/range` = proc_vals, `Processed unit/format` = proc_units)
}

# LF variant. sample_group_cols: the columns defining one sample, applied to
# both raw and processed frames.
lfStageCompare <- function(raw_df, proc_df, sample_group_cols,
						   flag_col = "flag", fleet_col = NULL, id_label = "Vessels",
						   date_col_raw, date_col_proc, date_fmt = "yyyy-mm-dd",
						   lat_col_raw, lon_col_raw, lat_col_proc, lon_col_proc,
						   coord_unit_raw = "raw (pre-conversion)", coord_unit_proc = "1 deg x 1 deg") {
	n_samples <- function(df) {
		cols <- intersect(sample_group_cols, names(df))
		if (length(cols) == 0) return(NA_integer_)
		nrow(dplyr::distinct(df, dplyr::across(dplyr::all_of(cols))))
	}
	n_raw_samples <- n_samples(raw_df); n_proc_samples <- n_samples(proc_df)

	fields <- "Samples"; input_vals <- as.character(n_raw_samples)
	proc_vals <- if (!is.na(n_raw_samples) && n_raw_samples > 0) pctRetained(n_proc_samples, n_raw_samples) else NA_character_
	input_units <- "-"; proc_units <- "-"

	fields <- c(fields, "Countries")
	input_vals <- c(input_vals, as.character(dplyr::n_distinct(safeCol(raw_df, flag_col), na.rm = TRUE)))
	proc_vals  <- c(proc_vals,  pctRetained(dplyr::n_distinct(safeCol(proc_df, flag_col), na.rm = TRUE),
											dplyr::n_distinct(safeCol(raw_df, flag_col), na.rm = TRUE)))
	input_units <- c(input_units, "flag"); proc_units <- c(proc_units, "flag")

	if (!is.null(fleet_col)) {
		fields <- c(fields, id_label)
		raw_fleet <- safeCol(raw_df, fleet_col); proc_fleet <- safeCol(proc_df, fleet_col)
		input_vals <- c(input_vals, if (!is.null(raw_fleet)) as.character(dplyr::n_distinct(raw_fleet, na.rm = TRUE)) else NA_character_)
		proc_vals  <- c(proc_vals,  if (!is.null(proc_fleet) && !is.null(raw_fleet))
			pctRetained(dplyr::n_distinct(proc_fleet, na.rm = TRUE), dplyr::n_distinct(raw_fleet, na.rm = TRUE))
			else "NA (column dropped by this stage)")
		input_units <- c(input_units, "id"); proc_units <- c(proc_units, "id")
	}

	fmt_date <- function(x) if (is.null(x)) NA_character_ else if (date_fmt == "yyyy-mm-dd")
		paste(format(range(x, na.rm = TRUE)), collapse = " - ") else paste(range(x, na.rm = TRUE), collapse = " - ")
	fields <- c(fields, "Date Range")
	input_vals <- c(input_vals, fmt_date(safeCol(raw_df, date_col_raw)))
	proc_vals  <- c(proc_vals,  fmt_date(safeCol(proc_df, date_col_proc)))
	input_units <- c(input_units, date_fmt); proc_units <- c(proc_units, date_fmt)

	fields <- c(fields, "Coordinates")
	input_vals <- c(input_vals, fmtCoord(safeCol(raw_df, lat_col_raw), safeCol(raw_df, lon_col_raw)))
	proc_vals  <- c(proc_vals,  fmtCoord(safeCol(proc_df, lat_col_proc), safeCol(proc_df, lon_col_proc)))
	input_units <- c(input_units, coord_unit_raw); proc_units <- c(proc_units, coord_unit_proc)

	tibble::tibble(Field = fields,
				   `Input value/range` = input_vals, `Input unit/format` = input_units,
				   `Processed value/range` = proc_vals, `Processed unit/format` = proc_units)
}


# Table for the PDF reports: LaTeX tabular at the natural width of its columns
# (markdown tables wrap the headers of wide tables). Underscores in the column
# names become spaces, numbers get thousands separators except in year-like
# and id columns, tables stay where they are written (float H, needs the
# float package), long tables break across pages, wide ones are set smaller.
ktab <- function(x, caption = NULL, digits = NULL, small = NULL) {
	x <- as.data.frame(x, check.names = FALSE)
	keep_plain <- grepl("^(f|yr|year|decade|cluster|fishery|month|mm|qtr|k-means)$", names(x))
	for (j in which(keep_plain)) x[[j]] <- as.character(x[[j]])
	if (!is.null(digits)) for (j in which(sapply(x, is.numeric))) x[[j]] <- round(x[[j]], digits)
	names(x) <- gsub("_", " ", names(x))
	if (is.null(small)) small <- ncol(x) > 6
	# captions: no character that needs escaping (Quarto escapes them again)
	if (!is.null(caption)) caption <- gsub("&", "and", gsub("%", "percent", gsub("_", " ", caption)))
	op <- options(knitr.kable.NA = ""); on.exit(options(op))
	tab <- knitr::kable(x, format = "latex", booktabs = TRUE, longtable = nrow(x) > 25,
						caption = caption, label = NA, row.names = FALSE, linesep = "",
						format.args = list(big.mark = ",", scientific = FALSE),
						position = if (nrow(x) > 25) "" else "H")
	tab <- as.character(tab)
	if (small) tab <- paste0("\\begingroup\\small\n", tab, "\n\\endgroup")
	# raw LaTeX block: passed to LaTeX untouched by pandoc
	knitr::asis_output(paste0("\n```{=latex}\n", tab, "\n```\n"))
}
