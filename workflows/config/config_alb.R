# ==============================================================================
# Albacore (ALB) configuration, sourced by workflows/alb/*.qmd
# Parameters only: libraries are loaded in the qmd files.
# ==============================================================================

species     <- "ALB"                       # species code as in the SQL tables
species_all <- c("yft", "skj", "bet", "alb", "oth")  # catch columns from sql_queries.R

# ---- Data ---------------------------------------------------------------------
request_tag <- "2026-11-24"                # folder written by workflows/sql_queries.R
sql_folder  <- here::here("data-raw", "sql", request_tag)
save_folder <- here::here("data-raw", "processed", tolower(species))
save_it     <- TRUE                        # save intermediate objects to RDS
verbose     <- FALSE                       # TRUE prints each cleaning step's console
                                           # output in the rendered document

# ---- Spatial ------------------------------------------------------------------
# Stock domain: the SEAPODYM model domain of the stock, as cell EDGES (parfile
# latitudeMin/Max, longitudeMin/Max). Records whose cell centre lies outside are
# removed (1 deg operational cells, 5 deg raised cells, length samples); land
# cells are still removed with the package 1 deg Pacific mask.
# South Pacific albacore (SEAPODYM SP ALB, 2 deg JRA55 grid):
stock_domain <- list(lat = c(-59, 5), lon = c(130, 290))
# North Pacific albacore: replace with the NP parfile domain.

# 1 deg cell-centre bounds of the domain, used by the range filters
lat_range <- c(stock_domain$lat[1] + 0.5, stock_domain$lat[2] - 0.5)
lon_range <- c(stock_domain$lon[1] + 0.5, stock_domain$lon[2] - 0.5)

# ---- Masks --------------------------------------------------------------------
use_temperature_mask <- FALSE              # TRUE removes records in cells colder than min_temp
# TODO: check this is the same mask as the YFT run (masks/temperature_mask_L1.rds)
temperature_mask_file <- system.file("extdata/masks/temp_mask.RDS", package = "seafisheries")
min_temp <- 10                             # degrees C, records in colder cells removed

# ---- Clustering variables -------------------------------------------------------
# Bathymetry regions of the 1 deg cells (columns lat, lon, region) and the
# value given to each region in bat_cat
bathymetry_file   <- system.file("extdata/masks/ocean_region.RDS", package = "seafisheries")
bathymetry_values <- c("1" = 0, "2" = 0.5, "3" = 1)
# HBF categories: (-Inf, 10], (10, 19], (19, 29], (29, Inf] -> 0, 1/3, 2/3, 1
hbf_cat_breaks    <- c(10, 19, 29)

# ---- Longline -----------------------------------------------------------------
hbf_threshold_cap <- 50                    # captain logbooks, max HBF = 49 in YFT run
hbf_threshold_obs <- 55                    # observers, see plot_hbf_histogram()
trip_gap_days     <- 3                     # days without sets that start a new trip

# ---- Raised data ----------------------------------------------------------------
# Missing weights of raised records reported in numbers: number x mean weight
# per fish of the species, from raised records with both, finest tier first;
# a tier is used when its reference has at least rai_w_min_fish fish.
rai_w_tiers    <- list(c("flag", "yr", "latCent", "lonCent"), c("flag", "yr"), "yr")
rai_w_min_fish <- 30

# ---- Length frequency ---------------------------------------------------------
# Conversion of other length codes to UF (upper jaw to caudal fork):
# UF = a * len^b. Codes not listed here and not UF are dropped.
# No SPC length-length factor for albacore (Project 90 reports and size data
# review, SC16-SC21): SD/US/PS albacore lengths (~0.3% of fish) are dropped.
# The YFT factors (Macdonald et al. 2020, SC16/ST-IP-06, Table A2) were:
#   SD = c(a = 3.951, b = 0.8369), US = as SD, PS = c(a = 11.385, b = 0.6619)
len_conversion <- list()
# Plausible albacore lengths (cm UF): outside, lengths are removed as errors.
# 2026 download: 7,368 operational lengths > 140 cm (7,255 port sampling),
# 219 in 130-140 cm, 473 < 30 cm (including zeros); 0.1% quantile 59 cm.
# 140 = FishBase maximum. TODO: check the lower bound on the distribution below 60 cm.
lf_len_range      <- c(30, 140)
lf_min_bins       <- 3                     # min distinct length bins per aggregated sample

# ---- Fisheries (LL_fisheries.qmd) -----------------------------------------------
# longlineTypology output, copied from the clustering run (gitignored)
cluster_file   <- here::here("data-raw", "clusters", "sp_hbf_latabs_pc4", "cluster_diagnostics.RDS")
k_opr          <- 7
merge_clusters <- NULL                     # e.g. list("3" = c(3, 5)) merges 5 into 3 (saved numbering)
fisheries_tag  <- "sp_hbf_latabs_pc4_k7"   # output folder: <save_folder>/LL_fisheries/<tag>

# Assignment of records without every clustering variable (no valid HBF, raised)
assign_method      <- "lda"                # "lda" or "centroid"
assign_prior       <- "tiered"             # "tiered", "global" or "equal"
assign_prior_tiers <- list(c("flag", "year"), "flag")
assign_prior_min_n <- 30
assign_fit_years   <- NULL                 # NULL = fit years of the clustering

# Delta (raised - operational)
assign_delta_group    <- c("flag", "year") # reconciliation: operational + delta = raised per group
assign_delta_coverage <- 0.2               # min logbook share of a cell's raised effort to split
                                           # its delta like the logbook fisheries
thr_E <- 1                                 # delta cells with effort <= thr_E set aside as dummy

# Length frequency outputs: lengths outside are capped to the bounds, and the
# length bins of the SEAPODYM files will be min_len:max_len.
# max_len: FishBase maximum for albacore (140 cm FL). min_len: no albacore
# below 20 cm in longline data. Check both against the length classes of the
# SEAPODYM albacore configuration.
min_len <- 20
max_len <- 140

# Figures of LL_fisheries.qmd (CPUE maps and time series): years shown.
# Does not filter the data or the outputs.
plot_years <- c(1980, 2022)

# Hampel filter
hampel_EC <- list(year_window = 5, radius_km = 250, k = 7)
hampel_LF <- list(month_window = 6, k = 7)

# ---- Other gears (other.qmd) ------------------------------------------------------
# Gears other than longline (L), purse seine (S) and pole-and-line (P).
oth_gears     <- NULL   # gear codes kept, e.g. c("T", "G"); NULL = all of them
oth_fisheries <- NULL   # NULL = one fishery per gear; or e.g. list(troll = "T", net = c("G", "D"))

# ---- SEAPODYM formatting (format_seapodym.qmd) -------------------------------------
seapodym_version <- "v1"
# SEAPODYM grid: domain = stock_domain (cell edges, as latitudeMin/Max and
# longitudeMin/Max of the parfile), resolution in degrees (parfile deltaX / 60)
seapodym_res       <- 2
# mask of the SEAPODYM configuration (0 = land), one row per latitude, north first
seapodym_mask_file <- "/opt/shared/SEAPODYM/data/forcings/run-jra55npv2_2x30d-sp/mask_jra55np_2deg_sp_cor.txt"
dest_folder_seapodym <- here::here("data-raw", "seapodym", tolower(species), seapodym_version)
seapodym_years <- NULL                   # e.g. c(1980, 2022); NULL = all years
hampel_adjusted_effort <- TRUE           # TRUE: effort adjusted by the Hampel filter
lf_max_fish <- 500                       # LF samples (fishery x year x quarter x region) capped to this
lf_region_res <- NULL                    # LF regions smaller than this (deg) are expanded to the
                                         # cell containing them; NULL = model resolution
bin_len     <- 1
length_bins <- as.character(seq(min_len, max_len, by = bin_len))

# Gear outputs, in fishery order. Missing files are skipped (with a message).
# Fishery labels: gear code + number, numbered across gears in this order.
seapodym_inputs <- list(
	S = list(ec = file.path(save_folder, "PS", "PS_EC_hampel.RDS"),
			 lf = file.path(save_folder, "PS", "PS_LF_hampel.RDS")),
	P = list(ec = file.path(save_folder, "PL", "PL_EC_hampel.RDS"),
			 lf = file.path(save_folder, "PL", "PL_LF_hampel.RDS")),
	O = list(ec = file.path(save_folder, "OTH", "OTH_EC_hampel.RDS"),
			 lf = file.path(save_folder, "OTH", "OTH_LF_hampel.RDS")),
	L = list(ec = file.path(save_folder, "LL_fisheries", fisheries_tag,
							c("LL_EC_hampel_opr.RDS", "LL_EC_hampel_raidelta.RDS")),
			 lf = file.path(save_folder, "LL_fisheries", fisheries_tag,
			 			   c("LL_LF_hampel_opr.RDS", "LL_LF_hampel_rai.RDS")))
)
# Catch without effort of every gear -> one dummy fishery (gear D, catch removal only).
# Raised longline records without effort (LL_rai_dummy.RDS) are not included, as in
# the original: their catch may already be in the logbook dummies.
seapodym_dummy <- c(
	LL_captain    = file.path(save_folder, "LL_cap_dummy.RDS"),
	LL_observer   = file.path(save_folder, "LL_obs_dummy.RDS"),
	LL_delta      = file.path(save_folder, "LL_fisheries", fisheries_tag, "LL_EC_raidelta_dummy.RDS"),
	LL_unassigned = file.path(save_folder, "LL_fisheries", fisheries_tag, "LL_EC_raidelta_unassigned.RDS"),
	OTH           = file.path(save_folder, "OTH", "OTH_EC_dummy.RDS")
)

