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
lat_range <- c(-54.5, 64.5)
lon_range <- c(89.5, 288.5)

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

# ---- Length frequency ---------------------------------------------------------
# Conversion of other length codes to UF (upper jaw to caudal fork):
# UF = a * len^b. Codes not listed here and not UF are dropped.
# TODO: YFT coefficients (Project 90 update, as in standardize_length_code()),
# replace with ALB values.
len_conversion <- list(
	SD = c(a = 3.951,  b = 0.8369),
	US = c(a = 3.951,  b = 0.8369),       # US treated as SD
	PS = c(a = 11.385, b = 0.6619)
)
lf_min_bins       <- 3                     # min distinct length bins per aggregated sample

# ---- Fisheries (LL_fisheries.qmd) -----------------------------------------------
# longlineTypology output, copied from the clustering run (gitignored)
cluster_file   <- here::here("data-raw", "clusters", "sp_hbf_latabs_pc4", "cluster_diagnostics.RDS")
k_opr          <- 7
merge_clusters <- list("3" = c(3, 5))      # NULL for none; saved numbering
fisheries_tag  <- "sp_hbf_latabs_pc4_k7"   # output folder: <save_folder>/LL_fisheries/<tag>

# Assignment of records without every clustering variable (no valid HBF, raised)
assign_method      <- "lda"                # "lda" or "centroid"
assign_prior       <- "tiered"             # "tiered", "global" or "equal"
assign_prior_tiers <- list(c("flag", "year"), "flag")
assign_prior_min_n <- 30
assign_fit_years   <- NULL                 # NULL = fit years of the clustering

# Delta (raised - operational)
thr_E <- 1                                 # delta records with effort <= thr_E removed

# Length frequency outputs
# TODO: YFT values, set ALB bounds
min_len <- 20
max_len <- 250

# Hampel filter
hampel_EC <- list(year_window = 5, radius_km = 250, k = 7)
hampel_LF <- list(month_window = 6, k = 7)

