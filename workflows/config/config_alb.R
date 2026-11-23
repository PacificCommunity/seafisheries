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
