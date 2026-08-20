#' @import dplyr
#' @importFrom rlang .data := !!
#' @importFrom lubridate month year make_date
#' @importFrom purrr pmap
#' @importFrom tidyr unnest_wider drop_na
NULL

#' Convert Cardinal Coordinates to Decimal Degrees (0-360 longitude range)
#'
#' @description
#' Converts coordinates with cardinal directions (N, S, E, W) to decimal degrees,
#' primarily designed for fisheries databases. For longitudes, uses a 0-360 degree
#' range instead of the -180/180 convention to facilitate calculations.
#'
#' @details
#' The function primarily handles coordinates in DDDMM.MMM format where:
#' \itemize{
#'   \item DDD: degrees
#'   \item MM.MMM: decimal minutes
#' }
#'
#' The conversion process:
#' \enumerate{
#'   \item Validates the input format
#'   \item Separates degrees and minutes
#'   \item Converts to decimal degrees
#'   \item Applies the appropriate sign/transformation based on cardinal direction:
#'   \itemize{
#'     \item N: positive
#'     \item S: negative
#'     \item E: as is
#'     \item W: converted to 0-360 range
#'   }
#' }
#'
#' The function detects and categorizes various failure modes:
#' \itemize{
#'   \item \code{empty_or_null}: NA, NULL, "", or "NULL" input
#'   \item \code{cardinal_only}: Input is just a cardinal direction (e.g., "N")
#'   \item \code{unknown_cardinal}: Ends with letter that's not N, S, E, W
#'   \item \code{multiple_decimals}: More than one decimal point in the string
#'   \item \code{invalid_format}: Contains invalid characters or malformed
#'   \item \code{insufficient_digits}: Not enough digits for DDDMM format
#'   \item \code{minutes_out_of_range}: Minutes >= 60
#'   \item \code{wrong_cardinal_for_type}: E/W for latitude or N/S for longitude
#'   \item \code{latitude_out_of_range}: Result < -90 or > 90
#'   \item \code{longitude_out_of_range}: Result < 0 or > 360
#' }
#'
#' @param coord Character string containing the coordinate (e.g., "12025W", "4530N")
#' @param warnings Logical; if TRUE, prints warnings when conversion fails.
#'   Default is TRUE.
#' @param cardinal Character; one of "latitude" or "longitude". Controls coordinate
#'   validation based on expected cardinal directions and value ranges.
#'   Default is "latitude".
#' @param format Character; one of "auto", "DDDMM", "DDD". Controls how the
#'   coordinate string is parsed. "auto" infers format from string length.
#'   Default is "auto".
#' @param .return_reason Logical; internal parameter used by
#'   \code{\link{convert_coordinates_df}}. If TRUE, returns a list with both the
#'   converted value and the failure reason (if any). Default is FALSE.
#'
#' @return If \code{.return_reason = FALSE} (default): Numeric value in decimal
#'   degrees. For longitude, returns values in 0-360 range. Returns NA if
#'   conversion fails.
#'
#'   If \code{.return_reason = TRUE}: A list with two elements:
#'   \itemize{
#'     \item \code{value}: Numeric value or NA
#'     \item \code{reason}: Character failure reason or NA if successful
#'   }
#'
#' @examples
#' # Latitude examples
#' convert_coordinate("4530N")
#' convert_coordinate("4530.5S")
#'
#' # Longitude examples (note conversion to 0-360 range)
#' convert_coordinate("12025W", cardinal = "longitude")
#' convert_coordinate("12025E", cardinal = "longitude")
#'
#' # With cardinal validation
#' convert_coordinate("4530N", cardinal = "latitude")
#' convert_coordinate("4530E", cardinal = "latitude")
#'
#' # Invalid coordinates return NA
#' convert_coordinate("0865.000S")
#' convert_coordinate(".4.1.000S")
#'
#' # Get failure reason (used internally by convert_coordinates_df)
#' convert_coordinate("0865.000S", .return_reason = TRUE)
#'
#' @seealso \code{\link{convert_coordinates_df}} for batch conversion of
#'   dataframe columns with failure reporting.
#'
#' @family coordinate utilities
#' @export
convert_coordinate <- function(coord, warnings = TRUE, cardinal = "latitude", format = "auto", .return_reason = FALSE) {

	# Helper to return result
	return_result <- function(value, reason = NA_character_) {
		if (.return_reason) {
			return(list(value = value, reason = reason))
		} else {
			return(value)
		}
	}

	# Check args
	if(is.na(coord) || is.null(coord) || coord == "NULL" || coord == "") {
		return(return_result(NA, "empty_or_null"))
	}
	cardinal <- match.arg(cardinal, c("latitude", "longitude"))
	format <- match.arg(format, c("auto", "DDDMM", "DDD"))

	# Handle case where input is just a single cardinal direction
	if(nchar(gsub("\\s+", "", coord)) == 1 && grepl("^[NSEWnsew]$", coord)) {
		if(warnings) warning("Coordinate contains only a cardinal direction: ", coord)
		return(return_result(NA, "cardinal_only"))
	}

	# Clean the input
	coord_clean <- gsub("\\s+$", "", coord)

	# Extract cardinal direction if present
	if(grepl("[a-zA-Z]$", coord_clean)) {
		last_char <- substr(coord_clean, nchar(coord_clean), nchar(coord_clean))
		if(toupper(last_char) %in% c("N", "S", "E", "W")) {
			direction <- toupper(last_char)
			coord_clean <- substr(coord_clean, 1, nchar(coord_clean) - 1)
		} else {
			if(warnings) warning("Unknown cardinal direction: ", last_char, " in coordinate: ", coord)
			return(return_result(NA, "unknown_cardinal"))
		}
	} else {
		direction <- NULL
	}

	# Handle coordinates without cardinal direction
	is_negative <- grepl("^-", coord_clean)
	if(is.null(direction)) {
		if(cardinal == "latitude") {
			direction <- if(is_negative) "S" else "N"
			if(is_negative) {
				coord_clean <- sub("^-", "", coord_clean)
			}
		} else {
			direction <- NULL
		}
	}

	# Handle potential numeric input
	if(is.numeric(coord_clean)) {
		coord_clean <- as.character(coord_clean)
	}

	# Check for multiple decimal points
	decimal_count <- nchar(coord_clean) - nchar(gsub("\\.", "", coord_clean))
	if(decimal_count > 1) {
		if(warnings) warning("Multiple decimal points in coordinate: ", coord)
		return(return_result(NA, "multiple_decimals"))
	}

	# Ensure coord_clean contains only digits and possibly a decimal point
	if(!grepl("^[0-9]+(\\.[0-9]+)?$", coord_clean)) {
		if(warnings) warning("Invalid coordinate format: ", coord)
		return(return_result(NA, "invalid_format"))
	}

	# Split into integral and decimal parts
	parts <- strsplit(coord_clean, "\\.")[[1]]
	integral_part <- parts[1]
	decimal_part <- if(length(parts) > 1) parts[2] else NULL

	# Determine format if auto
	integral_length <- nchar(integral_part)
	if(format == "auto") {
		if((cardinal == "latitude" && integral_length > 2) ||
		   (cardinal == "longitude" && integral_length > 3)) {
			format <- "DDDMM"
		} else {
			format <- "DDD"
		}
	}

	# Process based on format
	if(format == "DDDMM") {
		integral_length <- nchar(integral_part)
		minute_digits <- 2
		if(integral_length <= minute_digits) {
			if(warnings) warning("Not enough digits for DDDMM format: ", coord)
			return(return_result(NA, "insufficient_digits"))
		}

		minutes <- as.numeric(substr(integral_part, integral_length - minute_digits + 1, integral_length))
		degrees <- as.numeric(substr(integral_part, 1, integral_length - minute_digits))

		if(!is.null(decimal_part)) {
			decimal_minutes <- as.numeric(paste0("0.", decimal_part))
			minutes <- minutes + decimal_minutes
		}

		if(minutes >= 60) {
			if(warnings) warning("Minutes must be less than 60: ", minutes, " in coordinate: ", coord)
			return(return_result(NA, "minutes_out_of_range"))
		}

		decimal_degrees <- degrees + minutes / 60
	} else {
		degrees <- as.numeric(integral_part)
		if(!is.null(decimal_part)) {
			decimal_part_numeric <- as.numeric(paste0("0.", decimal_part))
			decimal_degrees <- degrees + decimal_part_numeric
		} else {
			decimal_degrees <- degrees
		}
	}

	# Apply direction if present
	if(!is.null(direction)) {
		if(cardinal == "latitude" && !(direction %in% c("N", "S"))) {
			if(warnings) warning("Invalid direction for latitude: ", direction)
			return(return_result(NA, "wrong_cardinal_for_type"))
		}
		if(cardinal == "longitude" && !(direction %in% c("E", "W"))) {
			if(warnings) warning("Invalid direction for longitude: ", direction)
			return(return_result(NA, "wrong_cardinal_for_type"))
		}

		if(direction == "S") {
			decimal_degrees <- -decimal_degrees
		} else if(direction == "W") {
			decimal_degrees <- 360 - decimal_degrees
		}
	}

	# Validate result
	if(cardinal == "latitude" && (decimal_degrees < -90 || decimal_degrees > 90)) {
		if(warnings) warning("Latitude out of range: ", decimal_degrees, " from coordinate: ", coord)
		return(return_result(NA, "latitude_out_of_range"))
	}

	if(cardinal == "longitude" && (decimal_degrees < 0 || decimal_degrees > 360)) {
		if(warnings) warning("Longitude out of range: ", decimal_degrees, " from coordinate: ", coord)
		return(return_result(NA, "longitude_out_of_range"))
	}

	return(return_result(decimal_degrees, NA_character_))
}


#' Convert Coordinate Columns in a Dataframe
#'
#' @description
#' Batch converts latitude and longitude columns in a dataframe from various
#' string formats to decimal degrees. Handles multiple input formats and provides
#' optional detailed reporting of conversion failures.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Extracts unique coordinate values to minimize redundant conversions
#'   \item Converts each unique value using \code{\link{convert_coordinate}}
#'   \item Joins converted values back to the original dataframe
#'   \item Optionally compiles a summary of all conversion failures
#' }
#'
#' This is more efficient than row-wise conversion when the dataframe contains

#' repeated coordinate values, which is common in fisheries data aggregated by
#' location.
#'
#' @param df Dataframe containing coordinate columns to convert.
#' @param lat_col Character; name of the latitude column. Default is "latitude".
#' @param lon_col Character; name of the longitude column. Default is "longitude".
#' @param report Logical; if TRUE, returns a list containing the converted
#'   dataframe plus failure summaries. If FALSE, returns only the converted
#'   dataframe. Default is FALSE.
#' @param format Character; one of "auto", "DDDMM", "DDD". Passed to
#'   \code{\link{convert_coordinate}}. Default is "auto".
#'
#' @return If \code{report = FALSE}: The input dataframe with latitude and
#'   longitude columns converted to decimal degrees (numeric). Failed conversions
#'   result in NA values.
#'
#'   If \code{report = TRUE}: A list with three elements:
#'   \describe{
#'     \item{data}{The converted dataframe}
#'     \item{summary}{A dataframe summarizing failures by type and reason:
#'       \itemize{
#'         \item \code{type}: "latitude" or "longitude"
#'         \item \code{reason}: Failure category (see \code{\link{convert_coordinate}})
#'         \item \code{count}: Number of unique coordinates with this failure
#'         \item \code{examples}: Up to 3 example values
#'       }
#'     }
#'     \item{failures}{A dataframe listing all failed conversions:
#'       \itemize{
#'         \item \code{type}: "latitude" or "longitude"
#'         \item \code{original}: The original coordinate string
#'         \item \code{reason}: Failure category
#'       }
#'     }
#'   }
#'
#' @examples
#' # Sample data with various coordinate formats
#' df <- data.frame(
#'   id = 1:5,
#'   latitude = c("4530N", "0865.000S", "2215.500S", ".4.1.000S", ""),
#'   longitude = c("17025E", "15030.000W", "16045E", "18065.000E", "12000W")
#' )
#'
#' # Simple conversion
#' df_converted <- convert_coordinates_df(df)
#'
#' # With failure report
#' result <- convert_coordinates_df(df, report = TRUE)
#' df_converted <- result$data
#' print(result$summary)
#'
#' # Custom column names
#' df2 <- data.frame(lat = "4530N", lon = "17025E")
#' convert_coordinates_df(df2, lat_col = "lat", lon_col = "lon")
#'
#' @seealso \code{\link{convert_coordinate}} for single value conversion and
#'   details on failure categories.
#' @family coordinate utilities
#' @export
convert_coordinates_df <- function(df, lat_col = "latitude", lon_col = "longitude",
								   report = FALSE, format = "auto") {

	# Get unique coordinates
	unique_lats <- unique(df[[lat_col]])
	unique_lons <- unique(df[[lon_col]])

	# Convert latitudes
	lat_results <- lapply(unique_lats, function(x) {
		convert_coordinate(x, warnings = FALSE, cardinal = "latitude",
						   format = format, .return_reason = TRUE)
	})

	converted_lats <- data.frame(
		orig_lat = unique_lats,
		lat_converted = sapply(lat_results, function(x) x$value),
		lat_reason = sapply(lat_results, function(x) x$reason),
		stringsAsFactors = FALSE
	)

	# Convert longitudes
	lon_results <- lapply(unique_lons, function(x) {
		convert_coordinate(x, warnings = FALSE, cardinal = "longitude",
						   format = format, .return_reason = TRUE)
	})

	converted_lons <- data.frame(
		orig_lon = unique_lons,
		lon_converted = sapply(lon_results, function(x) x$value),
		lon_reason = sapply(lon_results, function(x) x$reason),
		stringsAsFactors = FALSE
	)

	# Join back to dataframe
	df_out <- df |>
		left_join(converted_lats, by = stats::setNames("orig_lat", lat_col)) |>
		left_join(converted_lons, by = stats::setNames("orig_lon", lon_col)) |>
		mutate(
			!!lat_col := lat_converted,
			!!lon_col := lon_converted
		) |>
		select(-lat_converted, -lon_converted, -lat_reason, -lon_reason)

	if (!report) {
		return(df_out)
	}

	# Build failure reports
	build_summary <- function(converted_df, reason_col, orig_col, type) {
		failures <- converted_df |>
			filter(!is.na(.data[[reason_col]])) |>
			rename(original = !!orig_col, reason = !!reason_col)

		if (nrow(failures) == 0) {
			return(list(summary = NULL, failures = NULL))
		}

		summary <- failures |>
			group_by(reason) |>
			summarise(
				count = n(),
				examples = paste(head(original, 3), collapse = ", "),
				.groups = "drop"
			) |>
			mutate(type = type) |>
			select(type, reason, count, examples)

		failures <- failures |>
			mutate(type = type) |>
			select(type, original, reason)

		list(summary = summary, failures = failures)
	}

	lat_report <- build_summary(converted_lats, "lat_reason", "orig_lat", "latitude")
	lon_report <- build_summary(converted_lons, "lon_reason", "orig_lon", "longitude")

	summary_combined <- bind_rows(lat_report$summary, lon_report$summary)
	failures_combined <- bind_rows(lat_report$failures, lon_report$failures)

	return(list(
		data = df_out,
		summary = summary_combined,
		failures = failures_combined
	))
}

#' Check if a value is valid (non-missing, finite)
#'
#' Returns TRUE for values that are usable in analysis: not NA, NaN, Inf, or
#' NULL. For non-numeric input, only checks for NA. Designed for use with
#' [dplyr::if_all()] to filter rows where all required columns are valid.
#'
#' @param x A vector of any type.
#'
#' @return A logical vector of the same length as `x`.
#'
#' @examples
#' is_valid(c(1.5, NA, NaN, Inf, -Inf))   # TRUE FALSE FALSE FALSE FALSE
#' is_valid(c("a", NA, "b"))               # TRUE FALSE TRUE
#'
#' # Typical use: filter rows where all required columns are valid
#' # df %>% filter(if_all(all_of(required_cols), is_valid))
#'
#' @family data quality utilities
#' @export

is_valid <- function(x) {
	if (is.numeric(x)) {
		!is.na(x) & !is.nan(x) & is.finite(x) & !is.null(x)
	} else {
		!is.na(x)
	}
}

#' Report the number of records removed in a cleaning step
#'
#' Prints a formatted message to the console describing how many records were
#' removed and what percentage of the total they represent. Prints nothing if
#' `n_removed` is zero.
#'
#' @param n_removed Integer; number of records removed in this step.
#' @param n_total Integer; total number of records before this cleaning step.
#' @param reason Character; short description of why records were removed,
#'   used in the printed message.
#'
#' @return Invisibly returns NULL. Called for its side effect (console output).
#'
#' @examples
#' report_removal(42, 1000, "missing coordinates")
#' # 42  records removed ( missing coordinates ): 4.2 % of data
#'
#' report_removal(0, 1000, "duplicates")
#' # (prints nothing)
#'
#' @family data quality utilities
#' @export

report_removal <- function(n_removed, n_total, reason) {
	if(n_removed > 0) {
		cat(n_removed, " records removed (", reason, "):",
			round(n_removed / n_total * 100, 2), "% of data\n")
	}
}

#' Round coordinate to nearest 0.5-degree grid cell center
#'
#' Maps a decimal degree coordinate to the center of its containing 1x1 degree
#' SEAPODYM grid cell. Grid cells are centered at X.5 (e.g., 0.5, 1.5, -0.5,
#' -1.5), so any value in \[N, N+1) maps to N.5.
#'
#' @param x Numeric value or vector (latitude or longitude in decimal degrees).
#'
#' @return Numeric vector of the same length as `x`, with each value equal to
#'   `floor(x) + 0.5`.
#'
#' @examples
#' roundTo.5(3.7)    # 3.5
#' roundTo.5(3.2)    # 3.5
#' roundTo.5(-1.3)   # -1.5
#' roundTo.5(-1.7)   # -1.5
#'
#' @family coordinate utilities
#' @export

roundTo.5 <- function(x){
	floor(x) + 0.5
}

#' Round coordinate to nearest 5-degree grid cell center
#'
#' Maps a decimal degree coordinate to the center of its containing 5x5 degree
#' SEAPODYM grid cell. Grid cells are centered at X2.5 or X7.5 (e.g., 2.5, 7.5,
#' 12.5, 17.5, -2.5, -7.5), so any value in a given 5-degree band maps to the
#' center of that band.
#'
#' @param x Numeric value or vector (latitude or longitude in decimal degrees).
#'
#' @return Numeric vector of the same length as `x`, with each value equal to
#'   `floor(x/5)*5 + 2.5`.
#'
#' @examples
#' roundTo2.5(3.7)    # 2.5
#' roundTo2.5(7.2)    # 7.5
#' roundTo2.5(-3)     # -2.5
#' roundTo2.5(-7)     # -7.5
#'
#' @family coordinate utilities
#' @export
roundTo2.5 <- function(x){
	rounded <- numeric(length(x))
	for (i in seq_along(x)) {
		if (x[i] %% 10 < 5) {
			rounded[i] <- floor(x[i]/10) * 10 + 2.5
		} else {
			rounded[i] <- floor(x[i]/10) * 10 + 7.5
		}
	}
	return(rounded)
}

#' Check that required columns exist in a dataframe
#'
#' Internal helper. Stops with a message naming the missing column(s) and the
#' calling function, instead of letting downstream code fail with a generic
#' subsetting error.
#'
#' @param df A dataframe.
#' @param cols Character vector of column names expected in `df`.
#' @param fn_name Character; name of the calling function, used in the error
#'   message.
#'
#' @return Invisibly returns NULL if all columns are present; stops otherwise.
#' @keywords internal
.check_cols_exist <- function(df, cols, fn_name) {
	missing_cols <- setdiff(cols, names(df))
	if (length(missing_cols) > 0) {
		stop(fn_name, ": column(s) not found in data: ",
			 paste(missing_cols, collapse = ", "), call. = FALSE)
	}
	invisible(NULL)
}

#' Remove rows with invalid values in required columns
#'
#' Wraps [is_valid()] over a set of required columns and drops any row where
#' at least one of them is NA, NaN, Inf, or NULL. Logs the removal via
#' [report_removal()].
#'
#' @param df A dataframe.
#' @param required_cols Character vector of column names that must all be
#'   valid for a row to be kept.
#'
#' @return The filtered dataframe.
#'
#' @family cleaning steps
#' @export
remove_invalid <- function(df, required_cols) {
	.check_cols_exist(df, required_cols, "remove_invalid")
	n0 <- nrow(df)
	df_out <- df %>% filter(if_all(all_of(required_cols), is_valid))
	report_removal(n0 - nrow(df_out), n0, "invalid values (NA/NaN/Inf) in required columns")
	df_out
}

#' Remove dummy fisheries (positive catch, zero effort)
#'
#' A row is a dummy fishery if the sum across `catch_cols` is > 0 while
#' `effort_col` is 0. Pass a single column name to replicate checking one
#' species only; pass several to sum across species.
#'
#' @param df A dataframe.
#' @param catch_cols Character vector of one or more catch column names.
#' @param effort_col Character; name of the effort column.
#' @param return_dummy Logical; if TRUE, returns a list with both the
#'   cleaned data and the removed dummy-fishery rows, instead of just the
#'   cleaned data. Default FALSE.
#'
#' @return If `return_dummy = FALSE` (default): the filtered dataframe.
#'   If `return_dummy = TRUE`: a list with elements `data` (filtered
#'   dataframe) and `dummy` (the removed rows).
#'
#' @family cleaning steps
#' @export
remove_dummy_fisheries <- function(df, catch_cols, effort_col, return_dummy = FALSE) {
	.check_cols_exist(df, c(catch_cols, effort_col), "remove_dummy_fisheries")
	n0 <- nrow(df)
	catch_total <- rowSums(as.data.frame(df[, catch_cols, drop = FALSE]), na.rm = TRUE)
	is_dummy <- catch_total > 0 & df[[effort_col]] == 0
	df_out <- df[!is_dummy, ]
	report_removal(sum(is_dummy), n0, "dummy fisheries (catch > 0, effort = 0)")

	if (return_dummy) {
		return(list(data = df_out, dummy = df[is_dummy, ]))
	}
	df_out
}

#' Remove zero fisheries (zero catch and zero effort)
#'
#' @inheritParams remove_dummy_fisheries
#'
#' @return The filtered dataframe.
#'
#' @family cleaning steps
#' @export
remove_zero_fisheries <- function(df, catch_cols, effort_col) {
	.check_cols_exist(df, c(catch_cols, effort_col), "remove_zero_fisheries")
	n0 <- nrow(df)
	catch_total <- rowSums(as.data.frame(df[, catch_cols, drop = FALSE]), na.rm = TRUE)
	df_out <- df[!(catch_total == 0 & df[[effort_col]] == 0), ]
	report_removal(n0 - nrow(df_out), n0, "zero fisheries (catch = 0, effort = 0)")
	df_out
}

#' Remove statistical outliers in a numeric column
#'
#' Drops rows more than `n_sd` standard deviations from the column mean.
#'
#' @param df A dataframe.
#' @param col Character; name of the numeric column to check.
#' @param n_sd Numeric; number of standard deviations defining the cutoff.
#'   Default 3.
#'
#' @return The filtered dataframe.
#'
#' @family cleaning steps
#' @export
remove_outliers_sd <- function(df, col, n_sd = 3) {
	.check_cols_exist(df, col, "remove_outliers_sd")
	n0 <- nrow(df)
	m <- mean(df[[col]], na.rm = TRUE)
	s <- sd(df[[col]], na.rm = TRUE)
	df_out <- df %>% filter(abs(.data[[col]] - m) <= n_sd * s)
	report_removal(n0 - nrow(df_out), n0, paste0(col, " outliers (>", n_sd, " SD)"))
	df_out
}

#' Remove exact duplicate rows
#'
#' @param df A dataframe.
#'
#' @return The filtered dataframe.
#'
#' @family cleaning steps
#' @export
remove_duplicates <- function(df) {
	n0 <- nrow(df)
	df_out <- df %>% distinct()
	report_removal(n0 - nrow(df_out), n0, "exact duplicates")
	df_out
}

#' Apply the Pacific Ocean spatial mask
#'
#' Filters to `lat_range`/`lon_range` and semi-joins against `pmask_lookup`
#' (expected to have columns `latCent`, `lonCent`) using whatever grid-center
#' column names are supplied.
#'
#' @param df A dataframe, already containing rounded grid-center coordinates.
#' @param lat_col,lon_col Character; names of the grid-center lat/lon columns
#'   in `df` (e.g. from [roundTo.5()]).
#' @param pmask_lookup A lookup dataframe with columns `latCent`, `lonCent`.
#'
#' @return The filtered dataframe.
#'
#' @family cleaning steps
#' @export
apply_pacific_mask <- function(df, lat_col, lon_col, pmask_lookup) {
	.check_cols_exist(df, c(lat_col, lon_col), "apply_pacific_mask")
	n0 <- nrow(df)
	join_by <- stats::setNames(c("latCent", "lonCent"), c(lat_col, lon_col))
	df_out <- df %>%
		filter(.data[[lat_col]] >= lat_range[1] & .data[[lat_col]] <= lat_range[2],
			   .data[[lon_col]] >= lon_range[1] & .data[[lon_col]] <= lon_range[2]) %>%
		semi_join(pmask_lookup, by = join_by)
	report_removal(n0 - nrow(df_out), n0, "outside Pacific mask")
	df_out
}

#' Flag and clean Hooks Between Floats (HBF) values
#'
#' Longline-specific. Flags HBF values above `threshold` as outliers without
#' removing rows (adds a logical `hbf_outlier` column), and converts
#' non-positive HBF values to NA.
#'
#' @param df A dataframe.
#' @param hbf_col Character; name of the HBF column. Default "hbf".
#' @param threshold Numeric; HBF values above this are flagged as outliers.
#'   Default 50.
#'
#' @return The dataframe with `hbf_col` <= 0 converted to NA and a new
#'   logical column `hbf_outlier`.
#'
#' @family cleaning steps
#' @export
treat_hbf <- function(df, hbf_col = "hbf", threshold = 50) {
	.check_cols_exist(df, hbf_col, "treat_hbf")

	n_gt <- sum(df[[hbf_col]] > threshold, na.rm = TRUE)
	n_le0 <- sum(df[[hbf_col]] <= 0, na.rm = TRUE)

	df <- df %>% mutate(hbf_outlier = .data[[hbf_col]] > threshold)

	if (n_gt > 0) {
		cat(n_gt, " entries flagged with", hbf_col, ">", threshold, "(",
			round(n_gt / nrow(df) * 100, 2), "% of data)\n")
	}

	df[[hbf_col]] <- ifelse(df[[hbf_col]] <= 0, NA, df[[hbf_col]])

	if (n_le0 > 0) {
		cat(n_le0, " entries with", hbf_col, "<= 0 converted to NA (",
			round(n_le0 / nrow(df) * 100, 2), "% of data)\n")
	}

	df
}

#' Recode purse seine school-association codes
#'
#' Purse-seine-specific. Valid school types are `valid_codes` (default: the
#' full `school_type_lookup$school_type_id` set: -1, 0-8
#' Values outside that set, AND missing values (NA/NaN, or non-numeric
#' strings like "NULL" that coerce to NA), are recoded to -1 ("Unknown",
#' per `school_type_lookup`). Logs a breakdown of exactly which original codes
#' (and how many NAs) got recoded, not just an aggregate count.
#'
#' @param df A dataframe.
#' @param school_col Character; name of the school column. Default "school".
#' @param valid_codes Numeric vector of values considered valid. Default
#'   `school_type_lookup$school_type_id`.
#'
#' @return The dataframe with `school_col` recoded to numeric: valid values
#'   unchanged, out-of-set and missing values set to -1.
#'
#' @family cleaning steps
#' @export
treat_school <- function(df, school_col = "school", valid_codes = school_type_lookup$school_type_id) {
	.check_cols_exist(df, school_col, "treat_school")

	school_num <- suppressWarnings(as.numeric(df[[school_col]]))
	n_total <- nrow(df)

	is_missing <- is.na(school_num)
	is_out_of_range <- !is_missing & !(school_num %in% valid_codes)

	if (any(is_out_of_range)) {
		out_of_range_counts <- table(school_num[is_out_of_range])
		cat("Out-of-range '", school_col, "' values recoded to -1 (Unknown):\n", sep = "")
		for (val in names(out_of_range_counts)) {
			n <- out_of_range_counts[[val]]
			cat("  ", val, ": ", n, " (", round(n / n_total * 100, 2), "% of data)\n", sep = "")
		}
	}

	n_missing <- sum(is_missing)
	if (n_missing > 0) {
		cat("  NA: ", n_missing, " (", round(n_missing / n_total * 100, 2),
			"% of data) recoded to -1 (Unknown)\n", sep = "")
	}

	school_num[is_out_of_range | is_missing] <- -1
	df[[school_col]] <- school_num

	df
}

#' Trim trailing whitespace from a vessel name column
#'
#' Removes no rows -- exists mainly so it can be called conditionally inside
#' [process_catch_data()] when a vessel name column is present.
#'
#' @param df A dataframe.
#' @param vesselname_col Character; name of the vessel name column.
#'
#' @return The dataframe with `vesselname_col` right-trimmed.
#'
#' @family cleaning steps
#' @export
trim_vesselname <- function(df, vesselname_col) {
	.check_cols_exist(df, vesselname_col, "trim_vesselname")
	df %>% mutate(!!vesselname_col := stringr::str_trim(.data[[vesselname_col]], side = "right"))
}

#' Process raw catch/effort fisheries data into cleaned form
#'
#' Runs [remove_invalid()], an optional gear-specific step,
#' [remove_dummy_fisheries()], [remove_zero_fisheries()],
#' [remove_outliers_sd()], an optional vessel-name trim,
#' [remove_duplicates()], and [apply_pacific_mask()] in sequence. Each step
#' prints its own removal report as it runs.
#'
#' Assumes `df` has already been renamed to standard column names, had
#' coordinates converted via [convert_coordinates_df()], and had grid centers
#' computed via [roundTo.5()].
#'
#' Gear-specific step: `gear = "L"` (longline) runs [treat_hbf()];
#' `gear = "S"` (purse seine) runs [treat_school()]. Leave `gear = NULL`
#' (the default) for gears such as pole-and-line that have neither field --
#' the step is skipped entirely rather than forcing a choice. This step runs
#' immediately after [remove_invalid()] and before the catch/effort filters,
#' since the purse-seine ordering hasn't been validated against a real
#' pipeline yet -- flag if that placement turns out to be wrong for your data.
#'
#' Vessel-name trimming runs only if `vesselname_col` is supplied (not all
#' gears have this field, e.g. purse seine per your data), and always runs
#' before [remove_duplicates()] -- trimming after dedup would miss duplicate
#' rows that differ only by trailing whitespace on the vessel name.
#'
#' `catch_cols` is summed across all columns supplied when checking for
#' dummy/zero fisheries, so pass whichever species/catch columns are present
#' and relevant for your gear -- e.g. `_n` (count) columns for longline where
#' available, `_w` (weight) columns for purse seine where `_n` isn't present.
#'
#' @param df A dataframe, pre-renamed and coordinate-converted.
#' @param required_cols Character vector of columns checked for validity.
#' @param catch_cols Character vector of one or more catch column names,
#'   summed when checking dummy/zero fisheries.
#' @param effort_col Character; name of the effort column.
#' @param lat_col,lon_col Character; names of the grid-center lat/lon columns.
#' @param pmask_lookup A lookup dataframe with columns `latCent`, `lonCent`.
#' @param effort_sd_n Numeric; SD cutoff for effort outlier removal. Default 3.
#' @param gear Character or NULL; "L" for longline, "S" for purse seine, or
#'   NULL (default) to skip the gear-specific step entirely.
#' @param hbf_col Character; HBF column name, used when `gear = "L"`.
#'   Default "hbf".
#' @param hbf_threshold Numeric; HBF outlier threshold, used when
#'   `gear = "L"`. Default 50.
#' @param school_col Character; school column name, used when `gear = "S"`.
#'   Default "school".
#' @param vesselname_col Character or NULL; name of the vessel name column,
#'   trimmed before duplicate removal if supplied. Default NULL (skipped).
#' @param return_dummy Logical; if TRUE, returns a list with both the
#'   cleaned data and the removed dummy-fishery rows, instead of just the
#'   cleaned data. Default FALSE.
#'
#' @return If `return_dummy = FALSE` (default): the cleaned dataframe.
#'   If `return_dummy = TRUE`: a list with elements `data` (cleaned
#'   dataframe) and `dummy_fisheries` (the removed dummy-fishery rows).
#'
#' @family cleaning steps
#' @export
process_catch_data <- function(df, required_cols, catch_cols, effort_col,
							   lat_col, lon_col, pmask_lookup, effort_sd_n = 3,
							   gear = NULL,
							   hbf_col = "hbf", hbf_threshold = 50,
							   school_col = "school",
							   vesselname_col = NULL,
							   return_dummy = FALSE) {

	if (!is.null(gear)) {
		gear <- match.arg(gear, c("L", "S", "P", "O", "T", "G", "H", "K", "R"))
	}

	df <- remove_invalid(df, required_cols)

	if (!is.null(gear) && gear == "L") {
		df <- treat_hbf(df, hbf_col = hbf_col, threshold = hbf_threshold)
	} else if (!is.null(gear) && gear == "S") {
		df <- treat_school(df, school_col = school_col)
	}

	dummy_res <- remove_dummy_fisheries(df, catch_cols, effort_col, return_dummy = TRUE)
	df <- dummy_res$data
	dummy_fisheries <- dummy_res$dummy

	df <- remove_zero_fisheries(df, catch_cols, effort_col)
	# df <- remove_outliers_sd(df, effort_col, effort_sd_n)

	if (!is.null(vesselname_col)) {
		df <- trim_vesselname(df, vesselname_col)
	}

	df <- remove_duplicates(df)
	df <- apply_pacific_mask(df, lat_col, lon_col, pmask_lookup)

	if (return_dummy) {
		return(list(data = df, dummy_fisheries = dummy_fisheries))
	}
	df
}

#' Fill in missing or invalid month values from quarter
#'
#' Derives month from quarter (Q1->1, Q2->4, Q3->7, Q4->10). If `mm_col`
#' doesn't exist in `df` at all, it is created entirely from `qtr_col`. If it
#' exists, only missing/invalid entries (NA, or outside 1-12) are filled.
#'
#' @param df A dataframe.
#' @param mm_col Character; name of the month column (created if absent).
#'   Default "mm".
#' @param qtr_col Character; name of the quarter column (1-4). Default "qtr".
#'
#' @return `df` with `mm_col` present and filled from `qtr_col` wherever
#'   missing or invalid.
#'
#' @family cleaning steps
#' @export
fill_missing_month <- function(df, mm_col = "mm", qtr_col = "qtr") {
	.check_cols_exist(df, qtr_col, "fill_missing_month")

	qtr_to_month <- function(q) {
		dplyr::case_when(
			q == 1 ~ 1, q == 2 ~ 4, q == 3 ~ 7, q == 4 ~ 10,
			TRUE ~ NA_real_
		)
	}

	if (!mm_col %in% names(df)) {
		df[[mm_col]] <- qtr_to_month(df[[qtr_col]])
		cat("Column '", mm_col, "' created entirely from '", qtr_col, "'\n", sep = "")
		return(df)
	}

	is_invalid <- is.na(df[[mm_col]]) | !(df[[mm_col]] %in% 1:12)
	n_invalid <- sum(is_invalid)

	df[[mm_col]][is_invalid] <- qtr_to_month(df[[qtr_col]][is_invalid])

	if (n_invalid > 0) {
		cat(n_invalid, " entries with missing/invalid '", mm_col, "' filled from '", qtr_col, "' (",
			round(n_invalid / nrow(df) * 100, 2), "% of data)\n", sep = "")
	}

	df
}

#' Remove rows with invalid categorical values in a column
#'
#' Generic category-validity filter -- e.g. for strata codes, school types,
#' or gear codes where only a known set of values is meaningful. Note this
#' also drops NA values in `col`, since `NA %in% valid_values` is FALSE.
#'
#' @param df A dataframe.
#' @param col Character; name of the column to check.
#' @param valid_values Vector of allowed values.
#'
#' @return The filtered dataframe.
#'
#' @family cleaning steps
#' @export
filter_valid_categories <- function(df, col, valid_values) {
	.check_cols_exist(df, col, "filter_valid_categories")
	n0 <- nrow(df)
	df_out <- df %>% filter(.data[[col]] %in% valid_values)
	report_removal(n0 - nrow(df_out), n0, paste0("invalid ", col, " values"))
	df_out
}

#' Compute grid-cell center coordinates from a strata lookup table
#'
#' Adds a lon/lat offset to raw coordinates based on a strata code, using a
#' lookup table supplied by the caller. Offsets are data-source-specific
#' (e.g. resolution encoded in a stratification column) -- build the lookup
#' table in the calling document, not in the package.
#'
#' @param df A dataframe.
#' @param strat_col Character; name of the strata code column (e.g.
#'   "ASTRAT"). Must exist with the same name in both `df` and `lookup`.
#' @param lookup A dataframe with a column matching `strat_col`, plus
#'   `lon_offset` and `lat_offset`.
#' @param lon_col,lat_col Character; names of the raw longitude/latitude
#'   columns in `df` to offset.
#'
#' @return `df` with new columns `lonCent`/`latCent`. Rows whose `strat_col`
#'   value has no match in `lookup` get NA grid centers -- run
#'   `filter_valid_categories()` against the same set of values beforehand
#'   if that's not what you want.
#'
#' @family cleaning steps
#' @export
compute_grid_center <- function(df, strat_col, lookup, lon_col, lat_col) {
	.check_cols_exist(df, c(strat_col, lon_col, lat_col), "compute_grid_center")
	.check_cols_exist(lookup, c(strat_col, "lon_offset", "lat_offset"), "compute_grid_center")

	df_out <- df %>%
		left_join(lookup, by = strat_col) %>%
		mutate(
			lonCent = .data[[lon_col]] + lon_offset,
			latCent = .data[[lat_col]] + lat_offset
		) %>%
		select(-lon_offset, -lat_offset)

	n_unmatched <- sum(is.na(df_out$lonCent) | is.na(df_out$latCent))
	if (n_unmatched > 0) {
		cat(n_unmatched, " rows had no matching '", strat_col, "' in the lookup table (",
			round(n_unmatched / nrow(df_out) * 100, 2), "% of data)\n", sep = "")
	}

	df_out
}

#' Filter to a latitude/longitude bounding box
#'
#' Unlike `apply_pacific_mask()`, this does not semi-join against a grid
#' lookup table -- use it when data resolution doesn't match
#' `pmask_lookup`'s 1x1 grid (e.g. aggregated length-frequency data at 5x5
#' or coarser strata). No default range is provided (unlike
#' `apply_pacific_mask()`, which hardcodes the package's `lat_range`/
#' `lon_range`) to avoid a self-referential default -- pass the package
#' constants explicitly if you want the same bounds.
#'
#' @param df A dataframe.
#' @param lat_col,lon_col Character; names of latitude/longitude columns.
#' @param lat_range,lon_range Numeric vectors of length 2.
#'
#' @return The filtered dataframe.
#'
#' @family cleaning steps
#' @export
filter_bounding_box <- function(df, lat_col, lon_col, lat_range, lon_range) {
	.check_cols_exist(df, c(lat_col, lon_col), "filter_bounding_box")
	n0 <- nrow(df)
	df_out <- df %>%
		filter(.data[[lat_col]] >= lat_range[1] & .data[[lat_col]] <= lat_range[2],
			   .data[[lon_col]] >= lon_range[1] & .data[[lon_col]] <= lon_range[2])
	report_removal(n0 - nrow(df_out), n0, "outside bounding box")
	df_out
}

#' Remove length-frequency samples with insufficient bin diversity
#'
#' Drops groups (one row per group = one sample) that have fewer than
#' `min_bins` distinct length values, or where every row in the group has
#' the same frequency/count value (uninformative sample).
#'
#' @param df A dataframe.
#' @param group_cols Character vector of columns identifying a sample.
#' @param len_col Character; name of the length column. Default "len".
#' @param freq_col Character; name of the frequency/count column. Default
#'   "count".
#' @param min_bins Integer; minimum distinct length bins required. Default 3.
#'
#' @return The filtered dataframe.
#'
#' @family cleaning steps
#' @export
remove_sparse_lf_samples <- function(df, group_cols, len_col = "len", freq_col = "count", min_bins = 3) {
	.check_cols_exist(df, c(group_cols, len_col, freq_col), "remove_sparse_lf_samples")
	n0 <- nrow(df)

	df_out <- df %>%
		group_by(across(all_of(group_cols))) %>%
		filter(n_distinct(.data[[len_col]]) >= min_bins,
			   n_distinct(.data[[freq_col]]) > 1) %>%
		ungroup()

	report_removal(n0 - nrow(df_out), n0,
				   paste0("samples with <", min_bins, " length bins or a single frequency value"))
	df_out
}

#' Disaggregate length-frequency bins to 1cm resolution
#'
#' Expands each row with bin size `LSTRAT > 1` into `LSTRAT` separate 1cm
#' bins, splitting `count` proportionally across them. Rewritten from the
#' original dplyr `uncount()` + grouped `row_number()` version to plain base
#' R row-replication (`rep()`/`sequence()`), which avoids a grouped mutate
#' over every expanded row and should be noticeably faster on large data.
#' Behavior should match the original except that it no longer overwrites
#' any pre-existing `id` column -- the original's `mutate(id = row_number())`
#' would have clobbered a real trip ID with a meaningless row counter if one
#' was already present in the data.
#'
#' @param df A dataframe with columns `len`, `count`, `LSTRAT`. `LSTRAT` is
#'   assumed to be a positive integer with no NAs -- filter/impute upstream
#'   if that's not guaranteed.
#'
#' @return The disaggregated dataframe (base data.frame), `LSTRAT` reset to
#'   1 throughout.
#'
#' @family cleaning steps
#' @export
bin_to_1cm <- function(df) {
	.check_cols_exist(df, c("len", "count", "LSTRAT"), "bin_to_1cm")

	rep_counts <- df$LSTRAT
	offsets <- sequence(rep_counts) - 1

	df_out <- df[rep(seq_len(nrow(df)), rep_counts), ]
	df_out$len <- df_out$len + offsets
	df_out$count <- df_out$count / rep(rep_counts, rep_counts)
	df_out$LSTRAT <- 1
	rownames(df_out) <- NULL

	df_out
}

#' Process raw length-frequency data into cleaned form
#'
#' Runs `fill_missing_month()`, `remove_invalid()`,
#' `filter_valid_categories()`, `compute_grid_center()`,
#' `filter_bounding_box()`, and `remove_sparse_lf_samples()` in sequence.
#' Bin disaggregation (`bin_to_1cm()`) is NOT included -- it's a distinct
#' downstream step, not a cleaning step, call it separately on the result.
#' No gear branching, unlike `process_catch_data()` -- gear isn't relevant
#' to this cleaning pipeline.
#'
#' @param df A dataframe, pre-renamed to standard column names.
#' @param required_cols Character vector of columns checked for validity by
#'   `remove_invalid()` (e.g. length and raw coordinate columns).
#' @param mm_col,qtr_col Character; month/quarter columns, passed to
#'   `fill_missing_month()`.
#' @param strat_col Character; strata code column, passed to
#'   `filter_valid_categories()` and `compute_grid_center()`.
#' @param valid_strat Vector of allowed strata codes.
#' @param strat_lookup A dataframe mapping `strat_col` to `lon_offset`/
#'   `lat_offset`, passed to `compute_grid_center()`.
#' @param lon_col,lat_col Character; raw coordinate columns.
#' @param lat_range,lon_range Numeric vectors of length 2 bounding the
#'   region of interest, passed to `filter_bounding_box()`.
#' @param group_cols Character vector of columns identifying one LF sample,
#'   passed to `remove_sparse_lf_samples()`.
#' @param len_col,freq_col Character; length/frequency columns.
#' @param min_bins Integer; minimum distinct length bins per sample.
#'   Default 3.
#'
#' @return The cleaned dataframe.
#'
#' @family cleaning steps
#' @export
process_lf_data <- function(df, required_cols, mm_col = "mm", qtr_col = "qtr",
							strat_col, valid_strat, strat_lookup,
							lon_col, lat_col, lat_range, lon_range,
							group_cols, len_col = "len", freq_col = "count", min_bins = 3) {

	df <- fill_missing_month(df, mm_col = mm_col, qtr_col = qtr_col)
	df <- remove_invalid(df, required_cols)
	df <- filter_valid_categories(df, strat_col, valid_strat)
	df <- compute_grid_center(df, strat_col, strat_lookup, lon_col = lon_col, lat_col = lat_col)
	df <- filter_bounding_box(df, lat_col = "latCent", lon_col = "lonCent",
							  lat_range = lat_range, lon_range = lon_range)
	df <- remove_sparse_lf_samples(df, group_cols, len_col = len_col, freq_col = freq_col, min_bins = min_bins)
	df
}

#' Convert length-frequency school-type letter codes to numeric
#'
#' Translates the letter-coded school associations used in raised
#' length-frequency data (F, L, U, M, A, O -- per source description: F =
#' Drifting or Anchored FAD, L = Log, U = Unassociated, M = Marine mammal
#' (EPO), A = Associated, O = Other) to the numeric scheme in
#' `school_type_lookup`. LF data doesn't distinguish the finer categories
#' the numeric scheme allows for F (4 or 5, drifting vs anchored) or U (1
#' or 2, unassociated vs feeding on baitfish), so this collapses each to a
#' single fixed value (F->4, U->1) by convention.
#'
#' Values not present in `lookup` become NA in `output_col`, rather than
#' being guessed at. Run `treat_school()` on the result afterward -- it
#' applies the valid-code/unknown-recode logic and will treat those NAs as
#' missing, not as unknown.
#'
#' @param df A dataframe.
#' @param school_col Character; name of the raw letter-coded school column.
#'   Default "school_LF".
#' @param output_col Character; name of the new numeric column to create.
#'   Default "school". The original `school_col` is left untouched for
#'   traceability.
#' @param lookup Named character vector mapping letter codes to numeric
#'   codes. Default: F->4, L->3, U->1, M->6, A->7, O->8.
#'
#' @return `df` with a new column `output_col` holding the numeric codes.
#'
#' @family cleaning steps
#' @export
convert_school_letters <- function(df, school_col = "school_LF", output_col = "school",
								   lookup = c("F" = 4, "L" = 3, "U" = 1, "M" = 6, "A" = 7, "O" = 8)) {
	.check_cols_exist(df, school_col, "convert_school_letters")

	raw_vals <- as.character(df[[school_col]])
	recoded <- unname(lookup[raw_vals])

	n_unmapped <- sum(!raw_vals %in% names(lookup) & !is.na(raw_vals))
	if (n_unmapped > 0) {
		cat(n_unmapped, " entries with '", school_col, "' values not in the lookup, set to NA in '",
			output_col, "' (", round(n_unmapped / nrow(df) * 100, 2), "% of data)\n", sep = "")
	}

	df[[output_col]] <- recoded
	df
}

#' School type identifiers reference table
#'
#' Reference table for purse-seine school-association codes (Reference
#' Table 2). Used as the default `valid_codes` for treat_school() -- note
#' this is NOT a contiguous numeric range (it includes -1)
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{school_type_id}{Numeric school type code.}
#'   \item{description}{Description of the school type.}
#' }
#' @export
school_type_lookup <- data.frame(
	school_type_id = c(0, 1, 2, 3, 4, 5, 6, 7, 8, -1),
	description = c(
		"No school type (only searching activity)",
		"Unassociated/free school",
		"Feeding on baitfish",
		"Drifting log, debris or dead animal",
		"Drifting raft, FAD or payao",
		"Anchored raft, FAD or payao",
		"Live whale",
		"Live whale shark",
		"Other",
		"Unknown"
	),
	stringsAsFactors = FALSE
)

#' Standardize length measurement codes to Upper Jaw to Caudal Fork (UF)
#'
#' Converts SD/US and PS-coded lengths to UF using the allometric conversions
#' from the "Project 90 update" (US treated as equivalent to SD). UF values
#' are left unchanged. Codes outside c("UF","SD","US","PS") are left as-is --
#' run `infer_len_code_from_trip()` first to recover some of those from trip
#' context, and `filter_valid_categories()` afterwards to drop what's left.
#'
#' @param df A dataframe.
#' @param len_col Character; name of the length column. Default "len".
#' @param code_col Character; name of the length-code column. Default "len_code".
#'
#' @return `df` with `len_col` converted to UF-equivalent lengths and
#'   `code_col` set to "UF" for rows that were converted.
#'
#' @family cleaning steps
#' @export
standardize_length_code <- function(df, len_col = "len", code_col = "len_code") {
	.check_cols_exist(df, c(len_col, code_col), "standardize_length_code")

	sd_us <- df[[code_col]] %in% c("SD", "US")
	ps <- df[[code_col]] %in% "PS"

	df[[len_col]][sd_us] <- 3.951 * as.numeric(df[[len_col]][sd_us])^0.8369
	df[[len_col]][ps] <- 11.385 * as.numeric(df[[len_col]][ps])^0.6619
	df[[code_col]][sd_us | ps] <- "UF"

	n_conv <- sum(sd_us) + sum(ps)
	if (n_conv > 0) {
		cat(n_conv, " lengths converted to UF (", sum(sd_us), " SD/US, ", sum(ps), " PS): ",
			round(n_conv / nrow(df) * 100, 2), "% of data\n", sep = "")
	}
	df
}

#' Infer missing/invalid length codes from trip context
#'
#' For rows whose `code_col` isn't in `valid_codes`, checks whether the other
#' rows sharing the same `id_col` agree on a single valid code -- if so,
#' applies it. Rows with no consistent code among trip-mates are left
#' unchanged (to be dropped later, e.g. via `filter_valid_categories()`).
#'
#' @param df A dataframe.
#' @param id_col Character; name of the trip/sample identifier column. Default "id".
#' @param code_col Character; name of the length-code column. Default "len_code".
#' @param valid_codes Character vector of codes considered valid.
#'   Default c("UF","SD","PS","US").
#'
#' @return `df` with `code_col` recovered where possible.
#'
#' @family cleaning steps
#' @export
infer_len_code_from_trip <- function(df, id_col = "id", code_col = "len_code",
									 valid_codes = c("UF", "SD", "PS", "US")) {
	.check_cols_exist(df, c(id_col, code_col), "infer_len_code_from_trip")

	is_invalid <- !(df[[code_col]] %in% valid_codes)
	n_invalid <- sum(is_invalid)
	if (n_invalid == 0) return(df)

	affected_ids <- unique(df[[id_col]][is_invalid])

	trip_codes <- df %>%
		filter(.data[[id_col]] %in% affected_ids, .data[[code_col]] %in% valid_codes) %>%
		distinct(.data[[id_col]], .data[[code_col]]) %>%
		group_by(.data[[id_col]]) %>%
		filter(n() == 1) %>%  # trip has exactly one agreed-upon valid code
		ungroup()
	names(trip_codes) <- c(id_col, "inferred_code")

	df <- df %>%
		left_join(trip_codes, by = id_col) %>%
		mutate(!!code_col := ifelse(!(.data[[code_col]] %in% valid_codes) & !is.na(inferred_code),
									inferred_code, .data[[code_col]])) %>%
		select(-inferred_code)

	n_recovered <- n_invalid - sum(!(df[[code_col]] %in% valid_codes))
	cat(n_recovered, " of ", n_invalid, " rows with invalid '", code_col, "' recovered from trip context (",
		round(n_recovered / n_invalid * 100, 2), "% of invalid rows)\n", sep = "")
	df
}


#' Great-circle distance between two points
#'
#' Haversine formula. Vectorized: lat1/lon1 can be scalars while lat2/lon2
#' are vectors (or vice versa), following standard R recycling.
#'
#' @param lat1,lon1 Numeric; latitude/longitude of the first point(s), in
#'   decimal degrees.
#' @param lat2,lon2 Numeric; latitude/longitude of the second point(s), in
#'   decimal degrees.
#'
#' @return Numeric distance(s) in kilometers.
#'
#' @family hampel filter
#' @export
calculate_distance <- function(lat1, lon1, lat2, lon2) {
	lat1_rad <- lat1 * pi / 180
	lon1_rad <- lon1 * pi / 180
	lat2_rad <- lat2 * pi / 180
	lon2_rad <- lon2 * pi / 180

	R <- 6371  # Earth's radius in km
	dlat <- lat2_rad - lat1_rad
	dlon <- lon2_rad - lon1_rad

	a <- sin(dlat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon / 2)^2
	c <- 2 * atan2(sqrt(a), sqrt(1 - a))
	R * c
}

#' Hampel filter for length-frequency mean-length observations, applied per region
#'
#' For each region x date observation, compares its `what` value against the
#' median/MAD of same-region values within `month_window` months of it,
#' among the rows in `df`, and flags it as an outlier if it exceeds
#' `median + k * (mad_scale-weighted MAD)`. Only rows with at least 3
#' reference points (same region, within the time window) get a computed
#' flag; others pass through unchanged with `n_points`/`mad` left NA. Rows
#' with `what <= 0` are skipped (never flagged, `n_points`/`mad` left NA),
#' matching the original per-row behavior.
#'
#' Precomputes the region subset once per unique `region` in `df`, then
#' reuses that (much smaller) subset for every date query within that
#' region -- same precompute-once-per-group pattern as
#' [hampel_CE_batch()], replacing what was previously a per-row
#' `dplyr::filter()` rescan of the full `ref_data` table. Same
#' self-inclusion note as [hampel_CE_batch()]: the reference subset
#' includes the observation itself.
#'
#' @param df A dataframe with columns `region`, `date`, and whatever `what`
#'   names (same as `ref_data` inside [apply_hampel()]).
#' @param month_window Numeric; +/- months defining the time window.
#' @param k Numeric; number of (scaled) MADs defining the outlier threshold.
#' @param mad_scale Numeric; MAD-to-SD scaling constant. Default 1.4826
#'   (standard constant for consistency with a normal distribution).
#' @param what Character; name of the column in `df` to test for outliers.
#'   Default "mean_len".
#'
#' @return `df` with `outlier` (logical), `n_points` (reference points
#'   found), and `mad` (scaled MAD used) columns added.
#'
#' @family hampel filter
#' @export
hampel_LF_batch <- function(df, month_window, k, mad_scale = 1.4826, what = "mean_len") {
	.check_cols_exist(df, c("region", "date", what), "hampel_LF_batch")

	value_vec <- df[[what]]

	outlier  <- rep(FALSE, nrow(df))
	n_points <- rep(NA_integer_, nrow(df))
	mad_out  <- rep(NA_real_, nrow(df))

	for (reg in unique(df$region)) {
		reg_idx   <- which(df$region == reg)
		reg_date  <- df$date[reg_idx]
		reg_value <- value_vec[reg_idx]

		for (i in seq_along(reg_idx)) {
			qi <- reg_idx[i]
			if (value_vec[qi] <= 0) next

			within_vals <- reg_value[reg_date >= (reg_date[i] - months(month_window)) &
									 	reg_date <= (reg_date[i] + months(month_window))]

			np <- length(within_vals)
			n_points[qi] <- np
			if (np >= 3) {
				med <- median(within_vals, na.rm = TRUE)
				mad_value <- mad_scale * median(abs(within_vals - med), na.rm = TRUE)
				mad_out[qi] <- mad_value
				outlier[qi] <- abs(value_vec[qi] - med) > (k * mad_value)
			}
		}
	}

	df$outlier <- outlier
	df$n_points <- n_points
	df$mad <- mad_out
	df
}

#' Hampel filter for catch/effort CPUE, applied per fishery
#'
#' For each row, compares its CPUE against the median/MAD of same-month
#' CPUE values within `year_window` years and `radius_km` kilometers of it,
#' among the rows in `df`, and caps CPUE from above at
#' `median + k * (mad_scale-weighted MAD)` if it exceeds that. Only rows
#' with at least 3 reference points (in both the time window and the radius)
#' get a computed cap; others pass through unchanged with `n_points`/`mad`
#' left NA.
#'
#' Precomputes the time-window reference subset once per unique (yr, mm)
#' pair present in `df`. Distances are still computed one query point at a time
#' against the (much smaller, precomputed) reference subset. Used internally
#' by [apply_hampel()] (`type = "CE"`); exported so it can be run directly.
#'
#' @param df CPUE>0 subset for one fishery, with columns `date`, `lat`,
#'   `lon`, `CPUE` (same as `data_CPUEpos` inside [apply_hampel()]).
#' @param year_window Numeric; +/- years defining the time window.
#' @param radius_km Numeric; spatial radius in km.
#' @param k Numeric; number of (scaled) MADs defining the outlier threshold.
#' @param mad_scale Numeric; MAD-to-SD scaling constant. Default 1.4826
#'   (standard constant for consistency with a normal distribution).
#'
#' @return `df` with `newvalue` (capped CPUE), `n_points` (reference points
#'   found), and `mad` (scaled MAD used) columns added.
#'
#' @family hampel filter
#' @export
hampel_CE_batch <- function(df, year_window, radius_km, k, mad_scale = 1.4826) {
	.check_cols_exist(df, c("date", "lat", "lon", "CPUE"), "hampel_CE_batch")

	yr_vec <- year(df$date)
	mm_vec <- month(df$date)

	newvalue <- df$CPUE
	n_points <- rep(NA_integer_, nrow(df))
	mad_out  <- rep(NA_real_, nrow(df))

	yr_mm_groups <- unique(data.frame(yr = yr_vec, mm = mm_vec))

	for (g in seq_len(nrow(yr_mm_groups))) {
		g_yr <- yr_mm_groups$yr[g]
		g_mm <- yr_mm_groups$mm[g]

		ref_idx <- which(mm_vec == g_mm & yr_vec >= g_yr - year_window & yr_vec <= g_yr + year_window)
		if (length(ref_idx) < 3) next

		query_idx <- which(yr_vec == g_yr & mm_vec == g_mm)

		ref_lat <- df$lat[ref_idx]
		ref_lon <- df$lon[ref_idx]
		ref_val <- df$CPUE[ref_idx]

		for (qi in query_idx) {
			d <- calculate_distance(df$lat[qi], df$lon[qi], ref_lat, ref_lon)
			within_vals <- ref_val[d <= radius_km]
			np <- length(within_vals)
			n_points[qi] <- np
			if (np >= 3) {
				med <- median(within_vals, na.rm = TRUE)
				mad_value <- mad_scale * median(abs(within_vals - med), na.rm = TRUE)
				mad_out[qi] <- mad_value
				newvalue[qi] <- min(df$CPUE[qi], med + k * mad_value)
			}
		}
	}

	df$newvalue <- newvalue
	df$n_points <- n_points
	df$mad <- mad_out
	df
}

#' Apply a Hampel filter per fishery, for catch/effort or length-frequency data
#'
#' Loops over each fishery (`f`) in `df` and runs [hampel_CE_batch()] (for
#' `type = "CE"`) or [hampel_LF_batch()] (for
#' `type = "LF"`), printing a running summary per fishery as it goes.
#'
#' `type = "CE"` expects `df` to have `f`, `yr`, `mm`, `lat`, `lon`, `E`,
#' `C`, and `CPUE` (compute `CPUE = C / E` beforehand).
#' Caps CPUE from above per [hampel_CE_batch()], then
#' backs out an adjusted effort (`adjusted_E = C / newvalue`) holding catch
#' fixed. This assumes catch is more reliable than effort when the two
#' disagree.
#'
#' `type = "LF"` expects `df` to have `f`, `yr`, `mm`, `lon_from`, `lon_to`,
#' `lat_from`, `lat_to`, `len`, `count`.
#'
#' Performance note: each row/group independently re-filters and re-scans
#' `ref_data`, giving O(n) work per row and O(n^2) overall per fishery.
#' Not restructured here, fine at moderate size, may be slow for very
#' large fisheries.
#'
#' @param df A dataframe (see column requirements above, by `type`).
#' @param type Character; "CE" or "LF".
#' @param year_window Numeric; CE only, +/- years defining the time window.
#' @param radius_km Numeric; CE only, spatial radius in km.
#' @param month_window Numeric; LF only, +/- months defining the time
#'   window.
#' @param k Numeric; number of (scaled) MADs defining the outlier
#'   threshold, both types.
#' @param mad_scale Numeric; MAD-to-SD scaling constant. Default 1.4826
#'   (standard constant for consistency with a normal distribution).
#'
#' @return A dataframe: for "CE", `df` with `newvalue`, `n_points`, `mad`,
#'   `adjusted_E`, `is_adjusted` columns added (no rows removed). For "LF",
#'   `df` (at region x date x len resolution) with outlier region/date
#'   combinations removed.
#'
#' @family hampel filter
#' @export
apply_hampel <- function(df, type, year_window = NULL, radius_km = NULL,
						 month_window = NULL, k, mad_scale = 1.4826) {

	type <- match.arg(type, c("CE", "LF"))

	if (type == "CE") {
		.check_cols_exist(df, c("f", "yr", "mm", "lat", "lon", "E", "C", "CPUE"), "apply_hampel")
		if (is.null(year_window) || is.null(radius_km)) {
			stop("apply_hampel: year_window and radius_km are required when type = 'CE'", call. = FALSE)
		}
	} else {
		.check_cols_exist(df, c("f", "yr", "mm", "lon_from", "lon_to", "lat_from", "lat_to", "len", "count"),
						  "apply_hampel")
		if (is.null(month_window)) {
			stop("apply_hampel: month_window is required when type = 'LF'", call. = FALSE)
		}
	}

	fisheries <- sort(unique(df$f))
	res_list <- vector("list", length(fisheries))

	for (i in seq_along(fisheries)) {
		ifish <- fisheries[i]
		cat(sprintf("Hampel filter for fishery %s...\n", ifish))

		if (type == "CE") {
			subset_f <- df %>%
				filter(f == ifish) %>%
				mutate(date = make_date(yr, mm, "15"))

			data_CPUE_null <- subset_f %>% filter(CPUE == 0)
			data_CPUEpos <- subset_f %>% filter(CPUE > 0)

			hampel_filtered_CE <- hampel_CE_batch(
				data_CPUEpos,
				year_window = year_window, radius_km = radius_km,
				k = k, mad_scale = mad_scale
			) %>%
				mutate(
					adjusted_E = case_when(
						CPUE == 0 ~ E,
						newvalue != CPUE ~ C / newvalue,
						TRUE ~ E
					),
					is_adjusted = E != adjusted_E
				)

			result <- data_CPUE_null %>%
				mutate(newvalue = NA, n_points = NA, mad = NA, adjusted_E = E, is_adjusted = FALSE) %>%
				bind_rows(hampel_filtered_CE)

			n_pos <- sum(result$CPUE > 0)
			n_adj <- sum(result$is_adjusted)
			cat("Summary:\n")
			cat(sprintf("Total points: %s\n", nrow(result)))
			cat(sprintf("Points with non-zero CPUE: %s\n", n_pos))
			cat(sprintf("Points adjusted: %s\n", n_adj))
			cat(sprintf("Percentage adjusted: %s%%\n\n", round(100 * n_adj / n_pos, 2)))

		} else {
			data <- df %>%
				filter(f == ifish) %>%
				mutate(date = make_date(yr, mm, "15")) %>%
				arrange(lon_from, lon_to, lat_from, lat_to) %>%
				group_by(lon_from, lon_to, lat_from, lat_to) %>%
				mutate(region = cur_group_id()) %>%
				ungroup()

			ref_data <- data %>%
				group_by(region, date) %>%
				mutate(len = as.numeric(as.character(len))) %>%
				summarise(mean_len = sum(count * len) / sum(count), .groups = "drop") %>%
				drop_na(mean_len)

			outlier_hampel_LF <- hampel_LF_batch(
				ref_data,
				month_window = month_window, k = k, mad_scale = mad_scale, what = "mean_len"
			) %>%
				rename_with(~ gsub("hampel_results-", "", .x))

			joined <- data %>%
				left_join(outlier_hampel_LF %>% dplyr::select(region, date, outlier), by = c("region", "date"))
			n_outlier_rows <- sum(joined$outlier, na.rm = TRUE)
			result <- joined %>% filter(!outlier)

			cat("Summary:\n")
			cat(sprintf("Total points (region x date x len): %s\n", nrow(result)))
			cat(sprintf("Total unique points (region x date): %s\n", nrow(ref_data)))
			cat(sprintf("Unique outlier points: %s\n", sum(outlier_hampel_LF$outlier)))
			cat(sprintf("Total outlier points (rows): %s\n", n_outlier_rows))
			cat(sprintf("Percentage outlier discarded: %s%%\n\n",
						round(100 * n_outlier_rows / (nrow(result) + n_outlier_rows), 2)))
		}

		res_list[[i]] <- result
	}

	bind_rows(res_list)
}
