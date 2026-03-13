#' @importFrom dplyr left_join mutate select filter group_by summarise bind_rows rename
#' @importFrom rlang .data := !!

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
