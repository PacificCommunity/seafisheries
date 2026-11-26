# ==============================================================================
# Helpers for writing the SEAPODYM fisheries input files (format_EC(),
# format_LF()): model grid and mask from a SEAPODYM parfile, mask filtering of
# catch-effort records, and expansion of length regions smaller than a model
# cell.
# ==============================================================================


#' Model grid of a SEAPODYM parfile
#'
#' Reads the domain, resolution and mask file of a SEAPODYM parameter file
#' (XML). The domain values (`latitudeMin`, `latitudeMax`, `longitudeMin`,
#' `longitudeMax`) are the outer cell edges. The resolution is `space_reso`
#' (degrees) if present, else `deltaX` (minutes) / 60.
#'
#' @param parfile Character; path to the parfile.
#'
#' @return A list: `lat`, `lon` (edges), `res` (degrees), `lat_centres`,
#'   `lon_centres`, `mask_file` (strdir + str_file_mask, NA if absent),
#'   `first_date`, `last_date` (save_first_date / save_last_date, NA if absent).
#'
#' @family seapodym formatting
#' @export
read_parfile_grid <- function(parfile) {
	if (!file.exists(parfile)) stop("Parfile not found: ", parfile)
	txt <- paste(readLines(parfile, warn = FALSE), collapse = "\n")
	tag <- function(name, attr = "value") {
		m <- regmatches(txt, regexpr(sprintf("<%s\\b[^>]*\\b%s=\"[^\"]*\"", name, attr), txt))
		if (!length(m)) return(NA_character_)
		sub(sprintf(".*\\b%s=\"([^\"]*)\".*", attr), "\\1", m)
	}
	num <- function(name) as.numeric(tag(name))
	lat <- c(num("latitudeMin"), num("latitudeMax"))
	lon <- c(num("longitudeMin"), num("longitudeMax"))
	if (anyNA(c(lat, lon))) stop("latitudeMin/Max or longitudeMin/Max not found in ", parfile)
	res <- num("space_reso")
	if (is.na(res)) res <- num("deltaX") / 60
	if (is.na(res)) stop("space_reso or deltaX not found in ", parfile)
	mask_file <- if (is.na(tag("str_file_mask"))) NA_character_ else
		file.path(ifelse(is.na(tag("strdir")), ".", tag("strdir")), tag("str_file_mask"))
	date <- function(name) {
		y <- tag(name, "year"); m <- tag(name, "month"); d <- tag(name, "day")
		if (is.na(y)) return(as.Date(NA))
		as.Date(sprintf("%s-%s-%s", y, m, ifelse(is.na(d), 15, d)))
	}
	list(lat = lat, lon = lon, res = res,
		 lat_centres = seq(lat[1] + res / 2, lat[2] - res / 2, by = res),
		 lon_centres = seq(lon[1] + res / 2, lon[2] - res / 2, by = res),
		 mask_file = mask_file,
		 first_date = date("save_first_date"), last_date = date("save_last_date"))
}


#' SEAPODYM mask as a table of cells
#'
#' @param grid A grid from [read_parfile_grid()].
#' @param mask_file Character; mask text file (one row per latitude, north
#'   first). Default `grid$mask_file`.
#'
#' @return A data frame of the model cells: `lat`, `lon` (centres), `value`
#'   (0 = land, 1-3 = number of vertical layers).
#'
#' @family seapodym formatting
#' @export
read_seapodym_mask <- function(grid, mask_file = grid$mask_file) {
	if (is.na(mask_file) || !file.exists(mask_file)) stop("Mask file not found: ", mask_file)
	nlat <- length(grid$lat_centres); nlon <- length(grid$lon_centres)
	v <- scan(mask_file, nlines = nlat, quiet = TRUE)
	if (length(v) != nlat * nlon)
		stop("Mask ", mask_file, " has ", length(v), " values, the grid has ", nlat, " x ", nlon)
	m <- matrix(v, nrow = nlat, ncol = nlon, byrow = TRUE)
	data.frame(lat = rep(rev(grid$lat_centres), times = nlon),
			   lon = rep(grid$lon_centres, each = nlat),
			   value = as.vector(m))
}


#' Remove catch-effort records on land or outside the model domain
#'
#' A record is a cell of size `res` centred on (`lat`, `lon`). It is kept if
#' at least one sea cell of the model (mask value > 0) covers it: the model
#' cell containing its centre when the record is at most as large as a model
#' cell, or any model cell whose centre lies inside it when it is larger
#' (e.g. 5 deg records on a 1 or 2 deg model).
#'
#' @param df Data frame with `lat_col`, `lon_col`, `res_col`.
#' @param mask Mask cells from [read_seapodym_mask()].
#' @param grid Grid from [read_parfile_grid()].
#' @param lat_col,lon_col,res_col Column names.
#'
#' @return `df` without the records on land or outside the domain.
#'
#' @family seapodym formatting
#' @export
apply_seapodym_mask <- function(df, mask, grid, lat_col = "lat", lon_col = "lon", res_col = "res") {
	.check_cols_exist(df, c(lat_col, lon_col, res_col), "apply_seapodym_mask")
	n0 <- nrow(df)
	r <- grid$res
	sea <- mask[mask$value > 0, c("lat", "lon")]
	key <- function(la, lo) paste(round(la, 4), round(lo, 4))
	sea_key <- key(sea$lat, sea$lon)

	lat <- df[[lat_col]]; lon <- df[[lon_col]]; res <- df[[res_col]]
	keep <- logical(n0)
	# record at most as large as a model cell: model cell containing its centre
	small <- res <= r
	clat <- grid$lat[1] + (floor((lat[small] - grid$lat[1]) / r) + 0.5) * r
	clon <- grid$lon[1] + (floor((lon[small] - grid$lon[1]) / r) + 0.5) * r
	keep[small] <- key(clat, clon) %in% sea_key
	# larger record: any sea cell centre inside it
	if (any(!small)) {
		u <- unique(data.frame(lat = lat[!small], lon = lon[!small], res = res[!small]))
		u$sea <- vapply(seq_len(nrow(u)), function(i)
			any(abs(sea$lat - u$lat[i]) < u$res[i] / 2 & abs(sea$lon - u$lon[i]) < u$res[i] / 2),
			logical(1))
		keep[!small] <- u$sea[match(key(lat[!small], lon[!small]) |> paste(res[!small]),
									key(u$lat, u$lon) |> paste(u$res))]
	}
	report_removal(sum(!keep), n0, "on land or outside the SEAPODYM domain")
	df[keep, , drop = FALSE]
}


#' Expand length regions smaller than a model cell
#'
#' Length-frequency regions (`lon_from`, `lon_to`, `lat_from`, `lat_to`)
#' narrower than a model cell in a direction are replaced, in that direction,
#' by the edges of the model cell containing their centre (e.g. 1x1 deg
#' regions become the 2x2 deg cell they fall in), so that samples of the same
#' model cell can be merged.
#'
#' @param df Data frame with the region columns.
#' @param grid Grid from [read_parfile_grid()]: its domain edges are the origin
#'   of the cells.
#' @param res Cell size (degrees) the regions are expanded to. Default the
#'   model resolution, `grid$res`.
#'
#' @return `df` with the expanded regions.
#'
#' @family seapodym formatting
#' @export
expand_lf_regions <- function(df, grid, res = grid$res) {
	r <- res
	snap <- function(from, to, edge0) {
		small <- (to - from) < r
		lo <- edge0 + floor(((from + to) / 2 - edge0) / r) * r
		list(from = ifelse(small, lo, from), to = ifelse(small, lo + r, to))
	}
	x <- snap(df$lon_from, df$lon_to, grid$lon[1])
	y <- snap(df$lat_from, df$lat_to, grid$lat[1])
	df$lon_from <- x$from; df$lon_to <- x$to
	df$lat_from <- y$from; df$lat_to <- y$to
	df
}
