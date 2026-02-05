#' Pacific Ocean boundaries (0-360 longitude system)
#'
#' @description Default spatial boundaries for filtering fisheries data.
#' Users can override these in their scripts.
#'
#' @format Numeric vectors of length 2: c(min, max)
#' @examples
#' # Use defaults
#' data %>% filter(lon >= lon_range[1], lon <= lon_range[2])
#'
#' # Override for specific region
#' lon_range <- c(120, 200)
#' @export
lon_range <- c(89.5, 288.5)

#' @rdname lon_range
#' @export
lat_range <- c(-54.5, 64.5)
