#' @import ggplot2
NULL

#' Custom ggplot2 theme for fisheries maps and plots
#'
#' @description
#' A clean `theme_bw()`-based theme with sensible defaults for map figures:
#' legend at the bottom, wide legend keys, and blank facet strip backgrounds.
#'
#' @param text_size Numeric; base text size in points. Default is 11.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   customTheme()
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   customTheme(text_size = 14)
#'
#' @export
customTheme <- function(text_size = 11) {
	theme_bw() +
		theme(
			legend.position    = "bottom",
			legend.key.width   = unit(2.3, "cm"),
			text               = element_text(size = text_size),
			strip.background   = element_blank()
		)
}


#' World map background layer for Pacific-centered ggplot2 maps
#'
#' @description
#' Returns a `geom_polygon` layer with a world map background suitable for
#' Pacific-centered maps using the 0-360 longitude convention. The map is
#' duplicated and shifted so that landmasses crossing the antimeridian
#' (e.g. Russia, USA) render correctly.
#'
#' @param alpha Numeric; transparency of the land fill, between 0 and 1.
#'   Default is 0.3.
#'
#' @return A ggplot2 `geom_polygon` layer.
#'
#' @details
#' Requires the \pkg{maps} package to be installed. The function uses the
#' 0-360 longitude convention throughout, consistent with the rest of this
#' package. Do not mix with plots using the -180/180 convention.
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   map_bg() +
#'   coord_fixed(xlim = c(89.5, 288.5), ylim = c(-54.5, 64.5))
#'
#' # Adjust transparency
#' ggplot() +
#'   map_bg(alpha = 0.5) +
#'   coord_fixed()
#'
#' @export
map_bg <- function(alpha = 0.3) {
	raw   <- map_data("world")
	shifted <- raw |>
		dplyr::mutate(
			long  = long + 360,
			group = group + max(group) + 1
		)
	bg <- rbind(raw, shifted)
	geom_polygon(
		data   = bg,
		aes(long, lat, group = group),
		colour = "black",
		fill   = "#EFEFDB",
		alpha  = alpha
	)
}


#' Format latitude values as degree strings with cardinal direction
#'
#' @description
#' Converts numeric latitude values to character strings with a degree symbol
#' and N/S cardinal direction. Intended for use with ggplot2 axis labels.
#'
#' @param x Numeric vector of latitude values in decimal degrees.
#'   Negative values are treated as South.
#'
#' @return Character vector of formatted labels (e.g. `"45°N"`, `"30°S"`).
#'
#' @examples
#' lat_format(c(-45, 0, 30))
#' # [1] "45°S" "0°N"  "30°N"
#'
#' @export
lat_format <- function(x) {
	ifelse(x < 0, paste0(abs(x), "\u00b0S"), paste0(x, "\u00b0N"))
}


#' Format longitude values as degree strings with cardinal direction
#'
#' @description
#' Converts numeric longitude values in the 0-360 convention to character
#' strings with a degree symbol and E/W cardinal direction. Values <= 180
#' are labelled East; values > 180 are converted and labelled West.
#'
#' @param x Numeric vector of longitude values in the 0-360 convention.
#'
#' @return Character vector of formatted labels (e.g. `"170°E"`, `"120°W"`).
#'
#' @examples
#' lon_format(c(90, 180, 240))
#' # [1] "90°E"  "180°E" "120°W"
#'
#' @export
lon_format <- function(x) {
	ifelse(x <= 180, paste0(x, "\u00b0E"), paste0(360 - x, "\u00b0W"))
}


#' ggplot2 coordinate axis scales with Pacific-convention labels
#'
#' @description
#' Returns a list of two ggplot2 scale layers that format x (longitude) and
#' y (latitude) axis labels using the 0-360 longitude convention. Drop this
#' list into any ggplot2 call the same way you would add a scale manually.
#'
#' @return A list of two ggplot2 scale objects:
#'   \itemize{
#'     \item \code{scale_x_continuous} using \code{\link{lon_format}}
#'     \item \code{scale_y_continuous} using \code{\link{lat_format}}
#'   }
#'
#' @examples
#' library(ggplot2)
#' ggplot() +
#'   map_bg() +
#'   coord_labels() +
#'   coord_fixed(xlim = c(89.5, 288.5), ylim = c(-54.5, 64.5))
#'
#' @export
coord_labels <- function() {
	list(
		scale_y_continuous("", labels = lat_format),
		scale_x_continuous("", labels = lon_format)
	)
}

#' Plot a gridded fisheries map
#'
#' Wraps map_bg(), coord_labels(), coord_fixed(), scale_fill_viridis_c(), and
#' customTheme() into one call.
#'
#' Tile sizing assumes a uniform grid (fine for EC_clean's 1x1 grid). LF data
#' isn't uniform -- cell size varies by ASTRAT (2.5 to 10 degrees) -- so a
#' single tile_width/tile_height is only an approximation there. Facet by
#' ASTRAT if you need visually accurate cell sizes for LF.
#'
#' @param df Data frame containing the data to plot.
#' @param fill_col Character. Name of the column in `df` to map to tile fill.
#' @param lat_col Character. Name of the latitude column. Default `"latCent"`.
#' @param lon_col Character. Name of the longitude column. Default `"lonCent"`.
#' @param facet_col Character or NULL. Column to facet by. Default NULL (no facetting).
#' @param fill_label Character. Legend title for the fill scale. Defaults to `fill_col`.
#' @param filter_positive Logical. If TRUE, drop rows where `fill_col` is NA or <= 0. Default TRUE.
#' @param tile_width Numeric. Tile width in degrees. Default 1.
#' @param tile_height Numeric. Tile height in degrees. Default 1.
#'
#' @details
#' Requires the global objects `lon_range` and `lat_range` to be defined in
#' the environment (used to set `coord_fixed()` limits).
plot_fisheries_map <- function(df, fill_col, lat_col = "latCent", lon_col = "lonCent",
							   facet_col = NULL, fill_label = fill_col,
							   filter_positive = TRUE, tile_width = 1, tile_height = 1) {
	if (filter_positive) {
		keep <- !is.na(df[[fill_col]]) & df[[fill_col]] > 0
		df <- df[keep, ]
	}

	p <- ggplot(df) +
		geom_tile(aes(x = .data[[lon_col]], y = .data[[lat_col]], fill = .data[[fill_col]]),
				  width = tile_width, height = tile_height) +
		map_bg() +
		coord_labels() +
		coord_fixed(xlim = lon_range, ylim = lat_range) +
		scale_fill_viridis_c(name = fill_label) +
		customTheme()

	if (!is.null(facet_col)) {
		p <- p + facet_wrap(vars(.data[[facet_col]]))
	}

	p
}

#' Plot length-frequency density by fishery
#'
#' Requires `df` to contain columns `len`, `count`, and `f`, these column
#' names are hardcoded and not configurable.
#'
#' @param df Data frame with columns `len` (length), `count` (frequency weight),
#'   and `f` (fishery code to facet by).
plot_lf_density <- function(df) {
	ggplot(df, aes(x = len, weight = count)) +
		geom_density(fill = "steelblue", alpha = 0.4) +
		facet_wrap(~ f) +
		labs(
			x = "Length",
			y = "Density",
			title = paste("Length frequency by fishery")
		) +
		theme_minimal()
}
