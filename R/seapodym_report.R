#' Read a SEAPODYM catch-effort file
#'
#' Reads a file written by [format_EC()]: number of fisheries, number of
#' records per fishery, a header line, then one record per line.
#'
#' @param file Character; path to the catch-effort text file.
#'
#' @return A data frame with columns `f`, `yr`, `mm`, `dd`, `gr`, `lat`,
#'   `lon`, `res`, `E`, `C`. A warning is given if the record counts of the
#'   header do not match the records.
#'
#' @family seapodym formatting
#' @export
read_seapodym_EC <- function(file) {
	if (!file.exists(file)) stop("File not found: ", file)
	head <- readLines(file, n = 3)
	nb_f  <- as.integer(head[1])
	n_rec <- as.integer(strsplit(trimws(head[2]), "\\s+")[[1]])
	cols  <- strsplit(trimws(head[3]), "\\s+")[[1]]
	d <- utils::read.table(file, skip = 3, header = FALSE, col.names = cols,
						   colClasses = c(gr = "character"), stringsAsFactors = FALSE)
	d$f <- .fishery_number(d$f)
	if (length(n_rec) != nb_f || sum(n_rec) != nrow(d))
		warning(basename(file), ": header (", nb_f, " fisheries, ", sum(n_rec),
				" records) does not match the data (", length(unique(d$f)), " fisheries, ",
				nrow(d), " records)")
	d
}

#' Read a SEAPODYM length-frequency file
#'
#' Reads a file written by [format_LF()]: numbers of regions, fisheries and
#' records, the regions, the length bins, a header line, then one sample per
#' line (year, quarter, month, fishery, region, counts per length bin).
#'
#' @param file Character; path to the length-frequency text file.
#'
#' @return A list: `samples` (data frame `yr`, `qtr`, `mm`, `f`, `reg` and one
#'   column per length bin), `regions` (`reg`, `lon_from`, `lon_to`,
#'   `lat_from`, `lat_to`) and `bins` (lower length of each bin, cm).
#'
#' @family seapodym formatting
#' @export
read_seapodym_LF <- function(file) {
	if (!file.exists(file)) stop("File not found: ", file)
	n <- as.integer(strsplit(trimws(readLines(file, n = 1)), "\\s+")[[1]])
	nb_reg <- n[1]; nb_rec <- n[3]
	regions <- utils::read.table(file, skip = 1, nrows = nb_reg, header = FALSE,
								 col.names = c("reg", "lon_from", "lon_to", "lat_from", "lat_to"))
	b <- as.numeric(strsplit(trimws(readLines(file, n = nb_reg + 2)[nb_reg + 2]), "\\s+")[[1]])
	bins <- b[2] + (seq_len(b[1]) - 1) * b[3]
	samples <- utils::read.table(file, skip = nb_reg + 3, header = FALSE,
								 colClasses = c(rep("numeric", 3), "character", rep("numeric", 1 + b[1])),
								 col.names = c("yr", "qtr", "mm", "f", "reg", paste0("L", bins)))
	samples$f <- .fishery_number(samples$f)
	if (nrow(samples) != nb_rec)
		warning(basename(file), ": header gives ", nb_rec, " records, the file has ", nrow(samples))
	list(samples = samples, regions = regions, bins = bins)
}

# fishery number from a label written as a number or as gear group + number (L1, O15)
.fishery_number <- function(x) as.integer(gsub("\\D", "", as.character(x)))


#' PDF report of SEAPODYM fisheries input files
#'
#' Reads the catch-effort file (and optionally the length-frequency file)
#' written for SEAPODYM and plots, per fishery: a CPUE map (total catch /
#' total effort per cell; a catch map for fisheries without effort), the
#' annual catch and effort, and the length distribution. Works on any pair of
#' files in the SEAPODYM format, whatever the species or fisheries.
#'
#' Requires `rmarkdown` and a LaTeX installation.
#'
#' @param ec_file Character; catch-effort file ([format_EC()]).
#' @param lf_file Character; length-frequency file ([format_LF()]), or NULL.
#' @param output_file Character; PDF to write. Default: next to `ec_file`,
#'   with `_report.pdf` instead of `.txt`.
#' @param title Character; report title. Default from the file name.
#' @param years Numeric vector of length 2 to restrict the years, or NULL
#'   (default) for all years.
#' @param species Character; species name for the axis labels. Default "".
#' @param quiet Logical; passed to [rmarkdown::render()]. Default TRUE.
#'
#' @return The path of the PDF, invisibly.
#'
#' @family seapodym formatting
#' @export
seapodym_fisheries_report <- function(ec_file, lf_file = NULL, output_file = NULL, title = NULL,
									  years = NULL, species = "", quiet = TRUE) {
	if (!requireNamespace("rmarkdown", quietly = TRUE))
		stop("seapodym_fisheries_report() needs the 'rmarkdown' package.")
	ec <- read_seapodym_EC(ec_file)
	lf <- if (!is.null(lf_file)) read_seapodym_LF(lf_file) else NULL

	if (is.null(output_file)) output_file <- sub("\\.txt$", "_report.pdf", ec_file)
	if (identical(output_file, ec_file)) output_file <- paste0(ec_file, "_report.pdf")
	if (is.null(title)) title <- paste("SEAPODYM fisheries:", basename(ec_file))
	output_file <- normalizePath(output_file, mustWork = FALSE)

	template <- system.file("reports", "seapodym_fisheries_report.Rmd", package = "seafisheries")
	if (template == "") stop("Report template not found in the seafisheries installation.")
	work_dir <- tempfile("seapodym_report_")
	dir.create(work_dir)
	on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
	file.copy(template, work_dir)

	rmarkdown::render(file.path(work_dir, "seapodym_fisheries_report.Rmd"),
					  output_file = basename(output_file),
					  output_dir = dirname(output_file),
					  intermediates_dir = work_dir,
					  params = list(ec = ec, lf = lf, title = title, years = years, species = species,
					  			  ec_file = ec_file, lf_file = if (is.null(lf_file)) "" else lf_file),
					  envir = new.env(parent = globalenv()),
					  quiet = quiet)
	invisible(output_file)
}
