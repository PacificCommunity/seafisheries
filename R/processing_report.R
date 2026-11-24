#' PDF report of a data processing run
#'
#' Renders the diagnostics saved by a processing workflow (e.g.
#' `workflows/alb/longline.qmd`, file `LL_processing_diagnostics.RDS`) into a
#' PDF: records removed at each cleaning step and why, input vs processed
#' tables, catch and coordinate quality, length matching coverage, warnings,
#' and the console output of each step in an appendix.
#'
#' The report is generic: datasets are taken from `diag$steps`, and the tables
#' in `diag$tables` are attached to a dataset by name (`stage_<dataset>`,
#' `catch_quality_<dataset>`, `coords_<dataset>...`). Any other table is shown
#' in a "Other checks" section under its name.
#'
#' Requires `rmarkdown` and a LaTeX installation (e.g. `tinytex::install_tinytex()`).
#'
#' @param diag_file Character; path to the diagnostics RDS. Expected elements:
#'   `species`, `request_tag`, `rendered`, `steps` (data.frame with `dataset`,
#'   `step`, `n_in`, `n_out`), `messages` (named list of character vectors),
#'   `tables` (named list of data.frames) and optionally `warnings` (named list
#'   of character vectors).
#' @param output_file Character; path of the PDF to write. Default: same folder
#'   and name as `diag_file`, with extension `.pdf`.
#' @param title Character; report title. Default built from the species.
#' @param quiet Logical; passed to [rmarkdown::render()]. Default TRUE.
#'
#' @return The path of the PDF, invisibly.
#'
#' @examples
#' \dontrun{
#' processing_report("data-raw/processed/alb/LL_processing_diagnostics.RDS")
#' }
#'
#' @family diagnostics
#' @export
processing_report <- function(diag_file, output_file = NULL, title = NULL, quiet = TRUE) {
	if (!requireNamespace("rmarkdown", quietly = TRUE))
		stop("processing_report() needs the 'rmarkdown' package.")
	if (!file.exists(diag_file)) stop("File not found: ", diag_file)

	diag <- readRDS(diag_file)
	missing <- setdiff(c("steps", "messages", "tables"), names(diag))
	if (length(missing)) stop("Not a processing diagnostics file, missing: ",
							  paste(missing, collapse = ", "))

	if (is.null(output_file)) output_file <- sub("\\.[Rr][Dd][Ss]$", ".pdf", diag_file)
	if (is.null(title)) title <- paste(diag$species, "data processing diagnostics")
	output_file <- normalizePath(output_file, mustWork = FALSE)

	template <- system.file("reports", "processing_report.Rmd", package = "seafisheries")
	if (template == "") stop("Report template not found in the seafisheries installation.")

	# render from a temporary copy: the installed package folder may be read-only
	work_dir <- tempfile("processing_report_")
	dir.create(work_dir)
	on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
	file.copy(template, work_dir)

	rmarkdown::render(file.path(work_dir, "processing_report.Rmd"),
					  output_file = basename(output_file),
					  output_dir = dirname(output_file),
					  intermediates_dir = work_dir,
					  params = list(diag = diag, title = title, source_file = diag_file),
					  envir = new.env(parent = globalenv()),
					  quiet = quiet)
	invisible(output_file)
}

#' Short explanation of a processing step
#'
#' Matches a step name, as recorded by the processing workflows, to a one-line
#' reason why records are removed or changed. Used by [processing_report()].
#'
#' @param step Character vector of step names.
#'
#' @return Character vector of the same length; "" when no reason is known.
#'
#' @family diagnostics
#' @keywords internal
step_reason <- function(step) {
	reasons <- c(
		"activity_id"              = "Not a fishing set (activity other than 1).",
		"one row per"              = "Same record returned twice by the SQL joins (identical copies).",
		"convert coordinates"      = "Positions converted to decimal degrees. Unreadable values become NA and are removed by remove_invalid. No record removed here.",
		"remove_invalid"           = "Missing or non-finite value in a required column (date, position, effort, catch, length).",
		"remove_zero_fisheries"    = "No catch and no effort.",
		"remove_dummy_fisheries"   = "Catch with zero effort. Set aside and saved separately.",
		"treat_hbf"                = "Hooks between floats <= 0 or above the threshold set to NA. No record removed.",
		"trim_vesselname"          = "Trailing spaces removed from vessel names. No record removed.",
		"duplicates"               = "Exact duplicates (all columns except the record id).",
		"apply_pacific_mask"       = "Outside the model domain or on land.",
		"apply_temperature_mask"   = "Cell colder than the minimum temperature that month.",
		"sp_code|SP_ID"            = "Other species, gears or data sources.",
		"infer_len_code"           = "Missing length codes recovered from other fish of the same trip. No record removed.",
		"convert length codes"     = "Lengths measured as SD/US/PS converted to UF. No record removed.",
		"len_code"                 = "Length code still not UF after inference and conversion.",
		"bounding box|filter_bounding_box" = "Outside the latitude/longitude range of the model.",
		"length within"            = "Length outside the plausible range of the species (zeros, typos, other species): lf_len_range in the config.",
		"^Hampel filter$"          = "Outliers among neighbours of the same fishery: CPUE (space and time) for catch and effort, where effort is adjusted rather than records removed; mean length (neighbouring months) for lengths.",
		"process_lf_data"          = "Missing position or length, invalid strata, outside the domain, or samples with too few length bins. The row count also drops because rows of a sample with the same length are merged (fish kept)."
	)
	out <- rep("", length(step))
	for (p in names(reasons)) {
		hit <- out == "" & grepl(p, step)
		out[hit] <- reasons[[p]]
	}
	out
}
