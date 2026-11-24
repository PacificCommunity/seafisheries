# ==============================================================================
# Diagnostics helpers shared by the workflow documents.
# Sourced at the top of each qmd: creates a fresh .diag environment.
# The document saves list(steps = stepsTable(), messages = .diag$messages,
# warnings = .diag$warnings, tables = .diag$tables, ...) to an RDS that
# seafisheries::processing_report() turns into a PDF.
# ==============================================================================

# ---- Diagnostics -------------------------------------------------------------
# Everything is collected in .diag, saved by the document to an RDS read by
# seafisheries::processing_report():
#   steps     one row per cleaning step: dataset, step, rows in, rows out
#   messages  console output of each step (what the package functions print)
#   warnings  warnings raised by the steps and by the checks of this document
#   tables    stage comparisons, coordinate conversion summaries, catch quality,
#             length matching coverage, ...
# Dataset names are keys: tables named stage_<dataset>, catch_quality_<dataset>
# and coords_<dataset>... are shown with their dataset in the report.
.diag <- new.env()
.diag$steps <- list(); .diag$messages <- list(); .diag$warnings <- list(); .diag$tables <- list()

diagAdd <- function(slot, name, value) {
	x <- .diag[[slot]]; x[[name]] <- value; .diag[[slot]] <- x
	invisible(value)
}

# Records a warning in the diagnostics and raises it.
diagWarn <- function(where, ...) {
	msg <- paste0(...)
	.diag$warnings[[where]] <- c(.diag$warnings[[where]], msg)
	warning(msg, call. = FALSE)
}

# Runs fun(df, ...), records rows in/out, the printed output and the warnings.
# x: a data.frame, or a list with $data. Returns what fun returns.
# Re-running a step (same dataset and step) replaces its previous record.
runStep <- function(x, dataset, step, fun, ...) {
	df_in <- if (is.data.frame(x)) x else x$data
	key <- paste(dataset, step, sep = " | ")
	out <- NULL
	warns <- character(0)
	msg <- withCallingHandlers(
		utils::capture.output(out <- fun(df_in, ...)),
		warning = function(w) warns <<- c(warns, conditionMessage(w)))
	df_out <- if (is.data.frame(out)) out else out$data
	.diag$steps[[key]] <- data.frame(dataset = dataset, step = step,
									 n_in = nrow(df_in), n_out = nrow(df_out))
	.diag$warnings[[key]] <- if (length(warns)) warns else NULL
	.diag$messages[[key]] <- if (length(msg)) msg else NULL
	if (verbose && length(msg)) cat(paste0("[", key, "]"), msg, sep = "\n")
	out
}

stepsTable <- function(ds = NULL) {
	if (!length(.diag$steps))
		return(data.frame(dataset = character(0), step = character(0), n_in = integer(0),
						  n_out = integer(0), removed = integer(0), pct_removed = numeric(0)))
	st <- do.call(rbind, unname(.diag$steps))
	if (!is.null(ds)) st <- st[st$dataset == ds, ]
	st$removed <- st$n_in - st$n_out
	st$pct_removed <- round(100 * st$removed / st$n_in, 2)
	st
}

showSteps <- function(ds) {
	st <- stepsTable(ds)
	knitr::kable(st[, -1], row.names = FALSE, caption = paste("Cleaning steps:", ds))
}
