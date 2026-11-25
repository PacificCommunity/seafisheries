# ==============================================================================
# Fisheries assignment for longline records
#
# The fisheries are the clusters of a longlineTypology run
# (cluster_diagnostics.RDS). Records with every clustering variable get the
# nearest k-means centroid (apply_clusters()). Records missing some variables
# (no valid HBF, raised records) are assigned by discriminant analysis on the
# variables they have (fit_assignment(), assign_records()): Mahalanobis
# distance to each fishery's centroid with the pooled within-fishery
# covariance, restricted to the observed variables, plus an optional prior.
# check_assignment() and check_strata_assignment() measure how well this
# works on records whose fishery is known.
# ==============================================================================


#' Variables used to assign longline records to fisheries
#'
#' Recomputes the species fractions of a clustering scenario with
#' [longlineTypology::recalcSpeciesFractions()] (e.g. with the `sp` token,
#' skipjack is counted as "other") and the absolute latitude. Fractions are
#' set to NA for records without any catch in numbers (`total_n` 0 or NA):
#' such records cannot be placed in the species composition space.
#'
#' @param df A dataframe with the `*_n` catch columns used by the scenario.
#' @param scenario Clustering scenario, e.g. `cluster_diag$scenario`.
#' @param lat_col Character; latitude used for `latAbs`, the cell centre
#'   (e.g. "latitude" for monthly operational records at 1 degree, "latCent"
#'   for raised records at 5 degrees).
#'
#' @return `df` with `total_n`, the scenario's `*_fraction` columns and
#'   `latAbs` (re)computed.
#'
#' @family fisheries assignment
#' @export
assignment_vars <- function(df, scenario, lat_col = "latitude") {
	.check_cols_exist(df, lat_col, "assignment_vars")
	df <- longlineTypology::recalcSpeciesFractions(as.data.frame(df), scenario)
	frac <- grep("_fraction$", longlineTypology::selectScenario(scenario), value = TRUE)
	no_catch <- is.na(df$total_n) | df$total_n <= 0
	for (v in frac) df[[v]][no_catch] <- NA_real_
	df$latAbs <- abs(df[[lat_col]])
	df
}


#' Assign records to the clusters of a longlineTypology run
#'
#' Scales the clustering variables with the saved `scale_params`, projects
#' them on the saved PCA and gives each record the nearest saved k-means
#' centroid, then converts the raw kmeans label to the saved cluster numbering
#' (`longlineTypology::clusterLabels()`). Only the fitted objects of
#' `cluster_diag` are used: `df` can be any dataset with the clustering
#' variables, whatever its number of rows. Records missing a clustering
#' variable, or with an invalid HBF when HBF is used, get NA.
#'
#' The variables must be built as in the data the clustering was fitted on,
#' not only named the same (see [assignment_vars()]). As a check, the
#' complete records of the fit years are scaled with the saved parameters:
#' each variable should have a mean near 0 and a sd near 1. Departures are
#' returned as the `"input_check"` attribute and raise a warning.
#'
#' @param df A dataframe with the clustering variables (`cluster_diag$pca_select`).
#' @param cluster_diag The longlineTypology diagnostics list
#'   (cluster_diagnostics.RDS), or the path to it.
#' @param k Number of clusters (a K saved in `cluster_diag$kmeans_full`).
#' @param merge Optional named list of clusters to merge, in the saved
#'   numbering: `list("3" = c(3, 5))` merges cluster 5 into 3.
#' @param renumber Logical; after merging, renumber the clusters 1, 2, ...
#'   without gaps (in the saved order). Default TRUE.
#' @param date_col Character; date column used to select the fit years for
#'   the input check. Default "ymd".
#' @param check_tol Numeric; the input check warns when a scaled variable has
#'   |mean| > `check_tol` or a sd outside `[1 / (1 + check_tol), 1 + check_tol]`.
#'   Default 0.5.
#'
#' @return Integer vector of cluster numbers, length `nrow(df)`, NA for
#'   incomplete records. Attributes: `"label_map"` (saved number, merged and
#'   renumbered number) and `"input_check"` (data.frame).
#'
#' @family fisheries assignment
#' @export
apply_clusters <- function(df, cluster_diag, k, merge = NULL, renumber = TRUE,
						   date_col = "ymd", check_tol = 0.5) {
	diag <- if (is.character(cluster_diag)) readRDS(cluster_diag) else cluster_diag
	need <- c("scenario", "pca_select", "scale_params", "pca_res", "n_components",
			  "kmeans_full", "cluster_reorder")
	miss <- setdiff(need, names(diag))
	if (length(miss)) stop("cluster_diag is missing: ", paste(miss, collapse = ", "))
	k <- as.character(k)
	if (!k %in% names(diag$kmeans_full))
		stop("No K = ", k, " in cluster_diag; available: ", paste(names(diag$kmeans_full), collapse = ", "))

	vars <- diag$pca_select
	.check_cols_exist(df, vars, "apply_clusters")
	X <- as.data.frame(df)[, vars, drop = FALSE]
	complete <- stats::complete.cases(X)
	if (any(c("mean_hbf", "hbf_cat") %in% vars) && "hbf_outlier" %in% names(df))
		complete <- complete & df$hbf_outlier %in% "valid"

	Z   <- longlineTypology::applyScaleParams(X[complete, , drop = FALSE], diag$scale_params)
	pcs <- stats::predict(diag$pca_res, newdata = Z)[, seq_len(diag$n_components), drop = FALSE]
	C   <- as.matrix(diag$kmeans_full[[k]]$centers)[, colnames(pcs), drop = FALSE]
	d2  <- outer(rowSums(pcs^2), rowSums(C^2), "+") - 2 * pcs %*% t(C)
	raw <- max.col(-d2, ties.method = "first")
	lab <- longlineTypology::clusterLabels(diag, k, raw = raw)

	label_map <- .merge_map(as.integer(k), merge, renumber)
	out <- rep(NA_integer_, nrow(df))
	out[complete] <- label_map$cluster[match(lab, label_map$saved)]

	# input check: complete records of the fit years, scaled with the saved parameters
	chk <- NULL
	if (date_col %in% names(df) && !is.null(diag$fit_years)) {
		yr <- as.integer(format(df[[date_col]][complete], "%Y"))
		fit <- yr >= diag$fit_years[1] & yr <= diag$fit_years[2]
		if (any(fit)) {
			Zf <- Z[fit, vars, drop = FALSE]
			chk <- data.frame(variable = vars, n = sum(fit),
							  mean_scaled = round(colMeans(Zf), 3),
							  sd_scaled = round(apply(Zf, 2, stats::sd), 3), row.names = NULL)
			chk$ok <- abs(chk$mean_scaled) <= check_tol &
				chk$sd_scaled <= 1 + check_tol & chk$sd_scaled >= 1 / (1 + check_tol)
			if (!all(chk$ok))
				warning("apply_clusters(): ", paste(chk$variable[!chk$ok], collapse = ", "),
						" differ from the data the clustering was fitted on (scaled mean or sd); ",
						"check that they are built the same way.", call. = FALSE)
		}
	}
	attr(out, "label_map") <- label_map
	attr(out, "input_check") <- chk
	out
}

# saved cluster number -> number after merging (and renumbering)
.merge_map <- function(k, merge, renumber) {
	m <- data.frame(saved = seq_len(k), merged = seq_len(k))
	if (!is.null(merge)) {
		for (into in names(merge)) {
			from <- as.integer(merge[[into]])
			bad <- setdiff(c(from, as.integer(into)), seq_len(k))
			if (length(bad)) stop("merge: cluster(s) ", paste(bad, collapse = ", "), " not in 1..", k)
			m$merged[m$saved %in% from] <- as.integer(into)
		}
	}
	m$cluster <- if (renumber) match(m$merged, sort(unique(m$merged))) else m$merged
	m
}


#' Fit the model assigning records with missing variables to fisheries
#'
#' Computes, from records whose fishery is known (typically the complete
#' records labelled by [apply_clusters()]), each fishery's centroid and the
#' pooled within-fishery covariance of `vars`, after z-scoring, and the
#' fishery counts used as prior. [assign_records()] then assigns any record
#' from the variables it has.
#'
#' Prior: `"equal"` (none), `"global"` (fishery proportions) or `"tiered"`:
#' fishery proportions in the record's group, for the first tier of
#' `prior_tiers` whose group has at least `prior_min_n` reference records,
#' else the global proportions. Counts are smoothed (+0.5) so that no fishery
#' has a zero prior. A `"year"` grouping column is built from `date_col` if
#' absent, as is `"decade"`.
#'
#' @param df A dataframe with `vars` (and the prior grouping columns).
#' @param cl Fishery of each row of `df` (NA rows are ignored).
#' @param vars Character; assignment variables, e.g. `cluster_diag$pca_select`.
#' @param prior One of "tiered", "global", "equal". Default "tiered".
#' @param prior_tiers List of character vectors, finest first. Default
#'   `list(c("flag", "year"), "flag")`.
#' @param prior_min_n Minimum number of reference records for a group prior.
#'   Default 30.
#' @param scenario Clustering scenario, stored for [check_strata_assignment()].
#' @param date_col Character; date column. Default "ymd".
#'
#' @return An object of class `assignment_fit`.
#'
#' @family fisheries assignment
#' @export
fit_assignment <- function(df, cl, vars, prior = c("tiered", "global", "equal"),
						   prior_tiers = list(c("flag", "year"), "flag"), prior_min_n = 30,
						   scenario = NULL, date_col = "ymd") {
	prior <- match.arg(prior)
	if (length(cl) != nrow(df)) stop("cl must have one value per row of df")
	.check_cols_exist(df, vars, "fit_assignment")
	X   <- as.matrix(as.data.frame(df)[, vars, drop = FALSE])
	use <- stats::complete.cases(X) & !is.na(cl)
	if (sum(use) < 2) stop("fit_assignment(): fewer than 2 complete labelled records")

	labels <- sort(unique(cl[use]))
	g   <- match(cl[use], labels)
	K   <- length(labels)
	X   <- X[use, , drop = FALSE]
	ctr <- colMeans(X)
	scl <- apply(X, 2, stats::sd)
	scl[scl == 0 | is.na(scl)] <- 1
	Z   <- sweep(sweep(X, 2, ctr), 2, scl, "/")
	n   <- tabulate(g, K)
	mu  <- rowsum(Z, g, reorder = TRUE) / n
	R   <- Z - mu[g, , drop = FALSE]

	tier_counts <- NULL
	if (prior == "tiered") {
		ref <- .add_period_cols(as.data.frame(df)[use, , drop = FALSE], date_col)
		tier_counts <- lapply(prior_tiers, function(cols) {
			.check_cols_exist(ref, cols, "fit_assignment (prior_tiers)")
			key <- do.call(paste, c(ref[cols], sep = "|"))
			tab <- table(factor(key), factor(g, levels = seq_len(K)))
			matrix(as.numeric(tab), nrow = nrow(tab), dimnames = list(rownames(tab), NULL))
		})
		names(tier_counts) <- vapply(prior_tiers, paste, character(1), collapse = " x ")
	}

	structure(list(vars = vars, ctr = ctr, scl = scl, mu = mu,
				   W = crossprod(R) / (nrow(Z) - K), n = n, K = K, labels = labels,
				   prior = prior, prior_tiers = prior_tiers, prior_min_n = prior_min_n,
				   tier_counts = tier_counts, scenario = scenario, date_col = date_col,
				   n_reference = sum(use)),
			  class = "assignment_fit")
}

#' @export
print.assignment_fit <- function(x, ...) {
	cat("Fisheries assignment model:", x$K, "fisheries,", x$n_reference, "reference records\n")
	cat("Variables:", paste(x$vars, collapse = ", "), "\n")
	cat("Prior:", x$prior, if (x$prior == "tiered")
		paste0("(", paste(names(x$tier_counts), collapse = " > "), " > global, min n = ", x$prior_min_n, ")"), "\n")
	invisible(x)
}


#' Assign records to fisheries from the variables they have
#'
#' For each record, uses only the variables of `fit` that are present and
#' not NA (a missing column counts as missing for every record): Mahalanobis
#' distance (`method = "lda"`, pooled within-fishery covariance restricted to
#' those variables) or Euclidean distance in z space (`"centroid"`) to each
#' fishery centroid, minus twice the log prior. With Gaussian fisheries of
#' common covariance, using the observed variables only is the exact
#' marginal: no value is imputed for the missing ones.
#'
#' The covariance of the species fractions is singular when they sum to 1;
#' it is inverted on its non-null subspace (pseudo-inverse), which gives the
#' same distance as dropping one fraction.
#'
#' @param fit An `assignment_fit` from [fit_assignment()].
#' @param df A dataframe (with the prior grouping columns if the prior is tiered).
#' @param method "lda" (default) or "centroid".
#' @param prior Prior to use; default the one of `fit`. "tiered" needs a
#'   tiered `fit`.
#'
#' @return A data.frame, one row per row of `df`: `cluster` (fishery label),
#'   `prob` (posterior probability of that fishery; meaningful for "lda"),
#'   `margin` (half the score gap to the runner-up; small = near a boundary),
#'   `n_vars` (variables used), `prior_tier` (prior used). Records without
#'   any variable get NA.
#'
#' @family fisheries assignment
#' @export
assign_records <- function(fit, df, method = c("lda", "centroid"), prior = fit$prior) {
	method <- match.arg(method)
	prior  <- match.arg(prior, c("tiered", "global", "equal"))
	if (prior == "tiered" && is.null(fit$tier_counts))
		stop("assign_records(): fit has no tiered prior; refit with prior = 'tiered'")
	df <- as.data.frame(df)
	nr <- nrow(df); p <- length(fit$vars); K <- fit$K

	X <- matrix(NA_real_, nr, p)
	for (j in seq_len(p)) if (fit$vars[j] %in% names(df)) X[, j] <- as.numeric(df[[fit$vars[j]]])
	Z <- sweep(sweep(X, 2, fit$ctr), 2, fit$scl, "/")
	obs <- !is.na(Z)
	pattern <- as.vector(obs %*% 2^(seq_len(p) - 1))

	d2 <- matrix(NA_real_, nr, K)
	for (pt in setdiff(unique(pattern), 0)) {
		i <- which(pattern == pt)
		o <- which(bitwAnd(pt, 2^(seq_len(p) - 1)) > 0)
		Zo <- Z[i, o, drop = FALSE]; Mo <- fit$mu[, o, drop = FALSE]
		if (method == "lda") {
			Wi <- .pinv(fit$W[o, o, drop = FALSE])
			A  <- Zo %*% Wi
			d2[i, ] <- outer(rowSums(A * Zo), rowSums((Mo %*% Wi) * Mo), "+") - 2 * tcrossprod(A, Mo)
		} else {
			d2[i, ] <- outer(rowSums(Zo^2), rowSums(Mo^2), "+") - 2 * tcrossprod(Zo, Mo)
		}
	}
	d2 <- pmax(d2, 0)

	lp <- .log_prior(fit, df, prior)
	score <- d2 - 2 * lp$logp
	ok <- pattern > 0
	out <- data.frame(cluster = rep(NA, nr), prob = NA_real_, margin = NA_real_,
					  n_vars = rowSums(obs), prior_tier = ifelse(ok, lp$tier, NA_character_))
	if (any(ok)) {
		s  <- score[ok, , drop = FALSE]
		kk <- max.col(-s, ties.method = "first")
		best <- s[cbind(seq_along(kk), kk)]
		post <- exp(-(s - best) / 2)
		out$cluster[ok] <- fit$labels[kk]
		out$prob[ok] <- 1 / rowSums(post)
		if (K > 1) {
			s2 <- s; s2[cbind(seq_along(kk), kk)] <- Inf
			out$margin[ok] <- 0.5 * (apply(s2, 1, min) - best)
		} else out$margin[ok] <- Inf
	}
	out
}

# pseudo-inverse of a symmetric matrix, on its non-null subspace
.pinv <- function(A, tol = 1e-8) {
	e <- eigen(A, symmetric = TRUE)
	keep <- e$values > tol * max(e$values)
	e$vectors[, keep, drop = FALSE] %*% (t(e$vectors[, keep, drop = FALSE]) / e$values[keep])
}

.add_period_cols <- function(df, date_col) {
	if (!"year" %in% names(df) && date_col %in% names(df))
		df$year <- as.integer(format(df[[date_col]], "%Y"))
	if (!"decade" %in% names(df) && "year" %in% names(df))
		df$decade <- 10L * (df$year %/% 10L)
	df
}

# log prior per record and fishery, and the tier it comes from
.log_prior <- function(fit, df, prior) {
	nr <- nrow(df); K <- fit$K
	if (prior == "equal") return(list(logp = matrix(0, nr, K), tier = rep("equal", nr)))
	glob <- log((fit$n + 0.5) / (sum(fit$n) + 0.5 * K))
	logp <- matrix(glob, nr, K, byrow = TRUE)
	tier <- rep("global", nr)
	if (prior == "tiered") {
		df <- .add_period_cols(df, fit$date_col)
		# coarsest tier first, finer tiers overwrite where they have enough records
		for (t in rev(seq_along(fit$prior_tiers))) {
			cols <- fit$prior_tiers[[t]]
			if (!all(cols %in% names(df))) next
			counts <- fit$tier_counts[[t]]
			m   <- match(do.call(paste, c(df[cols], sep = "|")), rownames(counts))
			tot <- rowSums(counts)[m]
			use <- !is.na(m) & tot >= fit$prior_min_n
			if (any(use)) {
				logp[use, ] <- log((counts[m[use], , drop = FALSE] + 0.5) / (tot[use] + 0.5 * K))
				tier[use] <- names(fit$tier_counts)[t]
			}
		}
	}
	list(logp = logp, tier = tier)
}


#' Check the assignment on records whose fishery is known
#'
#' Assigns `df` with [assign_records()] after setting the `hide` variables to
#' NA, and compares with the known fishery `cl`. `hide = NULL` measures the
#' agreement between the discriminant analysis and the k-means assignment
#' with every variable; `hide = "mean_hbf"` measures how well records without
#' HBF are assigned. In-sample when `df` holds the reference records of `fit`.
#'
#' @param fit An `assignment_fit`.
#' @param df A dataframe whose rows have a known fishery.
#' @param cl Known fishery of each row.
#' @param hide Character; variables set to NA before assigning.
#' @param weight_col Character; column used as weight (e.g. effort "E"), or
#'   NULL for record counts.
#' @param by Character; optional grouping column(s) for the accuracy
#'   (e.g. "flag", "decade").
#' @param method,prior Passed to [assign_records()].
#' @param prob_breaks Classes of assignment probability for the calibration
#'   table.
#'
#' @return A list: `accuracy` (weighted share correctly assigned), `by_cluster`
#'   (recall, precision and weight share per fishery), `confusion` (known in
#'   rows, assigned in columns, shares of the total weight), `calibration`
#'   (per class of assignment probability: weight share, mean probability and
#'   accuracy; if the probability is reliable, accuracy is close to the mean
#'   probability), `by_group` (if `by`), and the settings.
#'
#' @family fisheries assignment
#' @export
check_assignment <- function(fit, df, cl, hide = NULL, weight_col = NULL, by = NULL,
							 method = "lda", prior = fit$prior,
							 prob_breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1)) {
	if (length(cl) != nrow(df)) stop("cl must have one value per row of df")
	df <- as.data.frame(df)
	d2 <- df
	for (v in hide) d2[[v]] <- NA_real_
	a <- assign_records(fit, d2, method = method, prior = prior)
	w <- if (is.null(weight_col)) rep(1, nrow(df)) else df[[weight_col]]
	res <- .compare_assignment(cl, a$cluster, w, fit$labels)
	ok  <- !is.na(a$cluster) & !is.na(cl)
	band <- cut(a$prob[ok], prob_breaks, include.lowest = TRUE)
	wo <- w[ok]; hit_o <- a$cluster[ok] == cl[ok]
	res$calibration <- data.frame(prob = levels(band),
		weight_share = round(as.vector(tapply(wo, band, sum)) / sum(wo), 4),
		mean_prob = round(as.vector(tapply(wo * a$prob[ok], band, sum) / tapply(wo, band, sum)), 4),
		accuracy = round(as.vector(tapply(wo * hit_o, band, sum) / tapply(wo, band, sum)), 4))
	if (!is.null(by)) {
		g   <- do.call(paste, c(.add_period_cols(df, fit$date_col)[by], sep = " | "))
		ok  <- !is.na(a$cluster) & !is.na(cl)
		hit <- ok & a$cluster == cl
		wt  <- tapply(w * ok, g, sum)
		res$by_group <- data.frame(group = names(wt),
								   weight_share = round(as.vector(wt) / sum(wt), 4),
								   accuracy = round(as.vector(tapply(w * hit, g, sum) / wt), 4))
		names(res$by_group)[1] <- paste(by, collapse = " | ")
	}
	res$hide <- hide; res$method <- method; res$prior <- prior
	res
}

.compare_assignment <- function(true, pred, w, labels) {
	ok  <- !is.na(true) & !is.na(pred)
	hit <- ok & pred == true
	f <- function(x) factor(x, levels = labels)
	tw <- tapply(w * ok, f(true), sum); tw[is.na(tw)] <- 0
	pw <- tapply(w * ok, f(pred), sum); pw[is.na(pw)] <- 0
	hw <- tapply(w * hit, f(true), sum); hw[is.na(hw)] <- 0
	list(accuracy = sum(w[hit]) / sum(w[ok]),
		 by_cluster = data.frame(cluster = labels,
		 						weight_share = round(as.vector(tw) / sum(w[ok]), 4),
		 						recall = round(as.vector(hw / tw), 4),
		 						precision = round(as.vector(hw / pw), 4)),
		 confusion = round(stats::xtabs(w[ok] ~ f(true[ok]) + f(pred[ok])) / sum(w[ok]), 4))
}


#' Check the assignment of aggregated (raised-like) records
#'
#' Aggregates records whose fishery is known to raised strata (flag x month
#' x `res` degree cell: catch numbers and effort summed), computes the
#' assignment variables of the aggregates ([assignment_vars()], cell centre
#' latitude) and assigns them with `fit`. The reference fishery of a stratum
#' is the one with most effort, and `purity` its effort share: a stratum of
#' purity 0.5 is half another fishery, and no single assignment can be right
#' for it. This is the raised situation: missing variables and mixing.
#'
#' @param fit An `assignment_fit` with a `scenario`.
#' @param df Records with `latitude`, `longitude`, the date, flag, effort and
#'   the `*_n` catch columns.
#' @param cl Fishery of each row of `df`.
#' @param res Cell size in degrees. Default 5.
#' @param flag_col,effort_col,date_col Column names.
#' @param method,prior Passed to [assign_records()].
#' @param purity_breaks Purity classes for the accuracy breakdown.
#'
#' @return A list as [check_assignment()], plus `by_purity` and `strata`
#'   (the aggregates with `cluster`, `purity`, `assigned`, `prob`).
#'
#' @family fisheries assignment
#' @export
check_strata_assignment <- function(fit, df, cl, res = 5, flag_col = "flag",
									effort_col = "E", date_col = "ymd",
									method = "lda", prior = fit$prior,
									purity_breaks = c(0, 0.5, 0.75, 0.9, 1)) {
	if (is.null(fit$scenario)) stop("fit has no scenario: refit with fit_assignment(scenario = )")
	df <- as.data.frame(df)
	ok <- !is.na(cl)
	df <- df[ok, , drop = FALSE]; cl <- cl[ok]
	g_cl <- match(cl, fit$labels)
	n_cols <- intersect(paste0(c("yft", "bet", "alb", "skj", "oth"), "_n"), names(df))

	lat <- floor(df$latitude / res) * res + res / 2
	lon <- floor(df$longitude / res) * res + res / 2
	mon <- format(df[[date_col]], "%Y-%m")
	key <- paste(df[[flag_col]], mon, lat, lon, sep = "|")
	u <- unique(key); g <- match(key, u)
	E <- df[[effort_col]]
	s <- rowsum(cbind(as.matrix(df[, n_cols, drop = FALSE]), E = E), g, reorder = TRUE)
	Ek <- rowsum(outer(E, seq_len(fit$K), "*") * outer(g_cl, seq_len(fit$K), "=="), g, reorder = TRUE)
	i <- match(seq_along(u), g)
	agg <- data.frame(flag = df[[flag_col]][i], ymd = as.Date(paste0(mon[i], "-15")),
					  latitude = lat[i], longitude = lon[i], s,
					  cluster = fit$labels[max.col(Ek, ties.method = "first")],
					  purity = apply(Ek, 1, max) / rowSums(Ek),
					  row.names = NULL, stringsAsFactors = FALSE)
	names(agg)[names(agg) == flag_col] <- "flag"
	agg <- assignment_vars(agg, fit$scenario, lat_col = "latitude")

	a <- assign_records(fit, agg, method = method, prior = prior)
	agg$assigned <- a$cluster; agg$prob <- a$prob
	out <- .compare_assignment(agg$cluster, agg$assigned, agg$E, fit$labels)
	band <- cut(agg$purity, purity_breaks, include.lowest = TRUE)
	hit <- !is.na(agg$assigned) & agg$assigned == agg$cluster
	out$by_purity <- data.frame(purity = levels(band),
								accuracy = as.vector(tapply(agg$E * hit, band, sum) / tapply(agg$E, band, sum)),
								effort_share = as.vector(tapply(agg$E, band, sum) / sum(agg$E)))
	out$strata <- agg
	out$method <- method; out$prior <- prior
	out
}
