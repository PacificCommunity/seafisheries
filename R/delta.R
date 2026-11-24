# ==============================================================================
# Delta: the part of the raised catch and effort missing from the operational
# (logbook) data, at the raised resolution (flag x 5 deg cell x month).
#
# reconcile_delta() computes it so that, per group (flag x year by default),
# operational + delta + dummy = raised exactly, for effort and each catch
# column. split_delta() gives the delta a fishery: the logbook fishery mix of
# the cell where the logbooks cover enough of it, the raised assignment
# otherwise.
# ==============================================================================


#' Delta between raised and operational data, reconciled per group
#'
#' For each variable (effort and each catch column) and each cell, the
#' difference raised - operational can be negative where the logbooks exceed
#' the raised data. Flooring it at 0 cell by cell adds catch: the excess of
#' those cells is never subtracted. Here, per group (flag x year by default):
#' \enumerate{
#'   \item the net missing amount is D = sum(raised) - sum(operational);
#'   \item if D > 0, the positive cell differences are scaled by
#'     D / sum(positive differences), so that they add up to D: negative
#'     cells reduce the delta of the other cells of the group;
#'   \item if D <= 0, the logbooks already exceed the raised total: no delta,
#'     and the excess is reported (`overshoot`). Operational data are never
#'     reduced.
#' }
#' Each variable is reconciled separately. Cells are then typed: `"delta"`
#' if their delta effort is above `thr_E`, `"dummy"` if they have a delta but
#' not enough effort to attach a catch rate to (kept apart, but part of the
#' totals), `"none"` otherwise. Per group and variable with D > 0,
#' operational + delta + dummy = raised.
#'
#' @param raised,operational Data frames at the same resolution, one row per
#'   cell (`key`), with `vars`. Missing cells count as 0.
#' @param key Character; columns identifying a cell.
#' @param vars Character; effort and catch columns to reconcile.
#' @param group Character; grouping columns for the reconciliation. `"year"`
#'   is built from `date_col` if absent.
#' @param effort_col Character; effort column (in `vars`).
#' @param thr_E Numeric; minimum delta effort of a `"delta"` cell.
#' @param date_col Character; date column (in `key`).
#'
#' @return A list: `cells` (one row per cell: `key`, the reconciled `vars`,
#'   `type`, `E_raised`, `E_operational` and `coverage` = operational /
#'   raised effort) and `groups` (one row per group and variable: raised,
#'   operational, net_missing, delta, dummy, overshoot).
#'
#' @family delta
#' @export
reconcile_delta <- function(raised, operational, key = c("flag", "latCent", "lonCent", "ymd"),
							vars, group = c("flag", "year"), effort_col = "E", thr_E = 1,
							date_col = "ymd") {
	.check_cols_exist(raised, c(key, vars), "reconcile_delta (raised)")
	.check_cols_exist(operational, c(key, vars), "reconcile_delta (operational)")
	if (!effort_col %in% vars) stop("effort_col must be one of vars")
	agg <- function(d) d |>
		dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
		dplyr::summarise(dplyr::across(dplyr::all_of(vars), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
	m <- dplyr::full_join(agg(raised), agg(operational), by = key, suffix = c("_rai", "_opr"))
	for (v in c(paste0(vars, "_rai"), paste0(vars, "_opr"))) m[[v]][is.na(m[[v]])] <- 0
	m <- as.data.frame(m)
	if ("year" %in% group && !"year" %in% names(m)) m$year <- as.integer(format(m[[date_col]], "%Y"))
	g <- do.call(paste, c(m[group], sep = "|"))

	cells <- m[, key, drop = FALSE]
	groups <- list()
	for (v in vars) {
		r <- m[[paste0(v, "_rai")]]; o <- m[[paste0(v, "_opr")]]
		d <- r - o
		D <- tapply(d, g, sum)
		P <- tapply(pmax(d, 0), g, sum)
		s <- ifelse(D > 0 & P > 0, D / P, 0)
		cells[[v]] <- pmax(d, 0) * s[g]
		groups[[v]] <- data.frame(group = names(D), variable = v,
								  raised = as.vector(tapply(r, g, sum)),
								  operational = as.vector(tapply(o, g, sum)),
								  net_missing = as.vector(D), stringsAsFactors = FALSE)
	}
	E <- cells[[effort_col]]
	any_pos <- rowSums(cells[, vars, drop = FALSE] > 0) > 0
	cells$type <- ifelse(E > thr_E, "delta", ifelse(any_pos, "dummy", "none"))
	cells$E_raised <- m[[paste0(effort_col, "_rai")]]
	cells$E_operational <- m[[paste0(effort_col, "_opr")]]
	cells$coverage <- ifelse(cells$E_raised > 0, cells$E_operational / cells$E_raised, NA_real_)

	groups <- do.call(rbind, groups)
	for (v in vars) {
		dv <- tapply(cells[[v]] * (cells$type == "delta"), g, sum)
		du <- tapply(cells[[v]] * (cells$type == "dummy"), g, sum)
		i <- groups$variable == v
		groups$delta[i] <- as.vector(dv[groups$group[i]])
		groups$dummy[i] <- as.vector(du[groups$group[i]])
	}
	groups$overshoot <- pmax(-groups$net_missing, 0)
	gcols <- do.call(rbind, strsplit(groups$group, "|", fixed = TRUE))
	for (j in seq_along(group)) groups[[group[j]]] <- gcols[, j]
	groups <- groups[, c(group, setdiff(names(groups), c("group", group)))]
	rownames(groups) <- NULL
	list(cells = cells, groups = groups)
}


#' Give the delta a fishery
#'
#' Cells where the logbooks cover at least `coverage_threshold` of the raised
#' effort: the delta is split between the fisheries of the logbook records of
#' that cell, in proportion to their effort (for effort) and to their catch of
#' each column (for catch; effort share when the logbooks have no catch of
#' that column). The assumption is that the unlogged effort of a flag in a
#' cell and month fishes like the logged one. Other cells: the fishery of the
#' raised records of the cell (`raised_fishery`, typically the fishery with
#' the most assigned raised effort).
#'
#' @param delta Delta cells (e.g. `reconcile_delta()$cells` of type "delta"),
#'   with `key`, `vars` and `coverage`.
#' @param operational Logbook records with `key`, `fishery_col` and `vars`,
#'   at the resolution of `key` or finer (summed per cell and fishery).
#' @param raised_fishery Data frame with `key` and `fishery_col` (one row per
#'   cell), plus any other column to keep (e.g. `purity`).
#' @param key,vars,effort_col As in [reconcile_delta()].
#' @param fishery_col Character; fishery column. Default "cluster".
#' @param coverage_threshold Numeric; minimum logbook coverage of a cell to
#'   use its logbook fishery mix. Default 0.2.
#'
#' @return One row per cell and fishery: `key`, `fishery_col`, `vars`,
#'   `delta_source` ("logbook_mix", "raised_assignment"; fishery NA when the
#'   raised records of the cell have no fishery either), `coverage`, and the
#'   extra columns of `raised_fishery` for raised-assignment rows. Sums of
#'   `vars` per cell equal `delta`.
#'
#' @family delta
#' @export
split_delta <- function(delta, operational, raised_fishery,
						key = c("flag", "latCent", "lonCent", "ymd"), vars,
						effort_col = "E", fishery_col = "cluster", coverage_threshold = 0.2) {
	.check_cols_exist(delta, c(key, vars, "coverage"), "split_delta (delta)")
	.check_cols_exist(operational, c(key, vars, fishery_col), "split_delta (operational)")
	.check_cols_exist(raised_fishery, c(key, fishery_col), "split_delta (raised_fishery)")
	delta <- as.data.frame(delta)

	opr <- operational |>
		dplyr::filter(!is.na(.data[[fishery_col]])) |>
		dplyr::group_by(dplyr::across(dplyr::all_of(c(key, fishery_col)))) |>
		dplyr::summarise(dplyr::across(dplyr::all_of(vars), ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
		as.data.frame()
	ok <- stats::ave(opr[[effort_col]], do.call(paste, c(opr[key], sep = "|")), FUN = sum) > 0
	opr <- opr[ok, , drop = FALSE]
	has_mix <- do.call(paste, c(delta[key], sep = "|")) %in% unique(do.call(paste, c(opr[key], sep = "|")))
	use_mix <- !is.na(delta$coverage) & delta$coverage >= coverage_threshold & has_mix

	# logbook mix: shares of each fishery in the cell, per variable
	A <- NULL
	if (any(use_mix)) {
		d_a <- delta[use_mix, c(key, vars, "coverage"), drop = FALSE]
		sh <- as.data.frame(dplyr::semi_join(opr, d_a, by = key))
		ck <- do.call(paste, c(sh[key], sep = "|"))
		totE <- stats::ave(sh[[effort_col]], ck, FUN = sum)
		for (v in vars) {
			tot <- stats::ave(sh[[v]], ck, FUN = sum)
			sh[[paste0(v, "_share")]] <- ifelse(tot > 0, sh[[v]] / tot, sh[[effort_col]] / totE)
		}
		sh <- sh[, c(key, fishery_col, paste0(vars, "_share"))]
		A <- dplyr::inner_join(d_a, sh, by = key, relationship = "one-to-many")
		for (v in vars) A[[v]] <- A[[v]] * A[[paste0(v, "_share")]]
		A <- A[, c(key, fishery_col, vars, "coverage")]
		A$delta_source <- "logbook_mix"
	}

	# raised assignment
	B <- delta[!use_mix, c(key, vars, "coverage"), drop = FALSE] |>
		dplyr::left_join(raised_fishery, by = key)
	B$delta_source <- ifelse(is.na(B[[fishery_col]]), NA_character_, "raised_assignment")

	as.data.frame(dplyr::bind_rows(A, B))
}
