#' Fit a model to individualized cohorts
#'
#' An individualized model is any model fitted to an [individualized
#' cohort][indiv_cohorts()].
#'
#' @details This fitting procedure takes a model specification, a model formula,
#'   and a slate of individualized cohorts. It fits the specified model to each
#'   cohort, generates predictions for the corresponding new datum, and augments
#'   the individualized cohort object with columns corresponding to the desired
#'   objects (the model itself or any of the [**broom**][broom::`broom-package`]
#'   tidiers).

#' @param formula A model formula, following the constraints in
#'   [recipes::recipe()].
#' @param data The corpus data frame from which the `cohorts` were subsetted.
#' @param cohorts Output of [indiv_cohorts()] (will eventually be classed).
#' @param ret.model,ret.augment,ret.tidy,ret.glance Logical flags for whether to
#'   include columns for model objects or any of three tidiers (which will be
#'   `NA` if methods are unavailable).
#' @example inst/examples/ex-indiv-fits.r

#' @export
indiv_fits <- function(
  formula, data, cohorts,
  ret.model = FALSE, ret.augment = FALSE, ret.tidy = TRUE, ret.glance = TRUE
) {
  
}
