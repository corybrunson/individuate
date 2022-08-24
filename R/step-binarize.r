#' Create binary variables from categorical and numeric variables
#'
#' `step_binarize()` creates a _specification_ of a recipe step that will create
#' binary variables from categorical or numeric variables based on a provided
#' set of passing or failing values.
#'
#' @details At least one of `pass` and `fail` must be provided, and if both are
#'   provided then values in neither will be converted to `NA`. For easier
#'   compatibility with other steps, binarization produces integer-valued
#'   variables taking the values `0` and `1` rather than logical variables.

#' @import recipes

#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical value indicating whether the values used for
#'   binarization have been checked.
#' @param pass,fail Vectors of values, matched to the variables, to be coded as
#'   `1` or `0`, respectively.
#' @param skip A logical value indicating whether the step should be skipped
#'   when the recipe is baked by `bake.recipe()`.
#' @param id A character string that is unique to this step, used to identify
#'   it.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any).
#' @example inst/examples/ex-step-binarize.r

#' @export
step_binarize <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  # assign values to `pass` or `fail` (or both);
  # if both are assigned, other values will be made `NA`
  pass = NULL, fail = NULL,
  skip = FALSE,
  id = rand_id("binarize")
) {
  add_step(
    recipe,
    step_binarize_new(
      terms = ellipse_check(...),
      trained = trained,
      role = role,
      pass = pass, fail = fail,
      skip = skip,
      id = id
    )
  )
}

step_binarize_new <- function(
  terms, role, trained, pass = NULL, fail = NULL, skip, id
) {
  step(
    subclass = "binarize",
    terms = terms,
    role = role,
    trained = trained,
    pass = pass, fail = fail,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_binarize <- function(x, training, info = NULL, ...) {
  # col_names <- terms_select(terms = x$terms, info = info)
  col_names <- recipes_eval_select(x$terms, training, info = info)
  check_type(training[, col_names])
  
  # prepare reference values of `pass` and/or `fail`?
  if (is.null(x$pass) && is.null(x$fail))
    rlang::abort("One of `pass` and `fail` must be non-null.")
  if (length(intersect(x$pass, x$fail)) > 0L)
    rlang::abort("`pass` and `fail` values must be disjoint.")
  
  step_binarize_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    pass = x$pass, fail = x$fail,
    skip = x$skip,
    id = x$id
  )
}

# helper function
binarize_by <- function(x, pass = NULL, fail = NULL) {
  # vector of `FALSE` values
  xb <- vector(mode = "logical", length = length(x))
  #xb <- rep(NA, length(x))
  # if `pass` is provided, set corresponding values to `TRUE`
  if (! is.null(pass)) {
    xb[x %in% pass] <- TRUE
    # if `fail` is also provided, set neither-corresponding values to `NA`
    if (! is.null(fail)) {
      xb[! x %in% union(pass, fail)] <- NA
    }
  } else if (! is.null(fail)) {
    # if only `fail` is provided, set non-corresponding, non-NA values to `TRUE`
    xb[! is.na(x) & ! x %in% fail] <- TRUE
  } else {
    xb[seq_along(x)] <- NA
  }
  # if `NA` is not in `pass` or `fail`, set `NA` values to `NA`
  if (! NA %in% union(pass, fail)) xb[is.na(x)] <- NA
  as.integer(xb)
}

#' @export
bake.step_binarize <- function(object, new_data, ...) {
  res <- apply(
    as.matrix(new_data[, object$terms]), 2L, binarize_by,
    pass = object$pass, fail = object$fail
  )
  new_data[, object$terms] <- res
  tibble::as_tibble(new_data)
}

#' @export
print.step_binarize <- function(
  x, width = max(20, options()$width - 35), ...
) {
  cat("Logical binarization on ", sep = "")
  printer(
    untr_obj = x$terms,
    tr_obj = NULL,
    trained = x$trained,
    width = width
  )
  invisible(x)
}
