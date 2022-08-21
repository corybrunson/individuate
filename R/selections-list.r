#' Additional methods for selecting variables in step functions
#'
#' These functions augment those documented at [recipes::has_role()].
#'
#' @description
#'
#' `all_list()` selects columns of type 'list'. `all_list_predictors()` selects
#' 'list' type columns assigned the role 'predictor'.
#' 
#' **These need to be allowed into step functions somehow.**
#' 
#' @name selections-list

#' @export
#' @rdname selections-list
all_list <- function() {
  has_type("list")
}
#' @export
#' @rdname selections-list
all_list_predictors <- function() {
  intersect(has_role("predictor"), has_type("list"))
}
