#' Construct individualized cohorts
#'
#' An individualized cohort for a case \eqn{x} consists of the most similar, or
#' relevant, cases to \eqn{x} in the available corpus.
#'
#' @details The individualized cohort about an index case may be capped at a
#'   number or a similarity threshold. When the index cases are drawn from the
#'   corpus, they are excluded from their own cohorts.

#' @importFrom rlang "%||%"

#' @param data A data frame, used as the corpus.
#' @param new_data A data frame of index cases, or an integer vector of row
#'   names or numbers used to slice cases from `data`.
#' @param simil_method A character value, passed to the `method` parameter of
#'   [proxy::simil()].
#' @param threshold A numeric value that similarities between each index case
#'   and the cases in its individualized cohort must exceed.
#' @param cardinality An integer value that bounds the size of each
#'   individualized cohort (up to rank ties).
#' @param ties_method passed to the `ties.method` parameter of [rank()].
#' @param weight The name of a weight object (a character value) or an object
#'   itself (of the form `*_weight`).
#' @param .full_cohorts Logical; whether to retain a column of cohorts in
#'   addition to a column of their index sets with respect to `data`.
#' @return A tibble with columns `row` (either `seq(nrow(new_data))` or
#'   `new_data`, depending on `new_data`), `new_datum` (each use case formatted
#'   as a one-row data frame), `idx` (the row numbers in `data` of the
#'   constructed individual cohort), and, optionally, `cohort` (the
#'   individualized cohort, formatted as a data frame).
#' @example inst/examples/ex-indiv-cohorts.r

#' @export
indiv_cohorts <- function(
  data, new_data = NULL, simil_method = "cosine",
  threshold = NULL, cardinality = NULL, ties_method = "min",
  weight = "constant",
  .full_cohorts = FALSE
) {
  
  # find `*_weight()` function if it exists
  if (is.character(weight) && exists(paste0(weight, "_weight"))) {
    weight <- get(paste0(weight, "_weight"))
    weight <- weight()
  }
  
  # if testing data not provided, default to training data and flag this action
  if (self <- is.null(new_data)) {
    ids <- seq(nrow(data))
    new_data <- data
  } else if (self <- is.numeric(new_data)) {
    ids <- as.integer(new_data)
    new_data <- data[new_data, , drop = FALSE]
  } else {
    stopifnot(is.data.frame(new_data))
    ids <- seq(nrow(new_data))
  }
  # default specs
  if (is.null(simil_method)) simil_method <- "cosine"
  # match to similarity/distance measures available in *proxy*
  simil_method <- match.arg(
    tolower(simil_method),
    tolower(names(proxy::pr_DB$get_entries()))
  )
  
  # calculate similarities between training set and (subset of) testing set
  simils <- proxy::simil(data, new_data, method = simil_method, by_rows = TRUE)
  # exclude selves
  if (self) {
    for (i in seq(ncol(simils))) {
      simils[[ids[[i]], i]] <- NA_real_
    }
  }
  # identify nearest neighbors
  idxs <- lapply(seq(ncol(simils)), function(i) {
    unname(which(
      simils[, i] > (threshold %||% 0) &
        rank(-simils[, i], ties.method = ties_method) <= (cardinality %||% Inf)
    ))
  })
  # compute each cohort's radius and cardinality
  thrs <- vapply(seq(ncol(simils)), function(i) min(simils[idxs[[i]], i]), 0)
  cards <- vapply(idxs, length, 0L)
  # calculate weights of neighbors
  wts <- lapply(
    seq_along(idxs),
    function(i) weight$calculation(simils[[i]][idxs[[i]]])
  )
  
  # create individualized cohorts
  cohorts <- tibble::tibble(
    row = ids,
    idx = idxs,
    threshold = thrs, cardinality = cards,
    weights = wts
  )
  if (.full_cohorts) {
    cohorts <- dplyr::mutate(
      cohorts,
      cohort = purrr::map(idxs, ~ dplyr::slice(data, .x))
    )
  }
  cohorts
}

# adapted from `scales::trans_new`
weight_new <- function(name, calculation, input = "dist") {
  if (rlang::is_formula(calculation))
    calculation <- rlang::as_function(calculation)
  structure(list(
    name = name,
    calculation = calculation,
    input = input
  ), class = "weight")
}
# -+- won't work via method dispatch -+-
print.weight <- function(x, ...) cat("Weighting function: ", x$name, "\n")
constant_weight <- function(value = 1, input = "dist") {
  input <- match.arg(input, c("distance", "similarity"))
  calculation <- function(x) rep(value, length(x))
  weight_new("constant_weight", calculation, input = input)
}
rank_weight <- function(ties_method = "min", input = "dist") {
  input <- match.arg(input, c("distance", "similarity"))
  ties_method <- tolower(ties_method)
  calculation <- if (input == "distance") {
    function(x) length(x) + 1L - rank(x, ties.method = ties_method)
  } else if (input == "similarity") {
    function(x) rank(x, ties.method = ties_method)
  }
  weight_new(paste0("rank-", ties_method), calculation, input = input)
}
triangle_weight <- function(apex = 1, input = "dist") {
  input <- match.arg(input, c("distance", "similarity"))
  apex <- as.numeric(apex)
  calculation <- if (input == "distance") {
    function(x) 1 - x
  } else if (input == "similarity") {
    function(x) x - 1
  }
  weight_new(paste0("triangle-", apex), calculation, input = input)
}
inverse_weight <- function(input = "dist") {
  input <- match.arg(input, c("distance", "similarity"))
  if (input == "similarity") rlang::warn(
    "Using identity rather than inverse weight for similarity measure.",
    .frequency = "once", .frequency_id = "individuate.inverse_weight"
  )
  calculation <- if (input == "distance") {
    function(x) 1 / x
  } else if (input == "similarity") {
    function(x) x
  }
  weight_new("inverse", calculation, input = input)
}
identity_weight <- function(input = "simil") {
  input <- match.arg(input, c("distance", "similarity"))
  if (input == "distance") rlang::warn(
    "Using inverse rather than identity weight for distance measure.",
    .frequency = "once", .frequency_id = "individuate.identity_weight"
  )
  calculation <- if (input == "distance") {
    function(x) x
  } else if (input == "similarity") {
    function(x) 1 / x
  }
  weight_new("inverse", calculation, input = input)
}

#' @export
rlang::`%||%`
