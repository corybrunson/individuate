#' Transform set-valued variables to logical membership variables
#'
#' The functions `step_mas()` create _specifications_ of recipe steps that
#' will create binary variables from set-valued attributes.
#'
#' @details `step_mas()` will construct a collection of binary variables that
#'   encode maximal itemsets from within a set-valued attribute using the MAS
#'   (Maximal-frequent All-confident pattern Selection) algorithm of Zhong &al
#'   (2020).

#' @template ref-zhong2020

#' @import recipes
#' @import Matrix
#' @importClassesFrom Matrix Matrix dMatrix CsparseMatrix ngCMatrix
#' @importClassesFrom arules transactions itemMatrix
#' @importMethodsFrom Matrix t
#' @importFrom arules items

#' @param recipe A recipe object. The step will be added to the sequence of
#'   operations for this recipe.
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step.
#' @param role For model terms created by this step, what analysis role should
#'   they be assigned? By default, the function assumes that the new columns
#'   created by the original variables will be used as predictors in a model.
#' @param trained A logical value indicating whether the values used for
#'   binarization have been checked.
#' @param max_length,min_support,min_all_confidence,min_overlap Parameters used
#'   by the MAS algorithm.
#' @param itemsets,itemnums,itemlabs A named list of itemsets, the numbers of
#'   items in each, and the unique items that appear in each. These are `NULL`
#'   until the step is trained by [recipes::prep.recipe()].
#' @param skip A logical value indicating whether the step should be skipped
#'   when the recipe is baked by `bake.recipe()`.
#' @param id A character string that is unique to this step, used to identify
#'   it.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any).
#' @example inst/examples/ex-step-mas.r

#' @export
step_mas <- function(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  max_length = Inf,
  min_support = 0.01,
  min_all_confidence = 0.1,
  min_overlap = 12L,
  itemsets = NULL,
  itemnums = NULL,
  itemlabs = NULL,
  skip = FALSE,
  id = rand_id("mas")
) {
  add_step(
    recipe,
    step_mas_new(
      terms = ellipse_check(...),
      trained = trained,
      role = role,
      max_length = max_length,
      min_support = min_support,
      min_all_confidence = min_all_confidence,
      min_overlap = min_overlap,
      itemsets = itemsets,
      itemnums = itemnums,
      itemlabs = itemlabs,
      skip = skip,
      id = id
    )
  )
}

step_mas_new <- function(
  terms, role, trained,
  max_length, min_support, min_all_confidence, min_overlap,
  itemsets, itemnums, itemlabs,
  skip, id
) {
  step(
    subclass = "mas",
    terms = terms,
    role = role,
    trained = trained,
    max_length = max_length,
    min_support = min_support,
    min_all_confidence = min_all_confidence,
    min_overlap = min_overlap,
    itemsets = itemsets,
    itemnums = itemnums,
    itemlabs = itemlabs,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_mas <- function(x, training, info = NULL, ...) {
  
  col_names <- terms_select(terms = x$terms, info = info)
  # check that all columns are list columns
  if (! all(vapply(training[, col_names, drop = FALSE], typeof, "") == "list"))
    rlang::abort("The `mas` step can only transform list columns.")
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (col_name in col_names) class(training[[col_name]]) <- "list"
  
  # maximum length as a finite value
  if (x$max_length == Inf) {
    x$max_length <- max(vapply(
      unlist(training[, col_names], recursive = FALSE),
      length, 0L
    ))
  }
  # minimum support as a proportion
  if (x$min_support >= 1) x$min_support <- x$min_support / nrow(training)
  
  # compute itemsets for each column
  
  # coerce training data to transactions
  col_transactions <- lapply(
    training[, col_names, drop = FALSE],
    as, Class = "transactions"
  )
  col_itemsets <- lapply(col_transactions, function(tas) {
    # find frequent itemsets
    its <- arules::eclat(
      tas,
      parameter = list(support = x$min_support, maxlen = x$max_len)
    )
    # restrict to maximal itemsets
    its <- subset(its, arules::is.maximal(its))
    # restrict to all-confidence threshold
    its <- subset(
      its,
      arules::interestMeasure(
        its, "allConfidence",
        transactions = tas) >= x$min_all_confidence
    )
  })
  # sizes of itemsets
  col_itemnums <- lapply(
    col_itemsets,
    function(its) rowSums(as(items(its), "matrix"))
  )
  
  # select patterns among itemsets
  
  # transform training transactions
  col_patterns <- mapply(
    function(tas, its, ns) {
      tas2 <- (t(as(as(items(its), "ngCMatrix"), "dMatrix")) %*%
                 as(as(tas, "ngCMatrix"), "dMatrix"))
      tas2 <- sweep(tas2, 1L, ns, `==`)
      tas2 <- as(as(as(tas2, "dMatrix"), "CsparseMatrix"), "ngCMatrix")
      as(tas2, "transactions")
    },
    tas = col_transactions, its = col_itemsets, ns = col_itemnums
  )
  # pattern selection based on intersections and overlaps, in order of support
  col_intersections <- lapply(col_itemsets, function(its) {
    as(items(its), "matrix") %*% t(as(items(its), "matrix"))
  })
  col_overlaps <- lapply(col_patterns, arules::crossTable, measure = "count")
  col_orders <- lapply(col_itemsets, function(its) {
    rev(order(arules::interestMeasure(its, "support")))
  })
  col_intersections <- mapply(
    function(int, ord) int[ord, ord, drop = FALSE],
    int = col_intersections, ord = col_orders,
    SIMPLIFY = FALSE
  )
  col_overlaps <- mapply(
    function(ovp, ord) ovp[ord, ord, drop = FALSE],
    ovp = col_overlaps, ord = col_orders,
    SIMPLIFY = FALSE
  )
  col_criteria <- mapply(
    function(int, ovp) int == 0L | ovp >= x$min_overlap,
    int = col_intersections, ovp = col_overlaps,
    SIMPLIFY = FALSE
  )
  col_criteria <- lapply(col_criteria, function(crt) {
    crt[lower.tri(crt, diag = TRUE)] <- NA
    crt
  })
  col_keeps <- lapply(col_criteria, function(crt) {
    apply(crt, 2L, all, na.rm = TRUE)
  })
  col_itemsets <- mapply(
    function(its, ord, kps) subset(its, subset = kps[order(ord)]),
    its = col_itemsets, ord = col_orders, kps = col_keeps
  )
  
  # format result
  
  # labels of items used in itemsets
  col_itemlabs <- lapply(
    col_itemsets,
    function(its) colnames(items(its))
  )
  # sizes of itemsets
  col_itemnums <- lapply(
    col_itemsets,
    function(its) rowSums(as(items(its), "matrix"))
  )
  
  step_mas_new(
    terms = col_names,
    role = x$role,
    trained = TRUE,
    max_length = x$max_length,
    min_support = x$min_support,
    min_all_confidence = x$min_all_confidence,
    min_overlap = x$min_overlap,
    itemsets = col_itemsets,
    itemnums = col_itemnums,
    itemlabs = col_itemlabs,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_mas <- function(object, new_data, ...) {
  
  # remove troublesome 'AsIs' class (and any other non-'list' classes)
  for (nm in object$terms) class(new_data[[nm]]) <- "list"
  
  # new sequence of column names
  itemsets_dats <- lapply(object$itemsets, as, "data.frame")
  new_col_names <- lapply(names(new_data), function(nm) {
    if (nm %in% names(itemsets_dats)) {
      paste(nm, itemsets_dats[[nm]]$items, sep = "_")
    } else nm
  })
  names(new_col_names) <- names(new_data)
  
  # coerce new data to transactions
  new_transactions <- mapply(
    function(col, lab) {
      c(
        # ensure that all itemset items are present
        list(lab),
        # remove any codes missing from itemset items
        lapply(col, intersect, lab)
      ) %>%
        as("transactions")
    },
    col = new_data[, object$terms, drop = FALSE], lab = object$itemlabs
  )
  
  # transform new data transactions
  new_transactions_rt <- mapply(
    function(tas, its, ns) {
      tas2 <- (t(as(as(items(its), "ngCMatrix"), "dMatrix")) %*%
                 as(as(tas, "ngCMatrix"), "dMatrix"))
      tas2 <- sweep(tas2, 1L, ns, `==`)
      tas2 <- as(as(as(tas2, "dMatrix"), "CsparseMatrix"), "ngCMatrix")
      as(tas2, "transactions")[-1L]
    },
    tas = new_transactions, its = object$itemsets, ns = object$itemnums
  )
  
  # coerce to transformed new data
  for (nm in names(new_transactions_rt)) {
    new_transactions_rt[[nm]] %>%
      as("matrix") %>%
      as.data.frame() %>%
      `names<-`(new_col_names[[nm]]) ->
      dat
    new_data <- cbind(new_data, dat)
  }
  
  new_data[, unlist(new_col_names), drop = FALSE]
}

#' @export
print.step_mas <- function(
  x, width = max(20, options()$width - 35), ...
) {
  cat("MAS binarization on ", sep = "")
  printer(
    untr_obj = x$terms,
    tr_obj = NULL,
    trained = x$trained,
    width = width
  )
  invisible(x)
}
