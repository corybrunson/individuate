set.seed(0)
mtcars_new <- sample(seq(nrow(mtcars)), 3L)
mtcars_form <- as.formula(mpg ~ cyl + disp + hp)
mtcars_mod <- lm(mtcars_form, mtcars)
mtcars_cohorts <- indiv_cohorts(
  mtcars, ids = mtcars_new, simil_method = "correlation",
  threshold = .9, cardinality = 6L, ties_method = "min"
)
mtcars_cohorts %>%
  dplyr::mutate(fit = purrr::map(cohort, ~ lm(mtcars_form, .x))) %>%
  dplyr::mutate(pred = purrr::map2_dbl(
    new_datum, fit,
    ~ predict(.y, newdata = .x)
  )) %>%
  print() ->
  mtcars_fits
tibble::tibble(
  response = mtcars$mpg[mtcars_new],
  lm_pred = predict(mtcars_mod, dplyr::slice(mtcars, mtcars_new)),
  im_pred = mtcars_fits$pred
)
mtcars_cohorts <- indiv_cohorts(
  mtcars, ids = mtcars_new, simil_method = "correlation",
  threshold = .9, cardinality = 6L, ties_method = "min", .full_cohorts = FALSE
)
mtcars_cohorts %>%
  # how much memory is allocated here?
  dplyr::mutate(fit = purrr::map(
    idx,
    ~ lm(mtcars_form, dplyr::slice(mtcars, .x))
  )) %>%
  dplyr::mutate(pred = purrr::map2_dbl(
    new_datum, fit,
    ~ predict(.y, newdata = .x)
  )) %>%
  print() ->
  mtcars_fits
tibble::tibble(
  response = mtcars$mpg[mtcars_new],
  lm_pred = predict(mtcars_mod, dplyr::slice(mtcars, mtcars_new)),
  im_pred = mtcars_fits$pred
)
