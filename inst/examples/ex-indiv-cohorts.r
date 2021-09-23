# sample "new data" (testing data) from `mtrows`
set.seed(0)
mtcars_new <- sample(seq(nrow(mtcars)), 5L)
# fix modeling formula
mtcars_form <- as.formula(mpg ~ cyl + disp + hp)
# fit linear model to training data
mtcars_mod <- lm(mtcars_form, mtcars[-mtcars_new, , drop = FALSE])
# construct a cohort for each testing datum (include full cohorts in result)
mtcars_cohorts <- indiv_cohorts(
  mtcars, new_data = mtcars_new, simil_method = "correlation",
  threshold = .9, cardinality = 10L, ties_method = "min", .full_cohorts = TRUE
)
# fit linear model to each cohort
# -+- this needs to be made into a standalone function -+-
mtcars_cohorts %>%
  dplyr::mutate(fit = purrr::map(cohort, ~ lm(mtcars_form, .x))) %>%
  dplyr::mutate(pred = purrr::map2_dbl(
    row, fit,
    ~ predict(.y, newdata = dplyr::slice(mtcars, .x))
  )) %>%
  print() ->
  mtcars_fits
# compare global predictions to individualized predictions
tibble::tibble(
  response = mtcars$mpg[mtcars_new],
  lm_pred = predict(mtcars_mod, dplyr::slice(mtcars, mtcars_new)),
  im_pred = mtcars_fits$pred
)
# construct a cohort for each testing datum (include indices only)
mtcars_cohorts <- indiv_cohorts(
  mtcars, new_data = mtcars_new, simil_method = "correlation",
  threshold = .9, cardinality = 10L, ties_method = "min"
)
# fit linear model to each cohort
mtcars_cohorts %>%
  # -+- how much memory is allocated here? -+-
  dplyr::mutate(fit = purrr::map(
    idx,
    ~ lm(mtcars_form, dplyr::slice(mtcars, .x))
  )) %>%
  dplyr::mutate(pred = purrr::map2_dbl(
    row, fit,
    ~ predict(.y, newdata = dplyr::slice(mtcars, .x))
  )) %>%
  print() ->
  mtcars_fits
# compare global predictions to individualized predictions
tibble::tibble(
  response = mtcars$mpg[mtcars_new],
  lm_pred = predict(mtcars_mod, dplyr::slice(mtcars, mtcars_new)),
  im_pred = mtcars_fits$pred
)
