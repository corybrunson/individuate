rec_test <- recipe(mtcars, mpg ~ .) %>%
  step_binarize(vs, am, pass = 1) %>%
  step_center(all_predictors(), -vs, -am) %>%
  step_scale(all_predictors(), -vs, -am) %>%
  prep()
bake(rec_test, mtcars)
