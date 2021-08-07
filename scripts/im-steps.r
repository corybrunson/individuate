library(tidyverse)
library(tidymodels)
devtools::load_all()

stop()

# experimentation with tidymodels
cv_test <- vfold_cv(mtcars, v = 3L)
split_test <- cv_test$splits[[1L]]
as_tibble(split_test)

cv_test <- vfold_cv(bake(rec_test, mtcars), v = 3L)
split_test <- cv_test$splits[[1L]]
as_tibble(split_test)
