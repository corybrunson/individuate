library(proxy)

`*.factor` <- function(e1, e2) {
  stopifnot(is.factor(e1), is.factor(e2))
  e12 <- forcats::fct_unify(list(e1 = e1, e2 = e2))
  e1 <- e12$e1; e2 <- e12$e2
  ifelse(e1 == e2, 1L, -1L)
}

cat_cos_prefun <- function(x, y, pairwise, p, reg_entry) {
  # format as data frames (borrowed from `pr_Gower_prefun`)
  x <- as.data.frame(x)
  if (!is.null(y)) y <- as.data.frame(y)
  # handle logical, numeric, and factor variables separately
  l <- sapply(x, is.logical)
  f <- sapply(x, is.factor)
  
}
cat_cos_fun <- function(x, y) {
  
}
cat_cos_postfun <- function(result, p) {
  
}

## create a new distance measure
mydist <- function(x,y) sum(x * y)

## create a new entry in the registry with two aliases
pr_DB$set_entry(FUN = mydist, names = c("test", "mydist"))

## look it up (index is case insensitive):
pr_DB$get_entry("TEST")

## modify the content of the description field in the new entry
pr_DB$modify_entry(names = "test", description = "foo function")

## create a new field
pr_DB$set_field("New")

## look up the test entry again (two ways)
pr_DB$get_entry("test")
pr_DB[["test"]]

## show total number of entries
length(pr_DB)

## show all entries (short list)
pr_DB$get_entries(pattern = "foo")

## show more details
summary(pr_DB, "long")

## get all entries in a list (and extract first two ones)
pr_DB$get_entries()[1:2]

## get all entries as a data frame (select first 3 fields)
as.data.frame(pr_DB)[,1:3]

## delete test entry
pr_DB$delete_entry("test")

## check if it is really gone
pr_DB$entry_exists("test")
