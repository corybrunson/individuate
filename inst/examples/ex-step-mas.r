# toy data set
toy_data <- data.frame(
  id = LETTERS[seq(21L, 26L)],
  letter = I(list(
    c("a", "b", "d"),
    c("a", "b", "c"),
    c("b", "d"),
    c("b"),
    c("a", "b", "d"),
    c("a", "b", "d", "e")
  )),
  part = rep(c("train", "test"), each = 3L)
)
# each part contains values missing from the other
print(toy_data)

# build preprocessing recipe
toy_data %>%
  filter(part == "train") %>%
  recipe() %>%
  step_mas(letter) %>%
  prep(strings_as_factors = FALSE) ->
  toy_rec

# preprocess training data
juice(toy_rec)

# preprocess testing data
toy_data %>%
  filter(part == "test") %>%
  bake(object = toy_rec)
