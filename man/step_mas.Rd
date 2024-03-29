% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step-mas.r
\name{step_mas}
\alias{step_mas}
\title{Transform set-valued variables to logical membership variables}
\usage{
step_mas(
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
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the sequence of
operations for this recipe.}

\item{...}{One or more selector functions to choose which variables are
affected by the step.}

\item{role}{For model terms created by this step, what analysis role should
they be assigned? By default, the function assumes that the new columns
created by the original variables will be used as predictors in a model.}

\item{trained}{A logical value indicating whether the values used for
binarization have been checked.}

\item{max_length, min_support, min_all_confidence, min_overlap}{Parameters used
by the MAS algorithm.}

\item{itemsets, itemnums, itemlabs}{A named list of itemsets, the numbers of
items in each, and the unique items that appear in each. These are \code{NULL}
until the step is trained by \code{\link[recipes:prep]{recipes::prep.recipe()}}.}

\item{skip}{A logical value indicating whether the step should be skipped
when the recipe is baked by \code{bake.recipe()}.}

\item{id}{A character string that is unique to this step, used to identify
it.}
}
\value{
An updated version of \code{recipe} with the new step added to the
sequence of existing steps (if any).
}
\description{
The functions \code{step_mas()} create \emph{specifications} of recipe steps that
will create binary variables from set-valued attributes.
}
\details{
\code{step_mas()} will construct a collection of binary variables that
encode maximal itemsets from within a set-valued attribute using the MAS
(Maximal-frequent All-confident pattern Selection) algorithm of Zhong &al
(2020).
}
\examples{
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
toy_data \%>\%
  filter(part == "train") \%>\%
  recipe() \%>\%
  step_mas(letter) \%>\%
  prep(strings_as_factors = FALSE) ->
  toy_rec

# preprocess training data
bake(toy_rec, new_data = NULL)

# preprocess testing data
toy_data \%>\%
  filter(part == "test") \%>\%
  bake(object = toy_rec)
}
\references{
Zhong H, Loukides G, & Gwadera R (2020) "Clustering datasets with
demographics and diagnosis codes". \emph{Journal of Biomedical Informatics} 102,
103360. \doi{10.1016/j.jbi.2019.103360}
}
