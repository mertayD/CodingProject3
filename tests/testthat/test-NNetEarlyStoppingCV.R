# write tests for that R function, in tests/testthat/test-NNetEarlyStoppingCV:
# (1) for valid inputs your function returns an output of the expected type/dimension
# (2) for an invalid input, your function stops with an informative error message.

library(CodingProject3)
library(testthat)
context("test-NNetEarlyStoppingCV")


test_that("NNetEarlyStoppingCV computes the right demensions", {
  data(ozone, package="ElemStatLearn")
  X.mat <- as.matrix(ozone[,-1])
  y.vec <- as.vector(ozone[,1])
  max.iterations <- 100
  step.size <- .5
  n.hidden.units <- 2
  result <- NNetEarlyStoppingCV(X.mat, y.vec, , max.iterations, step.size, n.hidden.units)
  
  expect_equal(length(res), 7)
})

test_that("NNetEarlyStoppingCV throws errors", {
  data(ozone, package="ElemStatLearn")
  X.mat <- as.matrix(ozone[,-1])
  y.vec <- as.vector(ozone[,1])
  max.iterations <- 100
  step.size <- .5
  n.hidden.units <- 2
  
  expect_error(NNetEarlyStoppingCV(X.mat, y.vec, , max.iterations, step.size, n.hidden.units), "Feature matrix is not a matrix")
})
