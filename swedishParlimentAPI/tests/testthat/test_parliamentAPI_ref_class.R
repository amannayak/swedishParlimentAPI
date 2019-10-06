library(testthat)
context("Politician")
api_out <- parliamentAPI$new()


test_that("class is correct", {
  expect_true(class(api_out)[1] == "parliamentAPI")
})

# test_that("Input is correct", {
#   expect_error(api_out$calData())
# })