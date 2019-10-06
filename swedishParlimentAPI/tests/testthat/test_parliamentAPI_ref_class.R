library(testthat)
context("Politician")
api_out <- parliamentAPI$new()


test_that("class is correct", {
  expect_true(class(api_out)[1] == "parliamentAPI")
})

test_that("Input is correct", {
  expect_error(api_out$CalData(startD = 20-10-2000, endD = 10-10-2010))
  expect_error(api_out$CalData(startD = "20-10-2000", endD = "10-10-2010"))
  expect_error(api_out$CalData(startD = "2010-01-01", endD = "2001-01-01"))
})

# test_that("Input is correct", {
#   expect_error(api_out$calData())
# })