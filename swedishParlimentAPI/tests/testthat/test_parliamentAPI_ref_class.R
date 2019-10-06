library(testthat)
context("Politician")
api_out <- parliamentAPI$new()

test_that("class is correct", {
  expect_true(class(api_out)[1] == "parliamentAPI")
})

test_that("Input Date are correct when it is char type", {
  expect_error(api_out$CalData(startD = 20-10-2000, endD = 10-10-2010))
})


test_that("Input Dates are correct when date format is YYYY-MM-DD", {
  expect_error(api_out$CalData(startD = "20-10-2000", endD = "10-10-2010"))
})

test_that("Input Date are correct when Start Date is smaller than End Date", {
  expect_error(api_out$CalData(startD = "2010-01-01", endD = "2001-01-01"))
})

listOutput <- api_out$CalData(startD = "2000-01-01", endD = "2010-01-01")
test_that("CalData method works", {
  #listOutput <- api_out$CalData(startD = "2000-01-01", endD = "2010-01-01")
  #calculated once to remove redundancy
  expect_true(class(listOutput)[1] == "list")
})

test_that("CalData should return Dataframe of all members in given period", {
  #listOutput <- api_out$CalData(startD = "2000-01-01", endD = "2010-01-01")
  allMem = listOutput$AllMembers
  expect_true(class(allMem)[1] == "data.frame")
})

test_that("CalData should return Dataframe of all Female Members in given period", {
  #listOutput <- api_out$CalData(startD = "2000-01-01", endD = "2010-01-01")
  allFemaleMem = listOutput$FemaleMembers
  expect_true(class(allFemaleMem)[1] == "data.frame")
})

test_that("CalData should return Dataframe of all Male Members in given period", {
  #listOutput <- api_out$CalData(startD = "2000-01-01", endD = "2010-01-01")
  allMenMember = listOutput$MaleMembers
  expect_true(class(allMenMember)[1] == "data.frame")
})

test_that("CalData should return count of all Members in given period", {
  #listOutput <- api_out$CalData(startD = "2000-01-01", endD = "2010-01-01")
  CountAll = listOutput$CountAll
  expect_true(class(CountAll)[1] == "integer")
})

test_that("CalData should return count of all Female Members in given period", {
  CountFemale = listOutput$CountFemale
  expect_true(class(CountFemale)[1] == "integer")
})

test_that("CalData should return count of all Male Members in given period", {
  #listOutput <- api_out$CalData(startD = "2000-01-01", endD = "2010-01-01")
  CountMale = listOutput$CountMan
  expect_true(class(CountMale)[1] == "integer")
})
