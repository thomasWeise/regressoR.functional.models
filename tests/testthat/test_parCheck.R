library("regressoR.functional.models")
context("par.check")

test_that("Test par.check with both limits", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad, paramUpper=c(3,4,5), paramLower=c(1,2,3))

  expect_true(par.check(functionalModel, c(1,2,3)))
  expect_true(par.check(functionalModel, c(3,4,5)))
  expect_true(par.check(functionalModel, c(2,3,4)))
  expect_false(par.check(functionalModel, c(1,NaN,3)))
  expect_false(par.check(functionalModel, c(1,+Inf,3)))
  expect_false(par.check(functionalModel, c(1,-Inf,3)))
  expect_false(par.check(functionalModel, c(1,NA,3)))
  expect_false(par.check(functionalModel, c(4,2,3)))
  expect_false(par.check(functionalModel, c(1,-2,3)))
})

test_that("Test par.check with mixed NA", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad, paramUpper=c(3,NA,5), paramLower=c(1,2,3))

  expect_true(par.check(functionalModel, c(1,2,3)))
  expect_true(par.check(functionalModel, c(3,4,5)))
  expect_true(par.check(functionalModel, c(2,3,4)))
  expect_false(par.check(functionalModel, c(1,NaN,3)))
  expect_false(par.check(functionalModel, c(1,+Inf,3)))
  expect_false(par.check(functionalModel, c(1,-Inf,3)))
  expect_false(par.check(functionalModel, c(1,NA,3)))
  expect_false(par.check(functionalModel, c(4,2,3)))
  expect_true(par.check(functionalModel, c(1,52,3)))
  expect_false(par.check(functionalModel, c(1,-2,3)))
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad, paramUpper=c(3,4,5), paramLower=c(1,NA,3))

  expect_true(par.check(functionalModel, c(1,2,3)))
  expect_true(par.check(functionalModel, c(3,4,5)))
  expect_true(par.check(functionalModel, c(2,3,4)))
  expect_false(par.check(functionalModel, c(1,NaN,3)))
  expect_false(par.check(functionalModel, c(1,+Inf,3)))
  expect_false(par.check(functionalModel, c(1,-Inf,3)))
  expect_false(par.check(functionalModel, c(1,NA,3)))
  expect_false(par.check(functionalModel, c(4,2,3)))
  expect_true(par.check(functionalModel, c(1,-2,3)))
  expect_false(par.check(functionalModel, c(-2,2,3)))
})

test_that("Test par.check with only lower limits", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad, paramLower=c(1,2,3))

  expect_true(par.check(functionalModel, c(1,2,3)))
  expect_true(par.check(functionalModel, c(3,4,5)))
  expect_true(par.check(functionalModel, c(2,3,4)))
  expect_false(par.check(functionalModel, c(1,NaN,3)))
  expect_false(par.check(functionalModel, c(1,+Inf,3)))
  expect_false(par.check(functionalModel, c(1,-Inf,3)))
  expect_false(par.check(functionalModel, c(1,NA,3)))
  expect_true(par.check(functionalModel, c(4,2,3)))
  expect_false(par.check(functionalModel, c(1,-2,3)))
})

test_that("Test par.check with only upper limits", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad, paramUpper=c(3,4,5))

  expect_true(par.check(functionalModel, c(1,2,3)))
  expect_true(par.check(functionalModel, c(3,4,5)))
  expect_true(par.check(functionalModel, c(2,3,4)))
  expect_false(par.check(functionalModel, c(1,NaN,3)))
  expect_false(par.check(functionalModel, c(1,+Inf,3)))
  expect_false(par.check(functionalModel, c(1,-Inf,3)))
  expect_false(par.check(functionalModel, c(1,NA,3)))
  expect_false(par.check(functionalModel, c(4,2,3)))
  expect_true(par.check(functionalModel, c(1,-2,3)))
})