library("regressoR.functional.models")
context("par.estimate")


test_that("Test par.estimate", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             paramUpper=c(3,4,5),
                                             paramLower=c(1,2,3))
  expect_identical(par.estimate(functionalModel, c(3, 3), c(4, 4), c(2.2, 3.2, 4.2)), c(2.2, 3.2, 4.2));
})

test_that("Test par.estimate and estimator", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             estimator=function(x, y, paramLower, paramUpper) c(2, 3, 4),
                                             paramUpper=c(3,4,5),
                                             paramLower=c(1,2,3))
  expect_identical(par.estimate(functionalModel, c(3, 3), c(4, 4), c(2.2, 3.2, 4.2)), c(2.2, 3.2, 4.2));
  expect_identical(par.estimate(functionalModel, c(3, 3), c(4, 4), NULL), c(2, 3, 4));
  expect_identical(par.estimate(functionalModel, c(3, 3), c(4, 4), c(12, 4)), c(2, 3, 4));
})

test_that("Test par.estimate and wrong estimator", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             estimator=function(x, y, paramLower, paramUpper) c(23, 3, 4),
                                             paramUpper=c(3,4,5),
                                             paramLower=c(1,2,3))
  expect_identical(par.estimate(functionalModel, c(3, 3), c(4, 4), c(2.2, 3.2, 4.2)), c(2.2, 3.2, 4.2));
  expect_false(identical(par.estimate(functionalModel, c(3, 3), c(4, 4), NULL), c(23, 3, 4)));
  expect_false(identical(par.estimate(functionalModel, c(3, 3), c(4, 4), c(12, 4)), c(23, 3, 4)));
})


test_that("Test par.estimate and limits", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             paramUpper=c(3,4,5),
                                             paramLower=c(1,2,3))
  for(i in 1:2000) {
    estimate <- par.estimate(functionalModel, c(3, 3), c(4, 4));
    expect_identical(estimate >= functionalModel@paramLower, rep(TRUE, functionalModel@paramCount));
    expect_identical(estimate <= functionalModel@paramUpper, rep(TRUE, functionalModel@paramCount));
  }
})

test_that("Test par.estimate and lower", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             paramLower=c(1,2,3))
  for(i in 1:2000) {
    estimate <- par.estimate(functionalModel, c(3, 3), c(4, 4));
    expect_identical(estimate >= functionalModel@paramLower, rep(TRUE, functionalModel@paramCount));
  }
})

test_that("Test estimate and upper", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             paramUpper=c(3,4,5))
  for(i in 1:2000) {
    estimate <- par.estimate(functionalModel, c(3, 3), c(4, 4));
    expect_identical(estimate <= functionalModel@paramUpper, rep(TRUE, functionalModel@paramCount));
  }
})

test_that("Test estimate and no limits", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  functionalModel <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad)
  for(i in 1:2000) {
    estimate <- par.estimate(functionalModel, c(3, 3), c(4, 4));
    expect_identical(is.finite(estimate), rep(TRUE, functionalModel@paramCount));
  }
})
