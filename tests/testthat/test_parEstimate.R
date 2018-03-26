library("regressoR.functional.models")
context("FunctionalModel.par.estimate")


test_that("Test FunctionalModel.par.estimate", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  model <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             paramUpper=c(3,4,5),
                                             paramLower=c(1,2,3))
  expect_identical(FunctionalModel.par.estimate(model, c(3, 3), c(4, 4), c(2.2, 3.2, 4.2)), c(2.2, 3.2, 4.2));
})

test_that("Test FunctionalModel.par.estimate and estimator", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  model <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             estimator=function(x, y) c(2, 3, 4),
                                             paramUpper=c(3,4,5),
                                             paramLower=c(1,2,3))
  expect_identical(FunctionalModel.par.estimate(model, c(3, 3), c(4, 4), c(2.2, 3.2, 4.2)), c(2.2, 3.2, 4.2));
  expect_identical(FunctionalModel.par.estimate(model, c(3, 3), c(4, 4), NULL), c(2, 3, 4));
  expect_identical(FunctionalModel.par.estimate(model, c(3, 3), c(4, 4), c(12, 4)), c(2, 3, 4));
})

test_that("Test FunctionalModel.par.estimate and wrong estimator", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  model <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             estimator=function(x, y) c(23, 3, 4),
                                             paramUpper=c(3,4,5),
                                             paramLower=c(1,2,3))
  expect_identical(FunctionalModel.par.estimate(model, c(3, 3), c(4, 4), c(2.2, 3.2, 4.2)), c(2.2, 3.2, 4.2));
  expect_false(identical(FunctionalModel.par.estimate(model, c(3, 3), c(4, 4), NULL), c(23, 3, 4)));
  expect_false(identical(FunctionalModel.par.estimate(model, c(3, 3), c(4, 4), c(12, 4)), c(23, 3, 4)));
})


test_that("Test FunctionalModel.par.estimate and limits", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  model <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             paramUpper=c(3,4,5),
                                             paramLower=c(1,2,3))
  for(i in 1:2000) {
    estimate <- FunctionalModel.par.estimate(model, c(3, 3), c(4, 4));
    expect_identical(estimate >= model@paramLower, rep(TRUE, model@paramCount));
    expect_identical(estimate <= model@paramUpper, rep(TRUE, model@paramCount));
  }
})

test_that("Test FunctionalModel.par.estimate and lower", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  model <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             paramLower=c(1,2,3))
  for(i in 1:2000) {
    estimate <- FunctionalModel.par.estimate(model, c(3, 3), c(4, 4));
    expect_identical(estimate >= model@paramLower, rep(TRUE, model@paramCount));
  }
})

test_that("Test estimate and upper", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  model <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad,
                                             paramUpper=c(3,4,5))
  for(i in 1:2000) {
    estimate <- FunctionalModel.par.estimate(model, c(3, 3), c(4, 4));
    expect_identical(estimate <= model@paramUpper, rep(TRUE, model@paramCount));
  }
})

test_that("Test estimate and no limits", {
  f <- function(x, par) { x*par[1] }
  grad <- function(x, par) { x*par }
  model <- FunctionalModel.new(f=f, paramCount=3L, gradient=grad)
  for(i in 1:2000) {
    estimate <- FunctionalModel.par.estimate(model, c(3, 3), c(4, 4));
    expect_identical(is.finite(estimate), rep(TRUE, model@paramCount));
  }
})


test_that("Test estimate over all default models", {
  for(i in 5*(1:10)) {
    x <- runif(n=i, min=1, max=100);
    y <- runif(n=i, min=1, max=100);
    for(model in FunctionalModel.all()) {
      result <- FunctionalModel.par.estimate(model, x, y);
      expect_true(!(is.null(result)));
      expect_identical(length(result), model@paramCount);
      expect_true(FunctionalModel.par.check(model, result));
    }
  }
})

