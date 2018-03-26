library("regressoR.functional.models")
context("FunctionalModel.exponentialDecay.1")

test_that("Test FunctionalModel.exponentialDecay.1", {
  model <- FunctionalModel.exponentialDecay.1();
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  expect_identical(model@paramCount, 4L)
  expect_length(model@paramLower, 4L)
  expect_length(model@paramUpper, 4L)

  expect_true(is.na(model@paramLower[1]))
  expect_gt(model@paramLower[2], 0)
  expect_true(is.na(model@paramLower[3]))
  expect_gt(model@paramLower[4], 0)

  expect_true(is.na(model@paramUpper[1]))
  expect_true(is.na(model@paramUpper[2]))
  expect_lt(model@paramUpper[3], 0)
  expect_true(is.na(model@paramUpper[4]))

  validObject(model)

  decay <- function(x, par) { par[1]+(par[2]*exp(par[3]*x^par[4])) }
  grad  <- function(x, par) { c(1,
                                exp(par[3]*x^par[4]),
                                par[2] * x^par[4] * exp(par[3]*x^par[4]),
                                par[2] * par[3] * x^par[4] * exp(par[3]*x^par[4]) * log(x) ) }
  par<-c(1, 3, -0.4, 2 );
  x <- c(1)
  y <- decay(x, par)
  expect_equal(model@f(x, par), y)
  expect_equal(model@gradient(x[1], par), grad(x[1], par))

  par<-c(0.1, 2, -0.4, 1 );
  x <- c(1, 2)
  y <- decay(x, par)
  expect_equal(model@f(x, par), y)
  expect_equal(model@gradient(x[1], par), grad(x[1], par))
  expect_equal(model@gradient(x[2], par), grad(x[2], par))
})
