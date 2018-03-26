library("regressoR.functional.models")
context("FunctionalModel.logistic.2")

test_that("Test FunctionalModel.logistic.2", {
  model <- FunctionalModel.logistic.2();
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  expect_identical(model@paramCount, 4L)
  expect_length(model@paramLower, 4L)
  expect_length(model@paramUpper, 4L)

  expect_true(is.na(model@paramLower[1]))
  expect_true(is.na(model@paramLower[2]))
  expect_gt(model@paramLower[3], 0)
  expect_true(is.na(model@paramLower[4]))

  expect_true(is.na(model@paramUpper[1]))
  expect_lt(model@paramUpper[2], 0)
  expect_true(is.na(model@paramUpper[3]))
  expect_lt(model@paramUpper[4], 0)

  validObject(model)

  logistic <- function(x, par) { par[1]+(par[2]/(1+(par[3]*(x^par[4])))) }
  grad  <- function(x, par) { c(1,
                                1/(1+par[3]*x^par[4]),
                                -(         par[2]*x^par[4])/(1+2*par[3]*x^par[4]+par[3]^2*x^(2*par[4])),
                                -(par[3]*log(x)*par[2]*x^par[4])/(1+2*par[3]*x^par[4]+par[3]^2*x^(2*par[4]))) }
  par<-c(1, -3, 0.4, -2 );
  x <- c(1)
  y <- logistic(x, par)
  expect_equal(model@f(x, par), y)
  expect_equal(model@gradient(x[1], par), grad(x[1], par))

  par<-c(0.2, -2, 0.4, -1 );
  x <- c(1, 2)
  y <- logistic(x, par)
  expect_equal(model@f(x, par), y)
  expect_equal(model@gradient(x[1], par), grad(x[1], par))
  expect_equal(model@gradient(x[2], par), grad(x[2], par))
})
