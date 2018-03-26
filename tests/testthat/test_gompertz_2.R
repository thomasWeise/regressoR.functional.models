library("regressoR.functional.models")
context("FunctionalModel.gompertz.2")

test_that("Test FunctionalModel.gompertz.2", {
  model <- FunctionalModel.gompertz.2();
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  expect_identical(model@paramCount, 4L)
  expect_null(model@paramLower)

  expect_true(is.na(model@paramUpper[1]))
  expect_lt(model@paramUpper[2], 0)
  expect_lt(model@paramUpper[3], 0)
  expect_lt(model@paramUpper[4], 0)

  validObject(model)

  gompertz <- function(x, par) { par[1L] + (par[2L] * exp(par[3L] * exp(par[4L] * x))) }
  grad  <- function(x, par) { c(1L,
                                exp(par[3L]*exp(par[4L]*x)),
                                par[2L]*exp(par[3L]*exp(par[4L]*x)+par[4L]*x),
                                par[2L]*par[3L]*x*exp(par[3L]*exp(par[4L]*x)+par[4L]*x)) }
  par<-c(1, -3, -0.4, -2 );
  x <- c(1)
  y <- gompertz(x, par)
  expect_equal(model@f(x, par), y)
  expect_equal(model@gradient(x[1], par), grad(x[1], par))

  par<-c(0.2, -2, -0.4, -1 );
  x <- c(1, 2)
  y <- gompertz(x, par)
  expect_equal(model@f(x, par), y)
  expect_equal(model@gradient(x[1], par), grad(x[1], par))
  expect_equal(model@gradient(x[2], par), grad(x[2], par))
})
