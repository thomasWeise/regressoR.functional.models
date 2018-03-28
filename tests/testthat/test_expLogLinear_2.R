library("regressoR.functional.models")
context("FunctionalModel.expLogLinear.2")

test_that("Test FunctionalModel.expLogLinear.2", {
  model <- FunctionalModel.expLogLinear.2();
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
  expect_true(is.na(model@paramUpper[4]))

  validObject(model)

  expLogLinear <- function(x, par) { par[1L] + par[2L]*exp(par[3L]*log(x+par[4L])) }
  grad  <- function(x, par) { c(1,
                               (par[4L]+x)^par[3L],
                                par[2L]*(par[4L]+x)^par[3L]*log(x+par[4L]),
                                par[2L]*par[3L]*(par[4L]+x)^(par[3L]-1)) }
  par<-c(1, 3, -0.4, 2 );
  x <- c(1)
  y <- expLogLinear(x, par)
  expect_equal(model@f(x, par), y)
  expect_equal(model@gradient(x[1], par), grad(x[1], par))

  par<-c(0.2, 2, -0.4, 1 );
  x <- c(1, 2)
  y <- expLogLinear(x, par)
  expect_equal(model@f(x, par), y)
  expect_equal(model@gradient(x[1], par), grad(x[1], par))
  expect_equal(model@gradient(x[2], par), grad(x[2], par))
})
