library("regressoR.functional.models")
context("FunctionalModel.constant")

test_that("Test FunctionalModel.constant", {
  model <- FunctionalModel.constant();
  expect_identical(is.null(model), FALSE);
  expect_is(model, "FunctionalModel")
  expect_identical(model@paramCount, 1L)
  expect_identical(model@paramLower, NULL)
  expect_identical(model@paramUpper, NULL)
  validObject(model)

  lin<-function(x,par){ rep(par[1], length(x)) }

  par<-c(1);
  x<-c(1)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x, y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1))

  par<-c(-3);
  x<-c(1,7)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1))
  expect_equal(model@gradient(x[2], par), c(1))

  par<-c(0);
  x<-c(1,7)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1))
  expect_equal(model@gradient(x[2], par), c(1))
})
