library("regressoR.functional.models")
context("linear")

test_that("Test linear", {
  model <- linear();
  expect_identical(is.null(model), FALSE);
  expect_is(model, "FunctionalModel")
  expect_identical(model@paramCount, 2L)
  expect_identical(model@paramLower, NULL)
  expect_identical(model@paramUpper, NULL)
  validObject(model)

  lin<-function(x,par){par[1]+x*par[2]}

  par<-c(1,3);
  x<-c(1)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x, y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1]))

  par<-c(-3,4);
  x<-c(1,7)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1]))
  expect_equal(model@gradient(x[2], par), c(1, x[2]))

  par<-c(-3,0);
  x<-c(1,7)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1]))
  expect_equal(model@gradient(x[2], par), c(1, x[2]))

  par<-c(0,4);
  x<-c(1,7)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1]))
  expect_equal(model@gradient(x[2], par), c(1, x[2]))

  par<-c(0,0);
  x<-c(1,7)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1]))
  expect_equal(model@gradient(x[2], par), c(1, x[2]))

  par<-c(0.1,0.6);
  x<-c(1,8,9)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1]))
  expect_equal(model@gradient(x[2], par), c(1, x[2]))
  expect_equal(model@gradient(x[3], par), c(1, x[3]))

  par<-c(2,5);
  x<-c(-4,5,3,45)
  y<-lin(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(lin(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1]))
  expect_equal(model@gradient(x[2], par), c(1, x[2]))
  expect_equal(model@gradient(x[3], par), c(1, x[3]))
  expect_equal(model@gradient(x[4], par), c(1, x[4]))

  yn <- y + 0.0001*rnorm(length(y))
  est<-model@estimator(x,yn)
  expect_identical(sum(abs(est-par)) < 0.1, TRUE)

  x <- rnorm(1000);
  yn <- lin(x, par) + 0.0001*rnorm(length(y))
  est<-model@estimator(x,yn)
  expect_identical(sum(abs(est-par)) < 0.1, TRUE)
})
