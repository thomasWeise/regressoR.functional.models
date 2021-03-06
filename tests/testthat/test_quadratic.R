library("regressoR.functional.models")
context("FunctionalModel.quadratic")

test_that("Test FunctionalModel.quadratic", {
  model <- FunctionalModel.quadratic();
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  expect_identical(model@paramCount, 3L)
  expect_identical(model@paramLower, NULL)
  expect_identical(model@paramUpper, NULL)
  validObject(model)

  quad<-function(x,params){params[1]+x*params[2]+x*x*params[3]}

  par<-c(1,3,-0.4);
  x<-c(1)
  y<-quad(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(quad(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2))

  par<-c(-3,4,2);
  x<-c(1,7)
  y<-quad(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(quad(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2))

  par<-c(-3,0,1);
  x<-c(1,7,2)
  y<-quad(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(quad(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2))

  par<-c(-3,2,0);
  x<-c(1,7,2)
  y<-quad(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(quad(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2))


  par<-c(0,4,1);
  x<-c(1,7)
  y<-quad(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(quad(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2))

  par<-c(0,0,0);
  x<-c(1,7)
  y<-quad(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(quad(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2))

  par<-c(0.1,0.6,0.5);
  x<-c(1,8,9)
  y<-quad(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(quad(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2))
  expect_equal(model@gradient(x[3], par), c(1, x[3], x[3]^2))

  par<-c(1,0.5,0.7);
  x<-rnorm(123);
  y<-quad(x, par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(quad(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2))
  expect_equal(model@gradient(x[3], par), c(1, x[3], x[3]^2))

  yn<-y+0.001*rnorm(length(y))
  est<-model@estimator(x,y)
  expect_identical( sum(abs(est-par)) < 0.02, TRUE)
})
