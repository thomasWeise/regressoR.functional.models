library("regressoR.functional.models")
context("FunctionalModel.cubic")




test_that("Test FunctionalModel.cubic", {
  model <- FunctionalModel.cubic();
  expect_identical(is.null(model), FALSE);
  expect_identical(is(model, "FunctionalModel"), TRUE)
  expect_identical(model@paramCount, 4L)
  expect_identical(model@paramLower, NULL)
  expect_identical(model@paramUpper, NULL)
  validObject(model)

  cube<-function(x,params){params[1]+x*params[2]+x*x*params[3]+x*x*x*params[4]}

  par<-c(1,3,-0.4,2);
  x<-c(1)
  y<-cube(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(cube(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2, x[1]^3))

  par<-c(-3,4,2,0.5);
  x<-c(1,7)
  y<-cube(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(cube(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2, x[1]^3))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2, x[2]^3))

  par<-c(-3,0,1,0.6);
  x<-c(1,7,2)
  y<-cube(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(cube(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2, x[1]^3))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2, x[2]^3))
  expect_equal(model@gradient(x[3], par), c(1, x[3], x[3]^2, x[3]^3))

  par<-c(-3,2,0,5);
  x<-c(1,7,2)
  y<-cube(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(cube(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2, x[1]^3))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2, x[2]^3))
  expect_equal(model@gradient(x[3], par), c(1, x[3], x[3]^2, x[3]^3))


  par<-c(0,4,1,7);
  x<-c(1,7)
  y<-cube(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(cube(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2, x[1]^3))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2, x[2]^3))

  par<-c(0,0,0,0);
  x<-c(1,7)
  y<-cube(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(cube(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2, x[1]^3))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2, x[2]^3))

  par<-c(0.1,0.6,0.5,0.4);
  x<-c(1,8,9)
  y<-cube(x,par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(cube(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2, x[1]^3))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2, x[2]^3))
  expect_equal(model@gradient(x[3], par), c(1, x[3], x[3]^2, x[3]^3))

  par<-c(1,0.5,0.7,4);
  x<-rnorm(123);
  y<-cube(x, par)
  expect_equal(model@f(x, par), y)
  est<-model@estimator(x,y)
  expect_equal(cube(x, est), y)
  expect_equal(model@gradient(x[1], par), c(1, x[1], x[1]^2, x[1]^3))
  expect_equal(model@gradient(x[2], par), c(1, x[2], x[2]^2, x[2]^3))
  expect_equal(model@gradient(x[3], par), c(1, x[3], x[3]^2, x[3]^3))

  yn<-y+0.001*rnorm(length(y))
  est<-model@estimator(x,y)
  expect_identical( sum(abs(est-par)) < 0.02, TRUE)
})

test_that("Test .cubic.from.four.points", {
  f   <- function(x, par) par[1] + (par[2] * x) + (par[3] * x * x) + (par[4] * x * x * x);

  par <- c(1, 2, 3, -4);
  expect_equal(par, .cubic.from.four.points(3, f(3, par), 4, f(4, par), 7, f(7, par), 5, f(5, par)));

  par <- c(-3, 0, 2, 3);
  expect_equal(par, .cubic.from.four.points(2, f(2, par), 5, f(5, par), 8, f(8, par), 0, f(0, par)));

  par <- c(0, 5, -3, 6);
  expect_equal(par, .cubic.from.four.points(-4, f(-4, par), 6, f(6, par), 7, f(7, par), 23, f(23, par)));

  par <- c(7, -7, 5, 0.3);
  expect_equal(par, .cubic.from.four.points(7, f(7, par), -7, f(-7, par), -9, f(-9, par), 0.7, f(0.7, par)));
})
