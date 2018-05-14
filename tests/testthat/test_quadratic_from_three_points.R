library("regressoR.functional.models")
context("FunctionalModel.quadratic")


test_that("Test FunctionalModel.quadratic.from.three.points", {
  f   <- function(x, par) par[1] + (par[2] * x) + (par[3] * x * x);

  par <- c(1, 2, 3);
  expect_equal(par, FunctionalModel.quadratic.from.three.points(3, f(3, par), 4, f(4, par), 7, f(7, par)));

  par <- c(-3, 0, 2);
  expect_equal(par, FunctionalModel.quadratic.from.three.points(2, f(2, par), 5, f(5, par), 8, f(8, par)));

  par <- c(0, 5, -3);
  expect_equal(par, FunctionalModel.quadratic.from.three.points(-4, f(-4, par), 6, f(6, par), 7, f(7, par)));

  par <- c(7, -7, 5);
  expect_equal(par, FunctionalModel.quadratic.from.three.points(7, f(7, par), -7, f(-7, par), -9, f(-9, par)));
})
