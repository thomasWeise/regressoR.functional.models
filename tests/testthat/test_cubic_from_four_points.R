library("regressoR.functional.models")
context("FunctionalModel.cubic")


test_that("Test FunctionalModel.cubic.from.four.points", {
  f   <- function(x, par) par[1] + (par[2] * x) + (par[3] * x * x) + (par[4] * x * x * x);

  par <- c(1, 2, 3, -4);
  expect_equal(par, FunctionalModel.cubic.from.four.points(3, f(3, par), 4, f(4, par), 7, f(7, par), 5, f(5, par)));

  par <- c(-3, 0, 2, 3);
  expect_equal(par, FunctionalModel.cubic.from.four.points(2, f(2, par), 5, f(5, par), 8, f(8, par), 0, f(0, par)));

  par <- c(0, 5, -3, 6);
  expect_equal(par, FunctionalModel.cubic.from.four.points(-4, f(-4, par), 6, f(6, par), 7, f(7, par), 23, f(23, par)));

  par <- c(7, -7, 5, 0.3);
  expect_equal(par, FunctionalModel.cubic.from.four.points(7, f(7, par), -7, f(-7, par), -9, f(-9, par), 0.7, f(0.7, par)));
})
