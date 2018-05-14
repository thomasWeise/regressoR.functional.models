library("regressoR.functional.models")
context("FunctionalModel.linear.from.two.points")


test_that("Test FunctionalModel.linear.from.two.points", {

  f <- function(x, p) p[1] + p[2]*x
  p <- c(4, -2);
  expect_equal(FunctionalModel.linear.from.two.points(1, f(1, p), 2, f(2, p)), p);
  expect_equal(FunctionalModel.linear.from.two.points(0, f(0, p), 2, f(2, p)), p);
  expect_equal(FunctionalModel.linear.from.two.points(-2, f(-2, p), 2, f(2, p)), p);

  p <- c(0, -2);
  expect_equal(FunctionalModel.linear.from.two.points(1, f(1, p), 2, f(2, p)), p);
  expect_equal(FunctionalModel.linear.from.two.points(0, f(0, p), 2, f(2, p)), p);
  expect_equal(FunctionalModel.linear.from.two.points(-2, f(-2, p), 2, f(2, p)), p);

  p <- c(3, 0);
  expect_equal(FunctionalModel.linear.from.two.points(1, f(1, p), 2, f(2, p)), p);
  expect_equal(FunctionalModel.linear.from.two.points(0, f(0, p), 2, f(2, p)), p);
  expect_equal(FunctionalModel.linear.from.two.points(-2, f(-2, p), 2, f(2, p)), p);
})


test_that("Test FunctionalModel.linear.from.two.points", {
  f   <- function(x, par) par[1] + (par[2] * x);

  par <- c(1, 2);
  expect_equal(par, FunctionalModel.linear.from.two.points(3, f(3, par), 4, f(4, par)));

  par <- c(-3, 0);
  expect_equal(par, FunctionalModel.linear.from.two.points(2, f(2, par), 5, f(5, par)));

  par <- c(0, 5);
  expect_equal(par, FunctionalModel.linear.from.two.points(-4, f(-4, par), 6, f(6, par)));

  par <- c(7, -7);
  expect_equal(par, FunctionalModel.linear.from.two.points(7, f(7, par), -7, f(-7, par)));
})
