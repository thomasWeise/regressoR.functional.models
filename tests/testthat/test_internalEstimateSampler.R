library("regressoR.functional.models")
context("internalEstimateSampler")

test_that(".solve.np", {
  x          <- runif(n=20);
  f          <- function(x, par) par[1] + par[2]*x;
  par        <- c(4, 3);
  f.used     <- function(x) f(x, par);
  y          <- f.used(x);
  sampler     <- function() rnorm(n=2);
  paramLower <- c(-1000L, -1000L);
  paramUpper <- c( 1000L,  1000L);
  n          <- length(par);

  res <- .solve.np(x, y, paramLower, paramUpper, sampler, f, x);

  expect_true(!(is.null(res)));
  expect_length(res, n);
  for(i in 1:n) {
    expect_lt(abs(res[i]-par[i]), 0.01);
  }
})



test_that(".estimate.internal (1)", {
  x          <- runif(n=20);
  f          <- function(x, par) par[1] + par[2]*x;
  par        <- c(4, 3);
  f.used     <- function(x) f(x, par);
  y          <- f.used(x);
  n          <- length(par);
  sampler     <- function() rnorm(n=n);
  paramLower <- c(-1000L, -1000L);
  paramUpper <- c( 1000L,  1000L);

  res <- .estimate.internal(x, y, paramLower, paramUpper, sampler, f, n);
  expect_true(!(is.null(res)));
  expect_length(res, 2);
  expect_lt(abs(res[1]-par[1]), 1);
  expect_lt(abs(res[2]-par[2]), 1);
})



test_that(".estimate.internal (2)", {
  x          <- runif(n=20);
  f          <- function(x, par) par[1] + par[2]*x + par[3]*sin(x);
  par        <- c(4, 3, 12);
  f.used     <- function(x) f(x, par);
  y          <- f.used(x);
  n          <- length(par);
  sampler    <- function() {t <- rnorm(n=n); t[3] <- abs(t[3]) + 0.01; t}
  paramLower <- c(-1000L, -1000L, 0.001);
  paramUpper <- c( 1000L,  1000L, 1000L);

  res <- .estimate.internal(x, y, paramLower, paramUpper, sampler, f, n);
  expect_true(!(is.null(res)));
  expect_length(res, n);
  for(i in 1:n) {
    expect_lt(abs(res[i]-par[i]), 10);
  }
})



test_that(".estimate.internal (3)", {
  x          <- runif(n=1220);
  f          <- function(x, par) atan(par[1] + par[2]*x + par[3]*sin(x)) + par[4];
  par        <- c(4, 3, 12, 0.5);
  f.used     <- function(x) f(x, par);
  y          <- f.used(x);
  n          <- length(par);
  sampler    <- function() {t <- rnorm(n=n); t[3] <- abs(t[3]) + 0.01; t[4] <- -abs(t[4]) + 0.89999; t}
  paramLower <- c(-1000L, -1000L, 0.001, -1000L);
  paramUpper <- c( 1000L,  1000L, 1000L, 0.9);

  res <- .estimate.internal(x, y, paramLower, paramUpper, sampler, f, 4);
  expect_true(!(is.null(res)));
  expect_length(res, n);
  for(i in 1:n) {
    expect_lt(abs(res[i]-par[i]), 20);
  }
})
