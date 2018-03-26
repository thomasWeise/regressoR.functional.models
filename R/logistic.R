#' @include internalEstimateSampler.R

# a+b/(1+c*x^d)
.logistic <- function(x, par) par[1L] + par[2L] / (1L + par[3L] * x ^ par[4L])

# the gradient
.logistic.gradient <- function(x, par) {
  xd <- x^par[4L];
  cxd <- par[3L] * xd;
  vl1 <- (1L + cxd+cxd + cxd*cxd);
  c(1L,
    1L / (1L + cxd),
    (-(par[2L] * xd) / vl1),
    (-(par[3L] * log(x) * par[2L] * xd) / vl1));
}

.paramLower.1 <- c(-1000L,  1e-15, 1e-15,  1e-15)
.paramUpper.1 <- c( 1000L,  1000L, 1000L,  1000L)
.sampler.1    <- function() c(rnorm(n=1),  abs(rnorm(n=1)) + 1e-15,
                                           abs(rnorm(n=1)) + 1e-15,
                                           abs(rnorm(n=1)) + 1e-15)
.paramLower.2 <- c(-1000L, -1000L, 1e-15, -1000L)
.paramUpper.2 <- c( 1000L, -1e-15, 1000L, -1e-15)
.sampler.2    <- function() c(rnorm(n=1), -abs(rnorm(n=1)) - 1e-15,
                                           abs(rnorm(n=1)) + 1e-15,
                                          -abs(rnorm(n=1)) - 1e-15)

.logistic.estimate.1 <- function(x, y)
  .estimate.4p.internal(x, y, .paramLower.1, .paramUpper.1,
                              .sampler.1, .logistic)

.logistic.estimate.2 <- function(x, y)
  .estimate.4p.internal(x, y, .paramLower.2, .paramUpper.2,
                              .sampler.2, .logistic)


# The internal constant for the first variant of the logistic model
# \code{a+b/(1+c*x^d)}
.logistic.1 <- FunctionalModel.new(
  f = .logistic,
  gradient = .logistic.gradient,
  paramCount = 4L,
  estimator = .logistic.estimate.1,
  paramLower = c(NA, 1e-15,  1e-15, 1e-15)
)

# The internal constant for the second variant of the logistic model
# \code{a+b/(1+c*x^d)}
.logistic.2 <- FunctionalModel.new(
  f = .logistic,
  gradient = .logistic.gradient,
  paramCount = 4L,
  estimator = .logistic.estimate.2,
  paramLower = c(NA, NA, 1e-15, NA),
  paramUpper = c(NA, -1e-15, NA, -1e-15)
)

#' @title Obtain the First Variant of the Logistic Model
#' @description This function returns the first variant of the logistic model
#'   \code{a+b/(1+c*x^d)} where both \code{b} and \code{d} are enforced to be
#'   positive.
#' @export FunctionalModel.logistic.1
#' @seealso FunctionalModel.logistic.2
FunctionalModel.logistic.1 <- function() .logistic.1


#' @title Obtain the Second Variant of the Logistic Model
#' @description This function returns the first variant of the logistic model
#'   \code{a+b/(1+c*x^d)} where both \code{b} and \code{d} are enforced to be
#'   negative.
#' @export FunctionalModel.logistic.2
#' @seealso FunctionalModel.logistic.1
FunctionalModel.logistic.2 <- function() .logistic.2

