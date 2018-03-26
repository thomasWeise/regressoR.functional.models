#' @include internalEstimateSampler.R
#' @include exponentialDecay.R

# a+(b*exp(c*x^d))
.gompertz <- function(x, par) par[1L] + (par[2L] * exp(par[3L] * exp(par[4L] * x)))

# the gradient
.gompertz.gradient <- function(x, par) {
  cedx <- par[3L] * exp(par[4L]*x);
  vl1  <- par[2L] * exp(cedx+par[4L]*x);
  c(1, exp(cedx), vl1, par[3L]*x*vl1)
}

.gompertz.paramLower.1 <- .exponentialDecay.paramLower.1
#                         c(-1000L,  1e-15, -1000L,  1e-15)
.gompertz.paramUpper.1 <- .exponentialDecay.paramUpper.1
#                         c( 1000L,  1000L, -1e-15,  1000L)
.gompertz.sampler.1    <- .exponentialDecay.sampler.1
#                         function() c(     rnorm(n=1),
#                                       abs(rnorm(n=1)) + 1e-15,
#                                      -abs(rnorm(n=1)) - 1e-15,
#                                       abs(rnorm(n=1)) + 1e-15)
.gompertz.paramLower.2 <- .exponentialDecay.paramLower.2
#                          c(-1000L, -1000L, -1000L, -1000L)
.gompertz.paramUpper.2 <- .exponentialDecay.paramUpper.2
#                          c( 1000L, -1e-15, -1e-15, -1e-15)
.gompertz.sampler.2    <- .exponentialDecay.sampler.2
#                          function() c(     rnorm(n=1),
#                                       -abs(rnorm(n=1)) - 1e-15,
#                                       -abs(rnorm(n=1)) - 1e-15,
#                                       -abs(rnorm(n=1)) - 1e-15)

.gompertz.estimate.1 <- function(x, y)
  .estimate.4p.internal(x, y, .gompertz.paramLower.1,
                              .gompertz.paramUpper.1,
                              .gompertz.sampler.1,
                              .gompertz)

.gompertz.estimate.2 <- function(x, y)
  .estimate.4p.internal(x, y, .gompertz.paramLower.2,
                              .gompertz.paramUpper.2,
                              .gompertz.sampler.2,
                              .gompertz)



# The internal constant for the first variant of the Gompertz model
.gompertz.1 <- FunctionalModel.new(
  f          = .gompertz,
  gradient   = .gompertz.gradient,
  paramCount = 4L,
  estimator  = .gompertz.estimate.1,
  paramLower = c(NA, 1e-15,     NA, 1e-15),
  paramUpper = c(NA,    NA, -1e-15,    NA)
)

# The internal constant for the second variant of the Gompertz model
.gompertz.2 <- FunctionalModel.new(
  f          = .gompertz,
  gradient   = .gompertz.gradient,
  paramCount = 4L,
  estimator  = .gompertz.estimate.2,
  paramUpper = c(NA, -1e-15, -1e-15, -1e-15)
)

#' @title Obtain the First Variant of the Gompertz Model
#' @description This function returns the first variant of the generalized
#'   Gompertz model \code{a+(b*exp(c*exp(d*x)))} where both \code{b} and
#'   \code{d} are enforced to be positive.
#' @export FunctionalModel.gompertz.1
#' @seealso FunctionalModel.gompertz.2
FunctionalModel.gompertz.1 <- function() .gompertz.1


#' @title Obtain the Second Variant of the Gompertz Model
#' @description This function returns the first variant of the generalized
#'   Gompertz model \code{a+(b*exp(c*exp(d*x)))} where both \code{b} and
#'   \code{d} are enforced to be negative.
#' @export FunctionalModel.gompertz.2
#' @seealso FunctionalModel.gompertz.1
FunctionalModel.gompertz.2 <- function() .gompertz.2

