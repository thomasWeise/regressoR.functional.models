#' @include internalEstimateSampler.R

# a+(b*exp(c*x^d))
.exponentialDecay <- function(x, par) par[1L] + (par[2L] * exp(par[3L] * (x ^ par[4L])))

# the gradient
.exponentialDecay.gradient <- function(x, par) {
  x4 <- x^par[4L];
  g2 <- exp(par[3L]*x4);
  g3 <- par[2L]*x4*g2;
  c(1, g2, g3, par[3L]*g3*log(x))
}

.exponentialDecay.paramLower.1 <- c(-1000L,  1e-15, -1000L,  1e-15)
.exponentialDecay.paramUpper.1 <- c( 1000L,  1000L, -1e-15,  1000L)
.exponentialDecay.sampler.1    <- function() c(     rnorm(n=1),
                                                abs(rnorm(n=1)) + 1e-15,
                                               -abs(rnorm(n=1)) - 1e-15,
                                                abs(rnorm(n=1)) + 1e-15)
.exponentialDecay.paramLower.2 <- c(-1000L, -1000L, -1000L, -1000L)
.exponentialDecay.paramUpper.2 <- c( 1000L, -1e-15, -1e-15, -1e-15)
.exponentialDecay.sampler.2    <- function() c(     rnorm(n=1),
                                               -abs(rnorm(n=1)) - 1e-15,
                                               -abs(rnorm(n=1)) - 1e-15,
                                               -abs(rnorm(n=1)) - 1e-15)

.exponentialDecay.estimate.1 <- function(x, y)
  .estimate.4p.internal(x, y, .exponentialDecay.paramLower.1,
                              .exponentialDecay.paramUpper.1,
                              .exponentialDecay.sampler.1,
                              .exponentialDecay)

.exponentialDecay.estimate.2 <- function(x, y)
  .estimate.4p.internal(x, y, .exponentialDecay.paramLower.2,
                              .exponentialDecay.paramUpper.2,
                              .exponentialDecay.sampler.2,
                              .exponentialDecay)



# The internal constant for the first variant of the exponential decay model
.exponentialDecay.1 <- FunctionalModel.new(
  f          = .exponentialDecay,
  gradient   = .exponentialDecay.gradient,
  paramCount = 4L,
  estimator  = .exponentialDecay.estimate.1,
  paramLower = c(NA, 1e-15,     NA, 1e-15),
  paramUpper = c(NA,    NA, -1e-15,    NA)
)

# The internal constant for the second variant of the exponential decay model
.exponentialDecay.2 <- FunctionalModel.new(
  f          = .exponentialDecay,
  gradient   = .exponentialDecay.gradient,
  paramCount = 4L,
  estimator  = .exponentialDecay.estimate.2,
  paramUpper = c(NA, -1e-15, -1e-15, -1e-15)
)

#' @title Obtain the First Variant of the Generalized Exponential Decay Model
#' @description This function returns the first variant of the generalized
#'   exponential decay model \code{a+(b*exp(c*x^d)} where both \code{b} and
#'   \code{d} are enforced to be positive.
#' @export FunctionalModel.exponentialDecay.1
#' @seealso FunctionalModel.exponentialDecay.2
FunctionalModel.exponentialDecay.1 <- function() .exponentialDecay.1


#' @title Obtain the Second Variant of the Generalized Exponential Decay Model
#' @description This function returns the first variant of the generalized
#'   exponential decay model \code{a+(b*exp(c*x^d)} where both \code{b} and
#'   \code{d} are enforced to be negative.
#' @export FunctionalModel.exponentialDecay.2
#' @seealso FunctionalModel.exponentialDecay.1
FunctionalModel.exponentialDecay.2 <- function() .exponentialDecay.2

