#' @include internalEstimateSampler.R

# a+(b*exp(c*x^d))
.exp.decay <- function(x, par) par[1L] + (par[2L] * exp(par[3L] * (x ^ par[4L])))

# the gradient
.exp.decay.gradient <- function(x, par) {
  x4 <- x^par[4L];
  g2 <- exp(par[3L]*x4);
  g3 <- par[2L]*x4*g2;
  c(1, g2, g3, par[3L]*g3*log(x))
}

.paramLower.1 <- c(-1000L,  1e-15, -1000L,  1e-15)
.paramUpper.1 <- c( 1000L,  1000L, -1e-15,  1000L)
.sampler.1    <- function() c(rnorm(n=1),  abs(rnorm(n=1)) + 1e-15, -abs(rnorm(n=1)) - 1e-15,  abs(rnorm(n=1)) + 1e-15)
.paramLower.2 <- c(-1000L, -1000L, -1000L, -1000L)
.paramUpper.2 <- c( 1000L, -1e-15, -1e-15, -1e-15)
.sampler.2    <- function() c(rnorm(n=1), -abs(rnorm(n=1)) - 1e-15, -abs(rnorm(n=1)) - 1e-15, -abs(rnorm(n=1)) - 1e-15)

.exp.decay.estimate.1 <- function(x, y)
  .estimate.4p.internal(x, y, .paramLower.1, .paramUpper.1, .sampler.1, .exp.decay)

.exp.decay.estimate.2 <- function(x, y)
  .estimate.4p.internal(x, y, .paramLower.2, .paramUpper.2, .sampler.2, .exp.decay)



# The internal constant for the first variant of the exponential decay model
.exp.decay.1 <- FunctionalModel.new(
  f = .exp.decay,
  gradient = .exp.decay.gradient,
  paramCount = 4L,
  estimator = .exp.decay.estimate.1,
  paramLower = c(NA, 1e-15,     NA, 1e-15),
  paramUpper = c(NA,    NA, -1e-15,    NA)
)

# The internal constant for the second variant of the exponential decay model
.exp.decay.2 <- FunctionalModel.new(
  f = .exp.decay,
  gradient = .exp.decay.gradient,
  paramCount = 4L,
  estimator = .exp.decay.estimate.2,
  paramUpper = c(NA, -1e-15, -1e-15, -1e-15)
)

#' @title Obtain the First Variant of the Generalized Exponential Decay Model
#' @description This function returns the first variant of the generalized
#'   exponential decay model \code{a+(b*exp(c*x^d)} where both \code{b} and
#'   \code{d} are enforced to be positive.
#' @export FunctionalModel.exponentialDecay.1
#' @seealso FunctionalModel.exponentialDecay.2
FunctionalModel.exponentialDecay.1 <- function() .exp.decay.1


#' @title Obtain the Second Variant of the Generalized Exponential Decay Model
#' @description This function returns the first variant of the generalized
#'   exponential decay model \code{a+(b*exp(c*x^d)} where both \code{b} and
#'   \code{d} are enforced to be negative.
#' @export FunctionalModel.exponentialDecay.2
#' @seealso FunctionalModel.exponentialDecay.1
FunctionalModel.exponentialDecay.2 <- function() .exp.decay.2

