#' @include internalEstimateSampler.R

# a + b*exp(c*log(x+d))
.expLogLinear <- function(x, par) par[1L] + par[2L]*exp(par[3L]*log(x+par[4L]))

# the gradient
.expLogLinear.gradient <- function(x, par) {
  xd  <- x+par[4L];
  xdc <- (xd)^par[3L];
  c(1,
    xdc,
    par[2L]*xdc*log(xd),
    par[2L]*par[3L]*(xd^(par[3L]-1)))
}

.expLogLinear.paramLower.1 <- c(-1000L,  1e-15, -1000L, -1000L)
.expLogLinear.paramUpper.1 <- c( 1000L,  1000L, -1e-15,  1000L)
.expLogLinear.sampler.1    <- function() c(     rnorm(n=1),
                                            abs(rnorm(n=1)) + 1e-15,
                                           -abs(rnorm(n=1)) - 1e-15,
                                                rnorm(n=1))
.expLogLinear.paramLower.2 <- c(-1000L, -1000L, 1e-15, -1000L)
.expLogLinear.paramUpper.2 <- c( 1000L, -1e-15, 1000L,  1000L)
.expLogLinear.sampler.2    <- function() c(     rnorm(n=1),
                                           -abs(rnorm(n=1)) - 1e-15,
                                            abs(rnorm(n=1)) + 1e-15,
                                                rnorm(n=1))

.expLogLinear.estimate.1 <- function(x, y)
  .estimate.internal(x, y, .expLogLinear.paramLower.1,
                           .expLogLinear.paramUpper.1,
                           .expLogLinear.sampler.1,
                           .expLogLinear, 4L)

.expLogLinear.estimate.2 <- function(x, y)
  .estimate.internal(x, y, .expLogLinear.paramLower.2,
                           .expLogLinear.paramUpper.2,
                           .expLogLinear.sampler.2,
                           .expLogLinear, 4L)


# The internal constant for the first variant of the exponential log-linear model
.expLogLinear.1 <- FunctionalModel.new(
  f          = .expLogLinear,
  gradient   = .expLogLinear.gradient,
  paramCount = 4L,
  estimator  = .expLogLinear.estimate.1,
  paramLower = c(NA, 1e-15,     NA, NA),
  paramUpper = c(NA,    NA, -1e-15, NA),
  name       = "Exponential Log-Linear Model (1)"
)

# The internal constant for the second variant of the exponential log-linear model
.expLogLinear.2 <- FunctionalModel.new(
  f          = .expLogLinear,
  gradient   = .expLogLinear.gradient,
  paramCount = 4L,
  estimator  = .expLogLinear.estimate.2,
  paramLower = c(NA,     NA, 1e-15, NA),
  paramUpper = c(NA, -1e-15,    NA, NA),
  name       = "Exponential Log-Linear Model (2)"
)

#' @title Obtain the First Variant of the Exponential Log-Linear Model
#' @description This function returns the first variant of the generalized
#'   exponential log-linear model \code{a + b*exp(c*log(x+d))} where \code{b} is
#'   enforced to be positive and \code{c} is enforced to be negative.
#' @export FunctionalModel.expLogLinear.1
#' @seealso FunctionalModel.expLogLinear.2
FunctionalModel.expLogLinear.1 <- function() .expLogLinear.1


#' @title Obtain the Second Variant of the Exponential Log-Linear Model
#' @description This function returns the first variant of the generalized
#'   exponential log-linear model \code{a + b*exp(c*log(x+d))} where \code{b} is
#'   enforced to be negative and \code{c} is enforced to be positive.
#' @export FunctionalModel.expLogLinear.2
#' @seealso FunctionalModel.expLogLinear.1
FunctionalModel.expLogLinear.2 <- function() .expLogLinear.2

