#' @include FunctionalModel.R
#' @include parCheck.R
#' @include parFix.R
#' @include utils.R

#' @title Create a Parameter Estimate for a Given Functional Model
#' @description Use the data available in the functional functional model
#'   together with the \code{x} and \code{y} coordinates to be modeled to
#'   produce an initial first guess of the parameters. This method should not be
#'   confused with actually fitting the model. It won't do that, it just
#'   provides a reasonable initial guess. It therefore uses the estimator
#'   function provided by the functional model, if any, and then attempts to fix
#'   any parameter values violating the constraints.
#' @param model the model function functional model
#' @param x the input values
#' @param y the corresponding output values
#' @param par an existing estimate, or \code{NULL} if no previous estimate
#'   exists
#' @return the parameter estimate
#' @export FunctionalModel.par.estimate
#' @importFrom stats rnorm runif
FunctionalModel.par.estimate <- function(model, x=NULL, y=NULL, par=NULL) {
  # If an acceptable estimate is already given, use this estimate
  if(FunctionalModel.par.check(model, par)) {
    return(par);
  }

  count <- model@paramCount;

  # It seems as if we cannot just use off-the-shelf Gaussian random numbers.
  # Obtain the lower boundaries
  paramLower <- model@paramLower;
  if(is.null(paramLower)) {
    paramLower <- rep(NA, count);
  } else {
    paramLower[!is.finite(paramLower)] <- NA;
  }

  # Obtain the lower boundaries
  paramUpper <- model@paramUpper;
  if(is.null(paramUpper)) {
    paramUpper <- rep(NA, count);
  } else {
    paramUpper[!is.finite(paramUpper)] <- NA;
  }

  # Check if the functional model defines an estimator function and there is
  # data that can be used for estimating.
  if(!(is.null(x) ||
       is.null(y) ||
       is.null(model@estimator))) {
    # The estimator function is defined, let's try using it.
    estimate <- NULL;
    .ignore.errors(
        estimate <- model@estimator(x=x, y=y)
    );
    if(FunctionalModel.par.check(model, estimate)) {
      # The estimator function returned reasonable values, use them!
      return(estimate);
    }
  }

  # OK, let's see whether we can use Gaussian random numbers for initialization.
  if( (is.null(model@paramLower) || all(model@paramLower < 1)) &&
      (is.null(model@paramUpper) || all(model@paramUpper > (-1))) ) {
    # It seems that there is a reasonable chance for that, so let us try 5*count times.
    for(i in 1:5*count) {
      # Generate the Gaussian distributed random vector.
      estimate <- stats::rnorm(n=count);
      if(FunctionalModel.par.check(model, estimate)) {
        # A valid vector has been generated, let's try to use it.
        return(estimate);
      }
    }
  }

  # This function is used to sample the elements of the estimate vector.
  # It allows for boundaries being defined or undefined on different elements.
  .sample <- function(x) {
    lower <- paramLower[x];
    upper <- paramUpper[x];
    if(is.na(lower)) {
      if(is.na(upper)) {
        # Neither a lower nor an upper bound exists.
        # We simply use Gaussian distributed random numbers.
        return(stats::rnorm(n=1));
      } else {
        # An upper bound exists, but no lower bound.
        # We try to sample numbers whose distances from the upper bound are
        # absolute normally distributed with standard deviation upper.
        return(upper * (1 - (sign(upper) * abs(stats::rnorm(n=1)))));
      }
    } else {
      if(is.na(upper)) {
        # A lower bound exists, but no upper bound.
        # We try to sample numbers whose distances from the lower bound are
        # absolute normally distributed with standard deviation lower.
        return(lower * (1 + (sign(lower) * abs(stats::rnorm(n=1)))));
      } else {
        if(lower >= upper) {
          return(lower);
        }
        # A lower and an upper bound both exist.
        # We sample numbers uniformly distributed between both bounds.
        return(stats::runif(n=1, min=lower, max=upper));
      }
    }
  };

  # Let's use our above sampling technique at most 50*count times.
  range <- 1:count;
  for(i in 1:50*count) {
    estimate <- vapply(X=range, FUN=.sample, FUN.VALUE = NaN);
    if(FunctionalModel.par.check(model, estimate)) {
      return(estimate);
    }
  }

  # OK, if we get here, we entirely failed to generate a reasonable parameter vector.
  # We just fall back to Gaussian distributed random numbers and hope that it
  # can automatically be fixed.
  return(FunctionalModel.par.fix(par=stats::rnorm(n=count), lower=paramLower, upper=paramUpper, paramCount=count));
}
