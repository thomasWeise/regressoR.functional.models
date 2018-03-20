#' @include FunctionalModel.R

#' @title Fix a Vector to be Contained in the Lower/Upper Boundaries
#' @description Make sure that a vector consists of only finite values within
#'   the specified boundaries. This method can be used to clean the result of
#'   a stochastic parameter estimation step to make sure that all synthesized
#'   parameters are finite and within the specified boundaries.
#' @param par the parameter vector to fix
#' @param lower the lower boundaries, or \code{NULL} if no lower boundaries are
#'   specified
#' @param upper the upper boundaries, or \code{NULL} if no upper boundaries are
#'   specified
#' @param paramCount the parameter count, by default the length of \code{par}
#' @importFrom stats runif rnorm
#' @export FunctionalModel.par.fix
FunctionalModel.par.fix <- function(par, lower=NULL, upper=NULL, paramCount=base::length(par)) {
  for(i in 1:paramCount) {
    pari <- par[i];
    notFinite <- (!(base::is.finite(pari)));

    if(base::is.null(lower) || base::is.na(lower[i])) {
      # only consider upper bound
      if(base::is.null(upper) || base::is.na(upper[i])) {
        # but there also is no upper bound
        if(notFinite) {
          # replace non-finite value with normally distributed random number
          par[i] <- stats::rnorm(n=1);
        }
      } else {
        # upper bound is specified
        if(notFinite || (pari > upper[i])) {
          # we need to fix par
          if(upper[i] < 0) {
            # Upper bound is negative: need to create number with bigger absolute value
            par[i] <- (stats::runif(n=1, min=2*upper[i], max=upper[i]) - base::abs(stats::rnorm(n=1)));
          } else {
            if(upper[i] > 0) {
              # Upper bound is positive: need to create number with smaller absolute value
              par[i] <- (stats::runif(n=1, min=-upper[i], max=upper[i]) - base::abs(stats::rnorm(n=1)));
            } else {
              # Upper bound is 0: create negative normally distributed number
              par[i] <- -base::abs(stats::rnorm(n=1));
            }
          }
        }
      }
      #else no bound specified, we don't need to fix this point
    } else {
      if(base::is.null(upper) || base::is.na(upper[i])) {
        # only lower bound is relevant
        if(notFinite || (pari < lower[i])) {
          # we need to fix par
          if(lower[i] < 0) {
            # lower bound is negative: need to create number with smaller absolute value
            par[i] <- (stats::runif(n=1, min=lower[i], max=-lower[i]) + base::abs(stats::rnorm(n=1)));
          } else {
            if(lower[i] > 0) {
              # lower bound is positive: need to create number with larger absolute value
              par[i] <- (stats::runif(n=1, min=lower[i], max=2*lower[i]) + base::abs(stats::rnorm(n=1)));
            } else {
              # lower bound is 0: create positive normally distributed number
              par[i] <- base::abs(stats::rnorm(n=1));
            }
          }
        }
        # else: everything is ok
      } else {
        # lower and upper bound are relevant
        if(notFinite || (pari > upper[i]) || (pari < lower[i])) {
          if(lower[i] >= upper[i]) {
            # only a single value is permissible anyway
            par[i] <- lower[i];
          } else {
            # we need to fix par
            par[i] <- stats::runif(n=1, min=lower[i], max=upper[i]);
          }
        }
      }
    }
  }
  return(par);
}
