#' @include FunctionalModel.R
#' @include utils.R

# Compute the coordinates of a linear function from two point coordinates
.linear.from.two.points <- function(x1, y1, x2, y2) {
  if(y2 == y1) {
    return(c(y2, 0));
  }
  m <- (y2-y1)/(x2-x1);
  if(is.finite(m)) {
    n <- y1-(m*x1);
    if(!(is.finite(n))) {
      n <- y2-(m*x2);
    }
    if(is.finite(n)) {
      return(c(n, m));
    }
  }
  return(NULL);
}

# the linear estimator function
.linear.estimator <- function(x, y, paramLower=NULL, paramUpper=NULL) {
  len <- length(x);
  res <- NULL;
  if(len>2L){
    mid <- as.integer(len/2)
    res <- .linear.from.two.points(mean(x[1:mid]), mean(y[1:mid]),
                                   mean(x[(mid+1):len]), mean(y[(mid+1):len]));
  }
  if((len >= 2L) && (is.null(res))){
    res <- .linear.from.two.points(x[1], y[1], x[len], y[len]);
    if(is.null(res)) {
      res <- .linear.from.two.points(x[1], y[1], x[2], y[2]);
    }
  }

  #    if(len > 2) {
  #      .ignore.errors({
  #        if(is.null(res)) {
  #          refined <- stats::nls(y ~ a*x + b, data=list(x=x, y=y));
  #        } else {
  #          refined <- stats::nls(y ~ a*x + b, data=list(x=x, y=y), start=list(a=res[2], b=res[1]));
  #        }
  #        if(!(is.null(refined))) {
  #          if(refined$convInfo$isConv) {
  #            par <- refined$m$getPars();
  #            nres <- c(par[["b"]], par[["a"]]);
  #            if(is.finite(nres[1]) && is.finite(nres[2])) {
  #              return(nres);
  #            }
  #          }
  #        }
  #      });
  #    }

  if(!(is.null(res))) {
    return(res);
  }

  if(len >= 1L){
    return(c(mean(y), 0));
  }
  return(c(0, 0));
}

# The internal constant for linear models
#' @importFrom stats nls
.linear <- FunctionalModel.new(
  f = function(x, par) par[1] + (par[2] * x),
  paramCount = 2L,
  estimator = .linear.estimator,
  gradient = function(x, par) c(1, x)
)

#' @title Obtain the Simple Linear Model \code{y = f(x) = a+b*x}
#' @description A simple linear model, i.e., a model of the form \code{y = f(x)
#'   = a+b*x} with two parameters (\code{a} and \code{b})).
#' @export FunctionalModel.linear
FunctionalModel.linear <- function() .linear
