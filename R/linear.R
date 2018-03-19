#' @include FunctionalModel.R
#' @include utils.R

# Compute the coordinates of a linear function from two point coordinates
.linear.from.two.points <- function(x1, y1, x2, y2) {
  if(y2 == y1) {
    return(base::c(y2, 0));
  }
  m <- (y2-y1)/(x2-x1);
  if(base::is.finite(m)) {
    n <- y1-(m*x1);
    if(!(base::is.finite(n))) {
      n <- y2-(m*x2);
    }
    if(base::is.finite(n)) {
      return(base::c(n, m));
    }
  }
  return(NULL);
}

# The internal constant for linear models
#' @importFrom stats nls
.linear <- FunctionalModel.new(
  f = function(x, par) par[1] + (par[2] * x),
  paramCount = 2L,
  estimator = function(x, y, paramLower=NULL, paramUpper=NULL) {
    len <- base::length(x);
    res <- NULL;
    if(len>2L){
      mid <- base::as.integer(len/2)
      res <- .linear.from.two.points(base::mean(x[1:mid]), base::mean(y[1:mid]),
                                     base::mean(x[(mid+1):len]), base::mean(y[(mid+1):len]));
    }
    if((len >= 2L) && (base::is.null(res))){
      res <- .linear.from.two.points(x[1], y[1], x[len], y[len]);
      if(base::is.null(res)) {
        res <- .linear.from.two.points(x[1], y[1], x[2], y[2]);
      }
    }

    if(len > 2) {
      .ignore.errors({
        if(base::is.null(res)) {
          refined <- stats::nls(y ~ a*x + b, data=base::list(x=x, y=y));
        } else {
          refined <- stats::nls(y ~ a*x + b, data=base::list(x=x, y=y), start=base::list(a=res[2], b=res[1]));
        }
        if(!(base::is.null(refined))) {
          if(refined$convInfo$isConv) {
            par <- refined$m$getPars();
            nres <- base::c(par[["b"]], par[["a"]]);
            if(base::is.finite(nres[1]) && base::is.finite(nres[2])) {
              return(nres);
            }
          }
        }
      });
    }

    if(!(base::is.null(res))) {
      return(res);
    }

    if(len >= 1L){
      return(base::c(base::mean(y), 0));
    }
    return(c(0, 0));
  },
  gradient = function(x, par) base::c(1, x)
)

#' @title Obtain the Simple Linear Model \code{y = f(x) = a+b*x}
#' @description A simple linear model, i.e., a model of the form \code{y = f(x)
#'   = a+b*x} with two parameters (\code{a} and \code{b})).
#' @export linear
linear <- function() .linear