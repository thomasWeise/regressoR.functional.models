#' @include FunctionalModel.R

#' @title Compute the parameter values of a Linear Function from two Point
#'   Coordinates
#' @description This function returns the two parameters of a linear model from
#'   two point coordinates.
#' @param x1 the first \code{x}-coordinate
#' @param y1 the first \code{y}-coordinate
#' @param x2 the second \code{x}-coordinate
#' @param y2 the second \code{y}-coordinate
#' @return a vector of type \code{(m,n)}, such than \code{f(x)=n+m*x} or
#'   \code{NULL} if no finite result is possible
#' @export FunctionalModel.linear.from.two.points
FunctionalModel.linear.from.two.points <- function(x1, y1, x2, y2) {
  if(y2 == y1) {
    return(c(y2, 0L));
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
.linear.estimator <- function(x, y) {
  len <- length(x);
  res <- NULL;
  if(len > 2L){
    mid <- as.integer(len/2L)
    res <- FunctionalModel.linear.from.two.points(mean(x[1L:mid]), mean(y[1L:mid]),
                                   mean(x[(mid+1L):len]), mean(y[(mid+1L):len]));
    if(!is.null(res)) {
      return(res);
    }
  }
  if(len >= 2L) {
    res <- FunctionalModel.linear.from.two.points(x[1L], y[1L], x[len], y[len]);
    if(!is.null(res)) {
      return(res);
    }

    res <- FunctionalModel.linear.from.two.points(x[1L], y[1L], x[2L], y[2L]);
    if(!is.null(res)) {
      return(res);
    }
  }

  if(len >= 1L){
    return(c(mean(y), 0L));
  }

  return(c(0L, 0L));
}

# The internal constant for linear models
.linear <- FunctionalModel.new(
  f = function(x, par) par[1L] + (par[2L] * x),
  paramCount = 2L,
  estimator = .linear.estimator,
  gradient = function(x, par) c(1, x),
  name       = "Linear Function"
)

#' @title Obtain the Simple Linear Model \code{y = f(x) = a+b*x}
#' @description A simple linear model, i.e., a model of the form \code{y = f(x)
#'   = a+b*x} with two parameters (\code{a} and \code{b})).
#' @export FunctionalModel.linear
FunctionalModel.linear <- function() .linear
