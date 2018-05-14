#' @include FunctionalModel.R
#' @include linear.R

#' @title Compute the parameter values of a Quadratic Function from three Point
#'   Coordinates
#' @description This function returns the three parameters of a quadratic model
#'   from three point coordinates.
#' @param x1 the first \code{x}-coordinate
#' @param y1 the first \code{y}-coordinate
#' @param x2 the second \code{x}-coordinate
#' @param y2 the second \code{y}-coordinate
#' @param x3 the third \code{x}-coordinate
#' @param y3 the third \code{y}-coordinate
#' @return a vector of type \code{(a,b,c)}, such than \code{f(x)=a+b*x+c*x^2} or
#'   \code{NULL} if no finite result is possible
#' @export FunctionalModel.quadratic.from.three.points
FunctionalModel.quadratic.from.three.points <- function(x1, y1, x2, y2, x3, y3) {
  x1s <- x1*x1;
  x2s <- x2*x2;
  x3s <- x3*x3;
  div <- (x1*(x3s-x2s)-x2*x3s+x2s*x3+x1s*(x2-x3));
  p1  <- (x1*(x3s*y2-x2s*y3)+x1s*(x2*y3-x3*y2)+(x2s*x3-x2*x3s)*y1)/div;
  p2  <- (-( (x1s*(y3-y2)-x2s*y3+x3s*y2+(x2s-x3s)*y1)/div));
  p3  <- (x1*(y3-y2)-x2*y3+x3*y2+(x2-x3)*y1)/div;
  if(is.finite(p1) && is.finite(p2) && is.finite(p3)) {
    return(c(p1, p2, p3));
  }
  return(NULL);
}

# The quadratic estimator function
.quadratic.estimator <- function(x, y) {
  len <- length(x);
  res <- NULL;

  if(len > 3L) {
    sres<-sapply(X=1:min(100L, len-3L),
                 FUN=function(x) {
                   sample <- sample.int(n=len, size=3L);
                   FunctionalModel.quadratic.from.three.points(x[sample[1L]], y[sample[1L]],
                                                x[sample[2L]], y[sample[2L]],
                                                x[sample[3L]], y[sample[3L]]);
                 });
    if( (!(is.null(sres))) && (length(sres)>0L) ) {
      if(is.null(dim(sres))) {
        sres <- sapply(sres[-which(sapply(sres, is.null))], rbind);
      }
      if( (!(is.null(sres))) && (length(sres)>0L) ) {
        res <- rowMeans(sres);
        if(is.finite(res[1L]) && is.finite(res[2L]) && is.finite(res[3L])) {
          return(res);
        }
      }
    }
  }

  if(len >= 3L) {
    res <- FunctionalModel.quadratic.from.three.points(x[1L], y[1L], x[len/2L+1L], y[len/2L+1L], x[len], y[len]);
    if(!is.null(res)) {
      return(res);
    }
    res <- FunctionalModel.quadratic.from.three.points(x[1L], y[1L], x[2L], y[2L], x[3L], y[3L]);
    if(!is.null(res)) {
      return(res);
    }
  }

  res <- .linear.estimator(x, y);
  if(is.null(res)) { return(NULL); }
  return(c(res, 0L));
}

# The internal constant for quadratic models
.quadratic <- FunctionalModel.new(
  f = function(x, par) par[1L] + (x * (par[2L] + (x * par[3L]))),
  paramCount = 3L,
  estimator  = .quadratic.estimator,
  gradient   = function(x, par) c(1, x, x*x),
  name       = "Quadratic Function"
)

#' @title Obtain the Simple Quadratic Model \code{y = f(x) = a + b*x + c*x^2}
#' @description A simple quadratic model, i.e., a model of the form \code{y =
#'   f(x) = a + b*x + c*x^2} with three parameters (\code{a}, \code{b}, and
#'   \code{c})).
#' @export FunctionalModel.quadratic
FunctionalModel.quadratic <- function() .quadratic
