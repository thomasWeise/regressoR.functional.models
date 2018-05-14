#' @include quadratic.R

#' @title Compute the parameter values of a Cubic Function from four Point
#'   Coordinates
#' @description This function returns the four parameters of a cubic model from
#'   four point coordinates.
#' @param x1 the first \code{x}-coordinate
#' @param y1 the first \code{y}-coordinate
#' @param x2 the second \code{x}-coordinate
#' @param y2 the second \code{y}-coordinate
#' @param x3 the third \code{x}-coordinate
#' @param y3 the third \code{y}-coordinate
#' @param x4 the fourth \code{x}-coordinate
#' @param y4 the fourth \code{y}-coordinate
#' @return a vector of type \code{(a,b,c,d)}, such than
#'   \code{f(x)=a+b*x+c*x^2+d*x^3} or \code{NULL} if no finite result is
#'   possible
#' @export FunctionalModel.cubic.from.four.points
FunctionalModel.cubic.from.four.points <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
  x1.2 <- x1 * x1;
  x1.3 <- x1.2 * x1;

  x2.2 <- x2 * x2;
  x2.3 <- x2.2 * x2;

  x3.2 <- x3 * x3;
  x3.3 <- x3.2 * x3;

  x4.2 <- x4 * x4;
  x4.3 <- x4.2 * x4;

  x33y4x43y3 <- (x3.3*y4-x4.3*y3);
  x32x43mx33x42 <- (x3.2*x4.3-x3.3*x4.2);
  x33x4mx3x43 <- (x3.3*x4-x3*x4.3);
  x3x42mx32x4 <- (x3*x4.2-x3.2*x4);
  x4y3mx3y4 <- (x4*y3-x3*y4);

  vl1 <- (x2.3*(y4-y3)-x3.3*y4+x4.3*y3+(x3.3-x4.3)*y2);
  vl2 <- (x3*y4+x2*(y3-y4)-x4*y3+(x4-x3)*y2);
  vl3 <- (x2*(x4.2-x3.2)-x3*x4.2+x3.2*x4+x2.2*(x3-x4));
  vl4 <- (x2.2*(x4.3-x3.3)-x3.2*x4.3+x3.3*x4.2+x2.3*(x3.2-x4.2));
  vl5 <- (x3*x4.3+x2*(x3.3-x4.3)+x2.3*(x4-x3)-x3.3*x4);
  vl6 <- (x1*vl4+x2*x32x43mx33x42+x1.2*vl5+x2.2*x33x4mx3x43+x1.3*vl3+x2.3*x3x42mx32x4);

  p1 <- (-(x1*(x2.2*x33y4x43y3+x2.3*(x4.2*y3-x3.2*y4)+x32x43mx33x42*y2)+x1.2*(x2*(x4.3*y3-x3.3*y4)+x2.3*(x3*y4-x4*y3)+x33x4mx3x43*y2)+x1.3*(x2*(x3.2*y4-x4.2*y3)+x2.2*x4y3mx3y4+x3x42mx32x4*y2)+(x2*(x3.3*x4.2-x3.2*x4.3)+x2.2*(x3*x4.3-x3.3*x4)+x2.3*(x3.2*x4-x3*x4.2))*y1)/vl6);
  p2 <- (x1.2*vl1+x2.2*x33y4x43y3+x1.3*(x3.2*y4+x2.2*(y3-y4)-x4.2*y3+(x4.2-x3.2)*y2)+x2.3*(x4.2*y3-x3.2*y4)+x32x43mx33x42*y2+vl4*y1)/vl6;
  p3 <- (-(x1*vl1+x2*x33y4x43y3+x1.3*vl2+x2.3*x4y3mx3y4+(x3*x4.3-x3.3*x4)*y2+(x2*(x4.3-x3.3)-x3*x4.3+x3.3*x4+x2.3*(x3-x4))*y1)/vl6);
  p4 <- (x1*(x2.2*(y4-y3)-x3.2*y4+x4.2*y3+(x3.2-x4.2)*y2)+x2*(x3.2*y4-x4.2*y3)+x1.2*vl2+x2.2*x4y3mx3y4+x3x42mx32x4*y2+vl3*y1)/vl6;

  if(is.finite(p1) && is.finite(p2) && is.finite(p3) && is.finite(p4)) {
    return(c(p1, p2, p3, p4));
  }
  return(NULL);
}


# The cubic estimator function
.cubic.estimator <- function(x, y) {
  len <- length(x);
  res <- NULL;

  if(len > 4L) {
    sres<-sapply(X=1:min(100L, len-4L),
                 FUN=function(x) {
                   sample <- sample.int(n=len, size=4L);
                   FunctionalModel.cubic.from.four.points(x[sample[1L]], y[sample[1L]],
                                           x[sample[2L]], y[sample[2L]],
                                           x[sample[3L]], y[sample[3L]],
                                           x[sample[4L]], y[sample[4L]]);
                 });
    if( (!(is.null(sres))) && (length(sres)>0L) ) {
      if(is.null(dim(sres))) {
        sres <- sapply(sres[-which(sapply(sres, is.null))], rbind);
      }
      if( (!(is.null(sres))) && (length(sres)>0L) ) {
        res <- rowMeans(sres);
        if(is.finite(res[1L]) && is.finite(res[2L]) && is.finite(res[3L]) && is.finite(res[4L])) {
          return(res);
        }
      }
    }
  }

  if(len >= 4L) {
    res <- FunctionalModel.cubic.from.four.points(x[1L], y[1L],
                                   x[len/3L+1L], y[len/3L+1L],
                                   x[(2L*len)/3L+1L], y[(2L*len)/3L+1L],
                                   x[len], y[len]);
    if(!is.null(res)) {
      return(res);
    }
    res <- FunctionalModel.cubic.from.four.points(x[1L], y[1L], x[2L], y[2L], x[3L], y[3L], x[4L], y[4L]);
    if(!is.null(res)) {
      return(res);
    }
  }

  res <- .quadratic.estimator(x, y);
  if(is.null(res)) { return(NULL); }
  return(c(res, 0L));
}



# The internal constant for cubic models
.cubic <- FunctionalModel.new(
  f = function(x, par) par[1] + (x * (par[2] + (x * (par[3] + (x * par[4]))))),
  paramCount = 4L,
  estimator = .cubic.estimator,
  gradient=function(x, par) c(1, x, x*x, x*x*x),
  name="Cubic Function"
)

#' @title Obtain the Simple Cubic Model \code{y = f(x) = a + b*x + c*x^2 + d*x^3}
#' @description A simple cubic model, i.e., a model of the form \code{y =
#'   f(x) = a + b*x + c*x^2 + d*x^3} with four parameters (\code{a}, \code{b}, \code{c}, and
#'   \code{4})).
#' @export FunctionalModel.cubic
FunctionalModel.cubic <- function() .cubic
