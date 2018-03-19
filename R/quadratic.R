#' @include FunctionalModel.R
#' @include linear.R
#' @include utils.R

# Compute the values of a quadratic function from three points
.quadratic.from.three.points <- function(x1, y1, x2, y2, x3, y3) {
  x1s<-x1*x1;
  x2s<-x2*x2;
  x3s<-x3*x3;
  div<-(x1*((x3s)-(x2s))-x2*(x3s)+(x2s)*x3+(x1s)*(x2-x3));
  res <- base::c( (x1*((x3s)*y2-(x2s)*y3)+(x1s)*(x2*y3-x3*y2)+((x2s)*x3-x2*(x3s))*y1)/div,
           (-( ((x1s)*(y3-y2)-(x2s)*y3+(x3s)*y2+((x2s)-(x3s))*y1)/div)),
            (x1*(y3-y2)-x2*y3+x3*y2+(x2-x3)*y1)/div);
  if(base::is.finite(res[1]) && base::is.finite(res[2] && base::is.finite(res[3]))) {
    return(res);
  }
  return(NULL);
}

# The internal constant for linear models
#' @importFrom stats nls
.quadratic <- FunctionalModel.new(
  f = function(x, par) par[1] + (x * (par[2] + (x * par[3]))),
  paramCount = 3L,
  estimator = function(x, y, paramLower, paramUpper) {
    len <- base::length(x);
    res <- NULL;

    if(len > 3) {
      sres<-base::sapply(X=1:min(100, len-3),
             FUN=function(x) {
               sample <- base::sample.int(n=len, size=3);
               .quadratic.from.three.points(x[sample[1]], y[sample[1]],
                                            x[sample[2]], y[sample[2]],
                                            x[sample[3]], y[sample[3]]);
             });
      if( (!(base::is.null(sres))) && (base::length(sres)>0) ) {
        if(base::is.null(base::dim(sres))) {
          sres <- base::sapply(sres[-base::which(base::sapply(sres, is.null))], rbind);
        }
        if( (!(base::is.null(sres))) && (base::length(sres)>0) ) {
          res <- base::rowMeans(sres);
          if(!(base::is.finite(res[1]) && base::is.finite(res[2]) && base::is.finite(res[3]))) {
            res <- NULL;
          }
        }
      }
    }

    if((len >= 3) && (base::is.null(res))) {
      res <- .quadratic.from.three.points(x[1], y[1], x[len/2L+1], y[len/2L+1], x[len], y[len]);
      if(base::is.null(res)) {
        res <- .quadratic.from.three.points(x[1], y[1], x[2], y[2], x[3], y[3]);
      }
    }

    if(len > 3) {
      .ignore.errors({
        if(base::is.null(res)) {
          start <- base::list(a=0, b=0, c=0)
        } else {
          start <- base::list(a=res[3], b=res[2], c=res[1]);
        }
        refined <- base::nls(y ~ a*x*x + b*x + c, data=base::list(x=x, y=y), start=start);
        if(!(base::is.null(refined))) {
          if(refined$convInfo$isConv) {
            par <- refined$m$getPars();
            nres <- base::c(par[["c"]], par[["b"]], par[["a"]]);
            if(base::is.finite(nres[1]) && base::is.finite(nres[2]) && base::is.finite(nres[3])) {
              return(nres);
            }
          }
        }
      });
    }

    if(!(base::is.null(res))) {
      return(res);
    }

    res <- linear()@estimator(x, y);
    if(base::is.null(res)) { return(NULL); }
    return(base::c(res, 0));
  },
  gradient=function(x, par) base::c(1, x, x*x)
)

#' @title Obtain the Simple Quadratic Model \code{y = f(x) = a + b*x + c*x^2}
#' @description A simple quadratic model, i.e., a model of the form \code{y = f(x)
#'   = a + b*x + c*x^2} with three parameters (\code{a}, \code{b}, and \code{c})).
#' @export quadratic
quadratic <- function() .quadratic