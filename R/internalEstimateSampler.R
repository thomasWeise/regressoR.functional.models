#' @include utils.R


# try to find estimates for n parameters by solving a set of n equations
# @param x.all the x coordinates of the n equations
# @param y.all the y coordinates of the n equations
# @param paramLower the lower limits
# @param paramUpper the upper limits
# @param f the function whose parameters to find
# @param sampler the sampler
# @param x.all all code
# @param len the length of x.all
# @param n the number of points to pick
# @return a parameter vector or NULL
#' @importFrom minqa bobyqa
.solve.np <- function(x.all, y.all, paramLower, paramUpper, sampler, f, len, n) {

  best.vec      <- NULL;
  best.onBounds <- len;

  for(i in 1L:23L) {
    sample <- sample.int(n=len, size=n);
    x  <- x.all[sample];
    y  <- y.all[sample];
    fn <- function(par) sum(abs(f(x, par) - y));

    .ignore.errors({
      result <- minqa::bobyqa(par=sampler(), fn=fn, lower=paramLower, upper=paramUpper);
      quality <- result$fval;
      if(is.finite(quality) && (quality >= 0)) {
        res <- result$par;
        if(all((res >= paramLower) && (res <= paramUpper) && is.finite(res)) &&
           all(is.finite(f(x.all, res)))) {
          onBounds <- sum((res-1e-13) <= paramLower) + sum((res+1e-13) >= paramUpper);
          if(onBounds < 1L) {
            return(res);
          }
          if(onBounds < best.onBounds) {
            best.vec      <- res;
            best.onBounds <- onBounds;
          }
        }
      }
    });
  }
  return(best.vec);
}


# a blueprint estimator for 4 parameters
# @param x the x coordinates
# @param x the y coordinates
# @param paramLower the lower limits
# @param paramUpper the upper limitns
# @param sampler a function which can sample a vector within these limits
# @param f the function whose parameters to find
# @param n the number of parameters
# @return a parameter vector or NULL
.estimate.internal <- function(x, y, paramLower, paramUpper, sampler, f, n) {
  len <- length(x);
  res <- NULL;

  if(len > n) {
    sres <- sapply(X=1L:min(11L, len-n),
                   FUN=function(i) {
                     return(.solve.np(x, y, paramLower, paramUpper, sampler, f, len, n));
                   });
    if( (!(is.null(sres))) && (length(sres)>0L) ) {
      if(is.null(dim(sres))) {
        sres <- sapply(sres[-which(sapply(sres, is.null))], rbind);
      }
      if( (!(is.null(sres))) && (length(sres) > 0L) ) {
        count <- dim(sres)[2L];
        ranksum <- rep(0, count);
        for(i in 1:n) {
          ranksum <- ranksum + rank(sres[i,]);
        }
        res <- sres[, order(ranksum)[count / 2L]];
        return(res);
      }
    }
  }

  if(len >= n) {
    sample <- 1L + 0L:(n-1L) * (len - 1L) / (n - 1L);
    res <- .solve.np(x[sample], y[sample], paramLower, paramUpper, sampler, f)
    if(!is.null(res)) {
      return(res);
    }
    sample <- 1:n;
    res <- .solve.np(x[sample], y[sample], paramLower, paramUpper, sampler, f)
    if(!is.null(res)) {
      return(res);
    }
  }

  return(NULL);
}
