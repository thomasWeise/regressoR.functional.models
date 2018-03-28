#' @include utils.R


# try to find estimates for n parameters by solving a set of n equations
# @param x the x coordinates of the n equations
# @param x the y coordinates of the n equations
# @param paramLower the lower limits
# @param paramUpper the upper limits
# @param par the starting oint
# @param f the function whose parameters to find
# @return a parameter vector or NULL
#' @importFrom minqa bobyqa
.solve.np <- function(x, y, paramLower, paramUpper, par, f) {
  .ignore.errors({
    result <- minqa::bobyqa(par=par,
                            fn=function(par) sum(abs(f(x, par) - y)),
                            lower=paramLower, upper=paramUpper);
    f <- result$fval;
    if(is.finite(f) && (f >= 0)) {
      res <- result$par;
      if(all((res >= paramLower) && (res <= paramUpper) && is.finite(res))) {
        return(result$par);
      }
    }
  });
  return(NULL);
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
    sres <- sapply(X=1:min(max(3*n, 41L), len-n),
                   FUN=function(i) {
                     sample <- sample.int(n=len, size=n);
                     return(.solve.np(x[sample], y[sample], paramLower, paramUpper, sampler(), f));
                   });
    if( (!(is.null(sres))) && (length(sres)>0L) ) {
      if(is.null(dim(sres))) {
        sres <- sapply(sres[-which(sapply(sres, is.null))], rbind);
      }
      if( (!(is.null(sres))) && (length(sres) > 0L) ) {
        count <- dim(sres)[2];
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
    res <- .solve.np(x[sample], y[sample], paramLower, paramUpper, sampler(), f)
    if(!is.null(res)) {
      return(res[1:n]);
    }
    sample <- 1:n;
    res <- .solve.np(x[sample], y[sample], paramLower, paramUpper, sampler(), f)
    if(!is.null(res)) {
      return(res[1:n]);
    }
  }

  return(NULL);
}
