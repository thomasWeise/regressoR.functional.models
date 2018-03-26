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
      if(all((res >= paramLower) && (res <= paramUpper))) {
        return(c(result$par, 1L/(1L+f)));
      }
    }
  });
  return(NULL);
}


#' @importFrom stats weighted.mean
.wm <- stats::weighted.mean
.wm <-force(.wm)

# a blueprint estimator for 4 parameters
# @param x the x coordinates
# @param x the y coordinates
# @param paramLower the lower limits
# @param paramUpper the upper limits
# @param sampler a function which can sample a vector within these limits
# @param f the function whose parameters to find
# @return a parameter vector or NULL
.estimate.4p.internal <- function(x, y, paramLower, paramUpper, sampler, f) {
  len <- length(x);
  res <- NULL;

  if(len > 4L) {
    sres<-sapply(X=1:min(30L, len-4L),
                 FUN=function(x) {
                   sample <- sample.int(n=len, size=4L);
                   .solve.np(x[sample], y[sample], paramLower, paramUpper, sampler(), f);
                 });
    if( (!(is.null(sres))) && (length(sres)>0L) ) {
      if(is.null(dim(sres))) {
        sres <- sapply(sres[-which(sapply(sres, is.null))], rbind);
      }
      if( (!(is.null(sres))) && (length(sres)>0L) ) {
        res <- c(.wm(sres[1,], sres[5,]),
                 .wm(sres[2,], sres[5,]),
                 .wm(sres[3,], sres[5,]),
                 .wm(sres[4,], sres[5,]));
        if(is.finite(res[1L]) && is.finite(res[2L]) && is.finite(res[3L]) && is.finite(res[4L])) {
          return(res);
        }

        res <- c(mean(sres[1,], trim=0.1),
                 mean(sres[2,], trim=0.1),
                 mean(sres[3,], trim=0.1),
                 mean(sres[4,], trim=0.1));
        if(is.finite(res[1L]) && is.finite(res[2L]) && is.finite(res[3L]) && is.finite(res[4L])) {
          return(res);
        }
        res <- rowMeans(sres[1:4,]);
        if(is.finite(res[1L]) && is.finite(res[2L]) && is.finite(res[3L]) && is.finite(res[4L])) {
          return(res);
        }
      }
    }
  }

  if(len >= 4L) {
    sample <- c(1L, len/3L+1L, (2L*len)/3L+1L, len);
    res <- .solve.np(x[sample], y[sample], paramLower, paramUpper, sampler(), f)
    if(!is.null(res)) {
      return(res);
    }
    sample <- c(1L, 2L, 3L, 4L);
    res <- .solve.np(x[sample], y[sample], paramLower, paramUpper, sampler(), f)
    if(!is.null(res)) {
      return(res);
    }
  }

  return(NULL);
}
