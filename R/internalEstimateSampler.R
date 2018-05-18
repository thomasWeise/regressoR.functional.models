# solve a single sample
#' @importFrom minqa bobyqa
#' @importFrom utilizeR ignoreErrors
.solve.sample <- function(x.all, y.all, sample, paramLower, paramUpper, f, sampler) {
  # generate the sample vector
  x  <- x.all[sample];
  x  <- force(x);
  y  <- y.all[sample];
  y  <- force(y);
  # build the objective function
  fn <- function(par) sum(abs(f(x, par) - y));
  fn <- force(fn);
  ret <- NULL;
  ignoreErrors({
    # get the initial point
    par <- sampler();
    # solve the small problem
    result <- bobyqa(par=par, fn=fn, lower=paramLower, upper=paramUpper);
    if(!(is.null(result))) {
      # extract result quality
      quality <- result$fval;
      if(is.finite(quality) && (quality >= 0)) {
        # ok, quality is ok
        result <- result$par;
        # so let us check if vector is valid
        if(all((result >= paramLower) & (result <= paramUpper) & is.finite(result)) &&
           all(is.finite(f(x.all, result)))) {
          # great, we got a solution
          ret <- result;
        }
      }
    }

    if(is.null(ret)) {
      # ok, the point was not a good starting point .. let's check it anyway
      quality <- fn(par);
      if(is.finite(quality) && (quality >= 0)) {
        # so let us check if vector is valie
        if(all((par >= paramLower) & (par <= paramUpper) & is.finite(par)) &&
           all(is.finite(f(x.all, par)))) {
          # great, we got a solution
          ret <- par;
        }
      }
    }
  })
  return(ret);
}

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
.solve.np <- function(x.all, y.all, paramLower, paramUpper, sampler, f, len, n) {
  best.vec      <- NULL;
  best.onBounds <- len;

  # we do 23 attempts to find samples which are not on the bounds
  for(i in seq_len(23L)) {
    # pick the sample
    sample <- sample.int(n=len, size=n);
    # get the parameters
    res    <- .solve.sample(x.all, y.all, sample, paramLower, paramUpper, f, sampler);

    if(!(is.null(res))) {
      # ok, sample could be solved -> check if it is on the bounds
      onBounds <- sum((res-1e-13) <= paramLower) + sum((res+1e-13) >= paramUpper);
      if(onBounds < 1L) {
        # not on the bounds, we can stop and return it
        return(res);
      }
      if(onBounds < best.onBounds) {
        # it was on the bounds in fewer dimensions than the previous sample?
        # cool, use it
        best.vec      <- res;
        best.onBounds <- onBounds;
      }
    }
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
    # we have more points than parameters
    sres <- lapply(X=seq_len(min(11L, len-n)),
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
    # we have at least as many parameters as points, but did not get a good sample yet

    # compute parameters from equi-distant points (according to their position)
    sample <- 1L + 0L:(n-1L) * (len - 1L) / (n - 1L);
    res <- .solve.sample(x, y, sample, paramLower, paramUpper, f, sampler)

    if(!is.null(res)) {
      return(res);
    }
    sample <- seq_len(n); # compute parameters from the first n points
    res <- .solve.sample(x, y, sample, paramLower, paramUpper, f, sampler);

    if(!is.null(res)) {
      return(res);
    }
  }

  # ok, we simply do not have enough points ... let's try to get a result anyway
  for(i in seq_len(50L)) {
    par <- sampler();
# so let us check if vector is valie
    if(all((par >= paramLower) & (par <= paramUpper) & is.finite(par)) &&
       all(is.finite(f(x, par)))) {
      # great, we got a solution
      return(par);
    }
  }

  return(NULL);
}
