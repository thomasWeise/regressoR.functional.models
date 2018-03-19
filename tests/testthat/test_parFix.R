library("regressoR.functional.models")
context("par.fix")


test_that("Test par.fix works correctly", {
  for(i in 1:75) {
    n <- as.integer(runif(n=1, min=1, max=8));
    if(runif(1) > 0.5) {
      lower <- NULL;
    } else {
      lower <- runif(n, min=-100, max=100);
    }
    if(runif(1) > 0.5) {
      upper <- NULL;
    } else {
      if(is.null(lower)) {
        upper <- runif(n=n, min=-100, max=100);
      } else {
        upper <- runif(n=n, min=lower+1e-7, max=lower+200);
      }
    }
    if(!(is.null(lower))) {
      while(runif(1) > 0.5) { lower[as.integer(runif(n)) + 1] <- NA; }
    }
    if(!(is.null(upper))) {
      while(runif(1) > 0.5) { upper[as.integer(runif(n)) + 1] <- NA; }
    }

    for(j in 1:100) {
      par <- runif(n=n, min=-200, max=200);
      while(runif(1) > 0.5) {
        z <- runif(n=1);
        if(z < 1/3) { z <- -Inf; }
        else if(z < 2/3) {z <- Inf; }
        else { z <- NaN; }
        par[as.integer(runif(n)) + 1] <- z;
        }
      x <- par.fix(par=par, lower, upper, n);
      for(k in 1:n) {
        expect_true(is.finite(x[k]));
        if(!(is.null(lower) || is.na(lower[k]))) {
          expect_true(x[k] >= lower[k]);
        }
        if(!(is.null(upper) || is.na(upper[k]))) {
          expect_true(x[k] <= upper[k]);
        }
      }
    }
  }
})

