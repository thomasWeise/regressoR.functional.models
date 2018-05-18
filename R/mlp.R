#' @include FunctionalModel.R
#' @include internalEstimateSampler.R

# compute the expected number of MLP parameters
.mlp.size <- function(layers) {
  size <- 0L;
  prev <- 1L;
  for(i in as.integer(layers)) {
    size <- size + (i * (prev + 1L));
    prev <- i;
  }
  return(size + prev + 1L);
}

# With this function, we can create a multi-layer perceptron model with the
# specified \code{layers} configuration and activation \code{func}tion. Each
# neuron has a bias parameter plus \code{n} weights for all incoming connections
# from the previous layer. The output of the last layer is also weighted and
# added to a bias parameter.
# @param layers the vector of layers, e.g., \code{c(1)} for a perceptron with a
#   single input=output node, \code{1, 2, 1} with an input node, two hidden
#   nodes, and an output node.
# @param func the activation function
# @return a list of type list(f, size, signs) which provides a pre-compiled
#   function \code{f} for the MLP, the number of weights \code{size}, and the
#   \code{signs} required for enforcing a strictly monotonously decreasing
#   perceptron function
#' @importFrom compiler cmpfun
.mlp.func <- function(layers, func, func.mode) {
  # compute the expected size
  layers       <- as.integer(layers);
  expectedSize <- .mlp.size(layers);
  signs        <- rep.int(0L, expectedSize);
  # if the function is decreasing, we need to the mode all the time
  toggle       <- (func.mode < 0L);

  # We first construct the MLP code as text which we will later parse. The mlp
  # function is vectorized. This is achieved by first making an internal,
  # non-vectorized function .inner and then calling it via vapply if necessary.
  text   <- "function(x, par) { .inner <- function(x) {"
  weight <- 1L;
  size   <- 1L;
  for(layer in layers) {
    # Each layer consists of neurons of the form func( vv + w1*z1 + w2*z2 + ...)
    # where vv is an absolute bias, the wi are weights, and the zi are the input
    # values which either come from the previous layer or are the function
    # input. Each layer constructs either a variable or vector (if it has more
    # than one neuron) of its outputs. Each layer stores these outputs again in
    # the variable x.
    text <- paste0(text, " x <- ");

    # If the layer has more than one neuron, construct a vector.
    if(layer > 1L) { text <- paste0(text, "c("); }

    # For each neuron in the layer...
    for(i in 1L:layer) {

      # if it is not the first neuron, we need to put a comma
      if(i > 1L) { text <- paste0(text, ", "); }

      # Then we invoke the activation function "func" with the bias plus...
      text <- paste0(text, func, "(par[", weight, "L] + (" );

      # if the previous layer has more than one neuron, then the sum of
      if(size > 1L) {
        text <- paste0(text, "sum(" );
      }

      nextWeight <- weight + size;
      weight     <- weight + 1L;

      # the weights times the output from the previous layer
      text <- paste0(text, "par[", weight);
      if(nextWeight > weight) {
        text <- paste0(text, "L:", nextWeight);
      }
      # if the function is increasing, the weights should be positive
      # if it is decreasing, they should be negative
      signs[weight:nextWeight] <- func.mode;
      if(toggle) { func.mode <- (-func.mode); }

      # the previous layer's results are always in x
      text <- paste0(text, "L] * x))");
      if(size > 1L) {
        text <- paste0(text, ")" );
      }

      weight <- nextWeight + 1L;
    }

    # we can finish this layer
    if(layer > 1L) { text <- paste0(text, ")"); }
    text <- paste0(text, ";");
    size <- layer;
  }

  # finally, we return an absolute value plus the weighted sum of all output neurons
  text <- paste0(text, " return( (");
  if(size > 1L) {
    text <- paste0(text, "sum(");
  }

  text <- paste0(text,"x * par[", weight);
  nextWeight <- weight + size;
  # the final weights are negative for a decreasing function:
  # if the function is increasing, the weights should be negative
  # if it is decreasing, they should be positive
  signs[weight:(nextWeight - 1L)] <- -(func.mode);

  stopifnot(identical(nextWeight, expectedSize));

  if(size > 1L) {
    text <- paste0(text, "L:", nextWeight - 1L);
  }
  text <- paste0(text, "L])");
  if(size > 1L) {
    text <- paste0(text, ")");
  }
  text <- paste0(text, " + par[", nextWeight, "L]); };",
                 # vectorize
                 " if(length(x) <= 1L) .inner(x)",
                 " else vapply(X=x, FUN=.inner, FUN.VALUE=NaN) }");

  # done with building MLP
  f <- eval(parse(text=text));
  f <- force(f);
  f <- cmpfun(f, options=list(optimize=3L));
  f <- force(f);

  return(list(f=f, size=expectedSize, signs=signs));
}


# The estimator function aims at generating reasonable start values for the parameters.
# The bias of the input neuron is guessed to be around -1*half of the mean of
# the x values so we maybe get stuff centered around zero. The bias of the
# output is guessed to be something like half the mean of the y values,
# because we assume that the result will be centered aroung zero in first
# approximation.
# For the rest, we just guess normally distributed numbers.
.mlp.estimator.general <- function(size, f) {
  size <- force(size);

  .estimator <- function(x, y) {
    # ensure that variables exist
    size <- force(size);
    x    <- force(x);
    y    <- force(y);
    size <- force(size);

    x.range  <- max(abs(range(x))); x.range  <- force(x.range);
    y.range  <- max(abs(range(y))); y.range  <- force(y.range);
    x.mean.n <- -mean(x);           x.mean.n <- force(x.mean.n);
    y.mean   <-  mean(y);           y.mean   <- force(y.mean);

    paramLower <- c(min(-1000L, -5L*x.range), rep(-1000L, size-2L), min(-1000L, -5L*y.range));
    paramUpper <- c(max( 1000L,  5L*x.range), rep( 1000L, size-2L), max( 1000L,  5L*y.range));

    .sampler <- function() c(rnorm(n=1, mean=x.mean.n, sd=0.3*x.range),
                             rnorm(n=(size-2L)),
                             rnorm(n=1, mean=y.mean, sd=0.3*y.range));
    .sampler <- force(.sampler);

    return(.estimate.internal(x, y, paramLower, paramUpper, .sampler, f, size));
  }

  .estimator <- force(.estimator);
  return(.estimator);
}


# The estimator function aims at generating reasonable start values for the parameters.
# The bias of the input neuron is guessed to be around -1*half of the mean of
# the x values so we maybe get stuff centered around zero. The bias of the
# output is guessed to be something like half the mean of the y values,
# because we assume that the result will be centered aroung zero in first
# approximation.
# For the rest, we just guess normally distributed numbers.
.mlp.estimator.monotonous <- function(size, sign, f) {
  size <- force(size);
  sign <- force(sign);

  .estimator <- function(x, y) {
# enforce the parameters
    size     <- force(size);
    sign     <- force(sign);
# compute the data range and means
    x.range  <- max(abs(range(x))); x.range  <- force(x.range);
    y.range  <- max(abs(range(y))); y.range  <- force(y.range);
    x.mean.n <- -mean(x);           x.mean.n <- force(x.mean.n);
    y.mean   <-  mean(y);           y.mean   <- force(y.mean);
# setup the basic boundaries to sample from within
    paramLower <- c(min(-1000L, -5L*x.range), rep(-1000L, size-2L), min(-1000L, -5L*y.range));
    paramUpper <- c(max( 1000L,  5L*x.range), rep( 1000L, size-2L), max( 1000L,  5L*y.range));
# fix the boundaries according the definition
    paramLower[sign > 0L] <- 0;
    paramUpper[sign < 0L] <- 0;
# extract the positive and negative part
    sign <- sign[2L:(size-1L)];
    positive <- (sign > 0L); positive <- force(positive);
    negative <- (sign < 0L); negative <- force(negative);
# specify the sampler function
    .sampler <- function() {
# create the middle part
      mid <- rnorm(n=(size-2L));
# and fix signs where necessary
      mid[positive] <- abs(mid[positive]);
      mid[negative] <- -abs(mid[negative]);
# attach the first and last number
      return(c(rnorm(n=1, mean=x.mean.n, sd=0.3*x.range), mid,
               (y.mean + abs(rnorm(n=1, mean=0, sd=0.3*y.range)))));
    }
    .sampler <- force(.sampler);
#cat("size", size, "sign", sign, "\nlo:", paramLower, "\nhi:", paramUpper, "\nex:", .sampler(), "\n");
    return(.estimate.internal(x, y, paramLower, paramUpper, .sampler, f, size));
  }

  .estimator <- force(.estimator);
  return(.estimator);
}


#' @title Create a Multi-Layer Perceptron Model with the Given Layers and
#'   Activation Function
#' @description With this function, we can create a multi-layer perceptron model
#'   with the specified \code{layers} configuration and activation
#'   \code{func}tion. Each neuron has a bias parameter plus \code{n} weights for
#'   all incoming connections from the previous layer. The output of the last
#'   layer is also weighted and added to a bias parameter.
#' @param layers the vector of layers, e.g., \code{c(1)} for a perceptron with a
#'   single input=output node, \code{1, 2, 1} with an input node, two hidden
#'   nodes, and an output node.
#' @param func the activation function
#' @param decreasing if \code{TRUE}, then an MLP is generated whose parameter
#'   range is limited such that only a monotonously decreasing function can be
#'   represented. If \code{FALSE}, a general MLP which can represent arbitrary
#'   function shapes (within its degrees of freedom) is produced.
#' @param func.mode Only relevant if \code{decreasing==TRUE}: If we want to
#'   generate a monotonously decreasing perceptron, then we need to know whether
#'   the activation function \code{func} is itself monotnously decreasing
#'   (\code{mode==-1L}) or increasing (\code{model==1L}). The default
#'   \code{tanh} function is increasing.
#' @return a functional model representing the given perceptron
#' @export FunctionalModel.mlp
FunctionalModel.mlp <- function(layers,
                                decreasing=FALSE,
                                func="tanh",
                                func.mode=1L) {

  # sanity check
  stopifnot( (length(layers) > 0L) &&
             is.character(func) && (length(func) == 1L) && (nchar(func) > 1L) &&
             is.logical(decreasing) &&
             ((!(isTRUE(decreasing))) || ((func.mode==1L) || (func.mode==-1L))));

  # generate the MLP
  layers <- as.integer(layers);
  mlp    <- .mlp.func(layers=layers, func=func, func.mode=func.mode);
  size   <- mlp$size; size <- force(size);
  f      <- mlp$f;    f    <- force(f);

  # generate ranges
  if(isTRUE(decreasing)) {
    # compute the parameter thresholds
    lower <- rep(NA, size);
    upper <- lower;
    signs <- mlp$signs;
    lower[signs > 0L] <- 0;
    upper[signs < 0L] <- 0;
    if(all(is.na(lower))) { lower <- NULL; }
    if(all(is.na(upper))) { upper <- NULL; }
    decreasing <- (!(is.null(lower) && is.null(upper)));
  } else {
    decreasing <- FALSE;
    lower      <- NULL;
    upper      <- NULL;
    signs      <- NULL;
  }
  lower <- force(lower);
  upper <- force(upper);

  # build the estimator function
  if(decreasing) {
    estimator <- .mlp.estimator.monotonous(size, signs, f);
  } else {
    estimator <- .mlp.estimator.general(size, f);
  }
  estimator <- force(estimator);

  # generate the name
  if(decreasing) {
    name <- "decMLP[";
  } else {
    name <- "MLP[";
  }
  name <- paste0(name, func, "-", paste(layers, collapse="-"), "]");

  return(FunctionalModel.new(f          = mlp$f,
                             paramCount = size,
                             estimator  = estimator,
                             paramLower = lower,
                             paramUpper = upper,
                             name       = name));
}
