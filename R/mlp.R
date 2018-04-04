#' @include FunctionalModel.R
#' @include internalEstimateSampler.R

#' @title Create a Multi-Layer Perceptron Model with the Given Layers and
#' Activation Function
#' @description With this function, we can create a multi-layer perceptron model
#'   with the specified \code{layers} configuration and activation
#'   \code{func}tion. Each neuron has a bias parameter plus \code{n} weights for
#'   all incoming connections from the previous layer. The output of the last
#'   layer is also weighted and added to a bias parameter.
#' @param layers the vector of layers, e.g., \code{c(1)} for a perceptron with a
#'   single input=output node, \code{1, 2, 1} with an input node, two hidden
#'   nodes, and an output node.
#' @param func the activation function
#' @return a functional model representing the given perceptron
#' @export FunctionalModel.mlp
FunctionalModel.mlp <- function(layers, func="tanh") {

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
    layer <- as.integer(layer);
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

  if(size > 1L) {
    text <- paste0(text, "L:", nextWeight - 1L);
  }
  text <- paste0(text, "L])");
  if(size > 1L) {
    text <- paste0(text, ")");
  }
  text <- paste0(text, " + par[", nextWeight, "L]); };",
                 " if(length(x) <= 1) .inner(x)",
                 " else vapply(X=x, FUN=.inner, FUN.VALUE=NaN) }");

  # done with building MLP
  f <- eval(parse(text=text));
  f <- force(f);

  # The estimator function aims at generating reasonable start values for the parameters.
  # The bias of the input neuron is guessed to be around -1*half of the mean of
  # the x values so we maybe get stuff centered around zero. The bias of the
  # output is guessed to be something like half the mean of the y values,
  # because we assume that the result will be centered aroung zero in first
  # approximation.
  # For the rest, we just guess normally distributed numbers.
  .estimator <- function(x, y) {
    x.range <- max(abs(range(x)));
    y.range <- max(abs(range(y)));

    paramLower <- c(min(-1000L, -5L*x.range), rep(-1000L, nextWeight-2L), min(-1000L, -5L*y.range));
    paramUpper <- c(max( 1000L,  5L*x.range), rep( 1000L, nextWeight-2L), max( 1000L,  5L*y.range));

    .sampler <- function() c(rnorm(n=1, mean=-mean(x), sd=0.3*x.range),
                             rnorm(n=(nextWeight-2L)),
                             rnorm(n=1, mean=mean(y), sd=0.3*y.range));
    .sampler <- force(.sampler);

    return(.estimate.internal(x, y, paramLower, paramUpper, .sampler, f, nextWeight));
  }
  .estimator <- force(.estimator);

  FunctionalModel.new(f          = f,
                      paramCount = nextWeight,
                      estimator  = .estimator,
                      name       = paste0("MLP[", func, "-",
                                   paste(layers, collapse="-"), "]"));
}
