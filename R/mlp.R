#' @include FunctionalModel.R

# the default estimator function
.mlp.estimator <- function(x, y, n) {
  c(-mean(x), rnorm(n=(n-2L)), mean(y))
}

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
  text   <- "function(x, par) { .inner <- function(x) {"
  weight <- 1L;
  size   <- 1L;
  for(layer in layers) {
    layer <- as.integer(layer);
    text <- paste0(text, " x <- ");
    if(layer > 1L) { text <- paste0(text, "c("); }
    for(i in 1L:layer) {

      if(i > 1L) { text <- paste0(text, ", "); }
      text <- paste0(text, func, "(par[", weight, "L] + (" );

      if(size > 1L) {
        text <- paste0(text, "sum(" );
      }

      nextWeight <- weight + size;
      weight     <- weight + 1L;

      text <- paste0(text, "par[", weight);
      if(nextWeight > weight) {
        text <- paste0(text, "L:", nextWeight);
      }
      text <- paste0(text, "L] * x))");
      if(size > 1L) {
        text <- paste0(text, ")" );
      }

      weight <- nextWeight + 1L;
    }
    if(layer > 1L) { text <- paste0(text, ")"); }
    text <- paste0(text, ";");
    size <- layer;
  }

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

  FunctionalModel.new(f = eval(parse(text=text)),
                      paramCount = nextWeight,
                      estimator = function(x, y) .mlp.estimator(x, y, nextWeight));
}
