#' @include FunctionalModel.R

# The internal constant for constant models
.constant <- FunctionalModel.new(
  f = function(x, par) par[1L],
  paramCount = 1L,
  estimator = function(x, y) { mean(y) },
  gradient = function(x, par) c(1),
  name       = "Constant"
)

#' @title Obtain the Simple Constant Model \code{y = f(x) = a}
#' @description A simple constant model, i.e., a model of the form \code{y =
#'   f(x) = a} with one parameter (\code{a}).
#' @export FunctionalModel.constant
FunctionalModel.constant <- function() .constant
