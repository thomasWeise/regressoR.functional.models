#' @include linear.R
#' @include quadratic.R
#' @include cubic.R
#' @include exponentialDecay.R
#' @include logistic.R
#' @include gompertz.R
#'
# the internal variable for default modes
.all <- list(FunctionalModel.linear(),
             FunctionalModel.quadratic(),
             FunctionalModel.cubic(),
             FunctionalModel.exponentialDecay.1(),
             FunctionalModel.exponentialDecay.2(),
             FunctionalModel.logistic.1(),
             FunctionalModel.logistic.2(),
             FunctionalModel.gompertz.1(),
             FunctionalModel.gompertz.2())

#' @title Obtain a List with the All Functional Models Provided by this Library
#' @description This function returns a list with the all the models provided by
#'   this library.
#' @return a list with all models provided by this library.
#' @export FunctionalModel.all
#' @seealso FunctionalModel.new
#' @seealso FunctionalModel.linear
#' @seealso FunctionalModel.quadratic
FunctionalModel.all <- function() .all
