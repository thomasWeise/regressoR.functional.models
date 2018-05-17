#' @include constant.R
#' @include linear.R
#' @include quadratic.R
#' @include cubic.R
#' @include exponentialDecay.R
#' @include logistic.R
#' @include gompertz.R
#' @include expLogLinear.R
#'
# the internal variable for default modes
.all <- unlist(list(FunctionalModel.constant(),
                    FunctionalModel.linear(),
                    FunctionalModel.quadratic(),
                    FunctionalModel.cubic(),
                    FunctionalModel.exponentialDecay.1(),
                    FunctionalModel.exponentialDecay.2(),
                    FunctionalModel.logistic.1(),
                    FunctionalModel.logistic.2(),
                    FunctionalModel.gompertz.1(),
                    FunctionalModel.gompertz.2(),
                    FunctionalModel.expLogLinear.1(),
                    FunctionalModel.expLogLinear.2()))

#' @title Obtain a List with the All Functional Models Provided by this Library
#' @description This function returns a list with the all the models provided by
#'   this library. Currently, these comprise standard constant, linear,
#'   quadratic, and cubic models as well as several sigmoid-style models such as
#'   exponential decay, logistic models, the Gompertz model, and an exponential
#'   log-linear model.
#' @return a list with all models provided by this library.
#' @export FunctionalModel.all
#' @seealso FunctionalModel.new
#' @seealso FunctionalModel.linear
#' @seealso FunctionalModel.quadratic
FunctionalModel.all <- function() .all
