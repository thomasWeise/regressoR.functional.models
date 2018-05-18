#' @include constant.R
#' @include linear.R
#' @include quadratic.R
#' @include cubic.R
#' @include exponentialDecay.R
#' @include logistic.R
#' @include gompertz.R
#' @include expLogLinear.R
#' @include mlpDefault.R
#'
# the internal variable for default models
.default <- unlist(list(FunctionalModel.constant(),
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
                        FunctionalModel.expLogLinear.2(),
                        FunctionalModel.mlp.default()))


# the internal variable for the monotonously decreasing models
.monotonous <- unlist(list(FunctionalModel.constant(),
                           FunctionalModel.linear(),
                           FunctionalModel.exponentialDecay.1(),
                           FunctionalModel.exponentialDecay.2(),
                           FunctionalModel.logistic.1(),
                           FunctionalModel.logistic.2(),
                           FunctionalModel.gompertz.1(),
                           FunctionalModel.gompertz.2(),
                           FunctionalModel.expLogLinear.1(),
                           FunctionalModel.expLogLinear.2(),
                           FunctionalModel.mlp.monotonous()))


#' @title Obtain a List with the Default Functional Models Provided by this
#'   Library
#' @description This function returns a list with the default models provided by
#'   this library. Currently, these comprise standard constant, linear,
#'   quadratic, and cubic models as well as several sigmoid-style models such as
#'   exponential decay, logistic models, the Gompertz model, and an exponential
#'   log-linear model. This also includes some multi-layer perceptrons (MLPs).
#' @return a list with the default models provided by this library.
#' @export FunctionalModel.default
#' @seealso FunctionalModel.new
#' @seealso FunctionalModel.constant
#' @seealso FunctionalModel.linear
#' @seealso FunctionalModel.quadratic
#' @seealso FunctionalModel.cubic
#' @seealso FunctionalModel.exponentialDecay.1
#' @seealso FunctionalModel.exponentialDecay.2
#' @seealso FunctionalModel.logistic.1
#' @seealso FunctionalModel.logistic.2
#' @seealso FunctionalModel.gompertz.1
#' @seealso FunctionalModel.gompertz.2
#' @seealso FunctionalModel.expLogLinear.1
#' @seealso FunctionalModel.expLogLinear.2
#' @seealso FunctionalModel.mlp.default
#' @seealso FunctionalModel.monotonous
FunctionalModel.default <- function() .default


#' @title Obtain a List with the Default Monotonously Decreasing Functional
#'   Models Provided by this Library
#' @description This function returns a list with the default monotonously
#'   decreasing models provided by this library. Currently, these comprise
#'   standard constant and linear model as well as several sigmoid-style models
#'   such as exponential decay, logistic models, the Gompertz model, and an
#'   exponential log-linear model.
#' @return a list with the default models provided by this library.
#' @export FunctionalModel.monotonous
#' @seealso FunctionalModel.new
#' @seealso FunctionalModel.constant
#' @seealso FunctionalModel.linear
#' @seealso FunctionalModel.exponentialDecay.1
#' @seealso FunctionalModel.exponentialDecay.2
#' @seealso FunctionalModel.logistic.1
#' @seealso FunctionalModel.logistic.2
#' @seealso FunctionalModel.gompertz.1
#' @seealso FunctionalModel.gompertz.2
#' @seealso FunctionalModel.expLogLinear.1
#' @seealso FunctionalModel.expLogLinear.2
FunctionalModel.monotonous <- function() .monotonous
