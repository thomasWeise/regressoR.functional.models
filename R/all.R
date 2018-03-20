#' @include linear.R
#' @include quadratic.R

# the internal variable for default modes
.all <- base::list(FunctionalModel.linear(),
                   FunctionalModel.quadratic())

#' @title Obtain a List with the All Functional Models Provided by this Library
#' @description This function returns a list with the all the models provided by
#'   this library.
#' @return a list with all models provided by this library.
#' @export FunctionalModel.all
#' @seealso FunctionalModel.new
#' @seealso FunctionalModel.linear
#' @seealso FunctionalModel.quadratic
FunctionalModel.all <- function() .all
