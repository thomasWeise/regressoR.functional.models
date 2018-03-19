#' @include FunctionalModel.R

#' @title Check that model parameters are acceptable for the functional model
#' @description Check the the model parameter vector has the right length, all
#'   finite elements, and fits to the boundaries specified in the functional
#'   model.
#' @param functionalModel the functional model
#' @param par the parameter vector to be checked
#' @return \code{TRUE} if the parameter vector is OK, \code{FALSE} otherwise
#' @export par.check
par.check <- function(functionalModel, par) {
  if(base::is.null(functionalModel)) {
    stop("Functional model cannot be null.")
  }
  (!(base::is.null(par))) &&
    (base::length(par) == functionalModel@paramCount) &&
    base::all(base::is.finite(par)) &&
    (base::is.null(functionalModel@paramLower) ||
     base::all(par >= functionalModel@paramLower, na.rm = TRUE)) &&
    (base::is.null(functionalModel@paramUpper) ||
     base::all(par <= functionalModel@paramUpper, na.rm = TRUE));
}