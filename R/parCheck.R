#' @include FunctionalModel.R

#' @title Check that model parameters are acceptable for the functional model
#' @description Check the the model parameter vector has the right length, all
#'   finite elements, and fits to the boundaries specified in the functional
#'   model.
#' @param model the functional model
#' @param par the parameter vector to be checked
#' @return \code{TRUE} if the parameter vector is OK, \code{FALSE} otherwise
#' @export FunctionalModel.par.check
FunctionalModel.par.check <- function(model, par) {
  if(base::is.null(model)) {
    stop("Functional model cannot be null.")
  }
  (!(base::is.null(par))) &&
    (base::length(par) == model@paramCount) &&
    base::all(base::is.finite(par)) &&
    (base::is.null(model@paramLower) ||
     base::all(par >= model@paramLower, na.rm = TRUE)) &&
    (base::is.null(model@paramUpper) ||
     base::all(par <= model@paramUpper, na.rm = TRUE));
}
