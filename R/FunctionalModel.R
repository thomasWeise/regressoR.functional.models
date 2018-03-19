# Either a \code{function} or \code{NULL}
#' @importFrom methods setClassUnion
setClassUnion(".regressoR.functional.models.functionOrNULL", c("function","NULL"))
# Either a \code{numeric} \code{vector} or \code{NULL}
#' @importFrom methods setClassUnion
setClassUnion(".regressoR.functional.models.vectorOrNULL", c("numeric","NULL"))

#' @title A FunctionalModel for a Functional Model
#'
#' @description This class holds the a blueprint for a functional model, i.e.,
#'   an unparameterized function model. Such a model is defined by a
#'   function \code{f} which accepts one scalar input \code{x} and a
#'   parameterization vector \code{par} and returns an output scalar \code{y}.
#'   The model depends on the parameterization \code{par}, which will later be
#'   subject to optimization to make the function \code{f(x, par)} fit to a
#'   model dataset \code{(x, y)}.
#'
#' @slot f the model function, taking as parameters a value \code{x} followed by
#'   a parameter vector \code{par}
#' @slot estimator is a function which takes in a vector of \code{x} and a
#'   vector of \code{y} values as well as limits \code{paramLower} and
#'   \code{paramUpper} and returns an estimate of the parameters, or \code{NULL}
#'   if no estimate can be made better than just standard random numbers
#' @slot gradient a function which takes in a value \code{x} and \code{par} and
#'   returns a vector with the gradient for each parameter dimension
#' @slot paramCount the number of model parameters
#' @slot paramLower the lower bounds for the parameters, or \code{NULL} if none
#'   are required. An element of the vector may be set of \code{NA} if no lower
#'   limit for that limit is specified (while lower limits are given for other
#'   parameter values).
#' @slot paramUpper the upper bounds for the parameters, or \code{NULL} if none
#'   are required. An element of the vector may be set of \code{NA} if no lower
#'   upper for that limit is specified (while upper limits are given for other
#'   parameter values).
#' @exportClass FunctionalModel
#' @seealso FunctionalModel.new
#' @importFrom methods setClass representation prototype
FunctionalModel <- setClass(
  Class = "FunctionalModel",
  representation = representation(f="function",
                                  paramCount="integer",
                                  gradient=".regressoR.functional.models.functionOrNULL",
                                  estimator=".regressoR.functional.models.functionOrNULL",
                                  paramLower=".regressoR.functional.models.vectorOrNULL",
                                  paramUpper=".regressoR.functional.models.vectorOrNULL"),
  prototype=prototype(gradient=NULL, estimator=NULL, paramLower=NULL, paramUpper=NULL),
  validity = function(object) {
    # check model function
    if(base::is.null(object@f) || (!(base::is.function(object@f)))){
      return("Model function must be non-null and a proper function.")
    }
    if(base::is.primitive(object@f)) {
      f.args <- base::formals(base::args(object@f));
    } else {
      f.args <- base::formals(object@f);
    }
    if ((base::length(f.args) != 2L) || (!(base::identical(base::names(f.args), base::c("x", "par"))))) {
      return("Model function must take exactly two arguments two arguments named 'x' and 'par'.");
    }

    # check the parameter count
    if (base::is.null(object@paramCount) ||
        base::is.na(object@paramCount)[1] ||
        (!(base::is.finite(object@paramCount))) ||
        (!(base::is.integer(object@paramCount))) ||
        (object@paramCount <= 0)) {
      return("Model parameter count must be an integer and bigger or equal to 1.");
    }

    # check the lower parameter limits
    if(!(base::is.null(object@paramLower))) {
      if(!(base::is.vector(object@paramLower))){
        return("Lower parameter bounds must be vector if specified.");
      }
      if(base::length(object@paramLower) != object@paramCount){
        return("Length of lower parameter bounds vector must be the same as parameter count.");
      }
      if(base::any(base::is.nan(object@paramLower), na.rm=TRUE)) {
        return("Lower limit cannot be NaN.");
      }
      if(base::any(object@paramLower >= +Inf, na.rm=TRUE)) {
        return("Lower limit cannot be positive infinite.");
      }
    }

    # check the upper parameter limits
    if(!(base::is.null(object@paramUpper))){
      if(!(base::is.vector(object@paramUpper))){
        return("Upper parameter bounds must be vector if specified.");
      }
      if(base::length(object@paramUpper) != object@paramCount){
        return("Length of upper parameter bounds vector must be the same as parameter count.");
      }
      if(base::any(base::is.nan(object@paramUpper), na.rm=TRUE)) {
        return("Upper limit cannot be NaN.");
      }
      if(base::any(object@paramUpper <= -Inf, na.rm=TRUE)) {
        return("Upper limit cannot be negative infinite.");
      }
    }

    # check that upper and lower bounds do not collide
    if(!(base::is.null(object@paramLower) || base::is.null(object@paramUpper))) {
      if(!(base::all(object@paramLower <= object@paramUpper, na.rm = TRUE))) {
        return("Lower bounds of parameters must be less or equal to upper bounds.");
      }
    }

    # check gradient function
    if(!(base::is.null(object@gradient))) {
      if(!(base::is.function(object@gradient))) {
        return("Gradient must be a function if specified.");
      }
      if(base::is.primitive(object@gradient)) {
        gradient.args <- base::formals(base::args(object@gradient));
      } else {
        gradient.args <- base::formals(object@gradient);
      }
      if ((base::length(gradient.args) != 2L) ||
          (!(base::identical(base::names(gradient.args), base::c("x", "par"))))) {
        return("Model gradient function must take exactly two arguments two arguments named 'x' and 'par'.");
      }
    }

    # check estimator function
    if(!(base::is.null(object@estimator))) {
      if(!(base::is.function(object@estimator))) {
        return("Estimator must be a function if specified.");
      }
      if(base::is.primitive(object@estimator)) {
        estimator.args <- base::formals(base::args(object@estimator));
      } else {
        estimator.args <- base::formals(object@estimator);
      }
      if ((base::length(estimator.args) != 4L) ||
          (!(base::identical(base::names(estimator.args), base::c("x", "y", "paramLower", "paramUpper"))))) {
        return("Model estimator function must take exactly two arguments two arguments named 'x', 'y', 'paramLower', and 'paramUpper'.");
      }
    }

    return(TRUE);
  }
)



#' @title Create a new instance of \code{\link{FunctionalModel}}
#' @description Instantiate the class \code{\link{FunctionalModel}}.
#'   Negative infinite lower limits will be aliased to \code{NA}. If all lower
#'   limits are \code{NA}, the lower limits vector will be set to \code{NULL}.
#'   Alias all positive infinite upper limits to \code{NA}. If all upper limits
#'   are \code{NA}, the vector of upper limits will be set to \code{NULL}.
#' @param f the model function, taking as parameters a value \code{x} followed
#'   by a parameter vector \code{par}
#' @param estimator is a function which takes in a vector of \code{x} and a
#'   vector of \code{y} values as well as limits \code{paramLower} and
#'   \code{paramUpper} and returns an estimate of the parameters, or \code{NULL}
#'   if no estimate can be made better than just standard normal random numbers
#' @param gradient a function which takes in a value \code{x} and \code{par} and
#'   returns a vector with the gradient for each parameter dimension
#' @param paramCount the number of model parameters
#' @param paramLower the lower bounds for the parameters, or \code{NULL} if none
#'   are required. An element of the vector may be set of \code{NA} if no lower
#'   limit for that limit is specified (while lower limits are given for other
#'   parameter values).
#' @param paramUpper the upper bounds for the parameters, or \code{NULL} if none
#'   are required. An element of the vector may be set of \code{NA} if no lower
#'   upper for that limit is specified (while upper limits are given for other
#'   parameter values).
#' @return the new functional functional model
#' @export FunctionalModel.new
#' @importFrom methods new validObject
FunctionalModel.new <- function(f, paramCount, gradient=NULL, estimator=NULL,
                                paramLower=NULL, paramUpper=NULL) {

  if(!(base::is.null(paramLower))) {
    # Alias negative infinite lower limits to NA.
    # Alias lower limits to NULL if they are all NA.
    paramLower[paramLower <= -Inf] <- NA;
    if(base::all(base::is.na(paramLower))) {
      # If all values are NA, then we don't need a lower bound
      paramLower <- NULL;
    }
  }

  if(!(base::is.null(paramUpper))) {
    # Alias positive infinite upper limits to NA.
    # Alias upper limits to NULL if they are all NA.
    paramUpper[paramLower >= +Inf] <- NA;
    if(base::all(base::is.na(paramUpper))) {
      # If all values are NA, then we don't need an upper bound
      paramUpper <- NULL;
    }
  }

  # Construct the instance.
  result <- methods::new("FunctionalModel",
                        f=f, paramCount=paramCount, gradient=gradient,
                             estimator=estimator, paramLower=paramLower,
                        paramUpper=paramUpper);
  result <- base::force(result);
  result@f <- base::force(result@f);
  result@paramCount <- base::force(result@paramCount);
  result@gradient <- base::force(result@gradient);
  result@estimator <- base::force(result@estimator);
  result@paramLower <- base::force(result@paramLower);
  result@paramUpper <- base::force(result@paramUpper);
  result <- base::force(result);
  methods::validObject(result);
  return(result);
}