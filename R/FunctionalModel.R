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
#'   vector of \code{y} values and returns an estimate of the parameters, or \code{NULL}
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
#' @slot name a textual name of the model
#' @exportClass FunctionalModel
#' @seealso FunctionalModel.new
#' @importFrom methods setClass representation prototype
#' @importFrom utilizeR function.args
#' @importClassesFrom utilizeR functionOrNULL
#' @importClassesFrom utilizeR numericOrNULL
FunctionalModel <- setClass(
  Class = "FunctionalModel",
  representation = representation(f="function",
                                  paramCount="integer",
                                  gradient="functionOrNULL",
                                  estimator="functionOrNULL",
                                  paramLower="numericOrNULL",
                                  paramUpper="numericOrNULL",
                                  name="character"),
  prototype=prototype(gradient=NULL, estimator=NULL, paramLower=NULL, paramUpper=NULL),
  validity = function(object) {
    # check model function
    if(is.null(object@f) || (!(is.function(object@f)))){
      return("Model function must be non-null and a proper function.")
    }
    if (!(identical(function.args(object@f), c("x", "par")))) {
      return("Model function must take exactly two arguments two arguments named 'x' and 'par'.");
    }

    # check the parameter count
    if (is.null(object@paramCount) ||
        is.na(object@paramCount)[1] ||
        (!(is.finite(object@paramCount))) ||
        (!(is.integer(object@paramCount))) ||
        (object@paramCount <= 0)) {
      return("Model parameter count must be an integer and bigger or equal to 1.");
    }

    # check the lower parameter limits
    if(!(is.null(object@paramLower))) {
      if(!(is.vector(object@paramLower))){
        return("Lower parameter bounds must be vector if specified.");
      }
      if(length(object@paramLower) != object@paramCount){
        return("Length of lower parameter bounds vector must be the same as parameter count.");
      }
      if(any(is.nan(object@paramLower), na.rm=TRUE)) {
        return("Lower limit cannot be NaN.");
      }
      if(any(object@paramLower >= +Inf, na.rm=TRUE)) {
        return("Lower limit cannot be positive infinite.");
      }
    }

    # check the upper parameter limits
    if(!(is.null(object@paramUpper))){
      if(!(is.vector(object@paramUpper))){
        return("Upper parameter bounds must be vector if specified.");
      }
      if(length(object@paramUpper) != object@paramCount){
        return("Length of upper parameter bounds vector must be the same as parameter count.");
      }
      if(any(is.nan(object@paramUpper), na.rm=TRUE)) {
        return("Upper limit cannot be NaN.");
      }
      if(any(object@paramUpper <= -Inf, na.rm=TRUE)) {
        return("Upper limit cannot be negative infinite.");
      }
    }

    # check that upper and lower bounds do not collide
    if(!(is.null(object@paramLower) || is.null(object@paramUpper))) {
      if(!(all(object@paramLower <= object@paramUpper, na.rm = TRUE))) {
        return("Lower bounds of parameters must be less or equal to upper bounds.");
      }
    }

    # check gradient function
    if(!(is.null(object@gradient))) {
      if(!(is.function(object@gradient))) {
        return("Gradient must be a function if specified.");
      }
      if (!(identical(function.args(object@gradient), c("x", "par")))) {
        return("Model gradient function must take exactly two arguments two arguments named 'x' and 'par'.");
      }
    }

    # check estimator function
    if(!(is.null(object@estimator))) {
      if(!(is.function(object@estimator))) {
        return("Estimator must be a function if specified.");
      }
      if (!(identical(function.args(object@estimator), c("x", "y")))) {
        return("Model estimator function must take exactly two arguments two arguments named 'x', and 'y'.");
      }
    }

    if(is.null(object@name) || (nchar(object@name) <= 0L)) {
      return("Model name must be a non-empty character string.");
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
#' @param name the name of the model
#' @return the new functional functional model
#' @export FunctionalModel.new
#' @importFrom methods new validObject
#' @importFrom utilizeR function.toString
FunctionalModel.new <- function(f, paramCount, gradient=NULL, estimator=NULL,
                                paramLower=NULL, paramUpper=NULL,
                                name=function.toString(f)) {

  if(!(is.null(paramLower))) {
    # Alias negative infinite lower limits to NA.
    # Alias lower limits to NULL if they are all NA.
    paramLower[paramLower <= -Inf] <- NA;
    if(all(is.na(paramLower))) {
      # If all values are NA, then we don't need a lower bound
      paramLower <- NULL;
    }
  }

  if(!(is.null(paramUpper))) {
    # Alias positive infinite upper limits to NA.
    # Alias upper limits to NULL if they are all NA.
    paramUpper[paramLower >= +Inf] <- NA;
    if(all(is.na(paramUpper))) {
      # If all values are NA, then we don't need an upper bound
      paramUpper <- NULL;
    }
  }

  # setup the name
  if(is.null(name)) {
    name <- function.toString(f);
  }

  # Construct the instance.
  result <- methods::new("FunctionalModel",
                         f=f, paramCount=paramCount, gradient=gradient,
                         estimator=estimator, paramLower=paramLower,
                         paramUpper=paramUpper, name=name);
  result <- force(result);
  result@f <- force(result@f);
  result@paramCount <- force(result@paramCount);
  result@gradient <- force(result@gradient);
  result@estimator <- force(result@estimator);
  result@paramLower <- force(result@paramLower);
  result@paramUpper <- force(result@paramUpper);
  result@name <- force(result@name);
  result <- force(result);
  methods::validObject(result);
  return(result);
}

#' @title Convert a \code{\link{FunctionalModel}} to a String
#' @description the \code{as.character} implementation for
#'   \code{\link{FunctionalModel}}
#' @param x the object
#' @return the name of the object
#' @importFrom methods setMethod
#' @name as.character
#' @aliases as.character,FunctionalModel-method
methods::setMethod("as.character", "FunctionalModel", function (x) x@name)
