% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parCheck.R
\name{FunctionalModel.par.check}
\alias{FunctionalModel.par.check}
\title{Check that model parameters are acceptable for the functional model}
\usage{
FunctionalModel.par.check(model, par)
}
\arguments{
\item{model}{the functional model}

\item{par}{the parameter vector to be checked}
}
\value{
\code{TRUE} if the parameter vector is OK, \code{FALSE} otherwise
}
\description{
Check the the model parameter vector has the right length, all
  finite elements, and fits to the boundaries specified in the functional
  model.
}
