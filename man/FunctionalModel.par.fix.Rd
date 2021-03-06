% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parFix.R
\name{FunctionalModel.par.fix}
\alias{FunctionalModel.par.fix}
\title{Fix a Vector to be Contained in the Lower/Upper Boundaries}
\usage{
FunctionalModel.par.fix(par, lower = NULL, upper = NULL,
  paramCount = length(par))
}
\arguments{
\item{par}{the parameter vector to fix}

\item{lower}{the lower boundaries, or \code{NULL} if no lower boundaries are
specified}

\item{upper}{the upper boundaries, or \code{NULL} if no upper boundaries are
specified}

\item{paramCount}{the parameter count, by default the length of \code{par}}
}
\description{
Make sure that a vector consists of only finite values within
  the specified boundaries. This method can be used to clean the result of
  a stochastic parameter estimation step to make sure that all synthesized
  parameters are finite and within the specified boundaries.
}
