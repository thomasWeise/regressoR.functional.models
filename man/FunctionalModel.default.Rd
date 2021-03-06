% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default.R
\name{FunctionalModel.default}
\alias{FunctionalModel.default}
\title{Obtain a List with the Default Functional Models Provided by this
  Library}
\usage{
FunctionalModel.default()
}
\value{
a list with the default models provided by this library.
}
\description{
This function returns a list with the default models provided by
  this library. Currently, these comprise standard constant, linear,
  quadratic, and cubic models as well as several sigmoid-style models such as
  exponential decay, logistic models, the Gompertz model, and an exponential
  log-linear model. This also includes some multi-layer perceptrons (MLPs).
}
\seealso{
FunctionalModel.new

FunctionalModel.constant

FunctionalModel.linear

FunctionalModel.quadratic

FunctionalModel.cubic

FunctionalModel.exponentialDecay.1

FunctionalModel.exponentialDecay.2

FunctionalModel.logistic.1

FunctionalModel.logistic.2

FunctionalModel.gompertz.1

FunctionalModel.gompertz.2

FunctionalModel.expLogLinear.1

FunctionalModel.expLogLinear.2

FunctionalModel.mlp.default

FunctionalModel.monotonous
}
