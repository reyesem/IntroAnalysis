% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_lm.R
\name{bootstrap_parametric_linear}
\alias{bootstrap_parametric_linear}
\title{Perform parametric bootstrap for linear models without constant variance.}
\usage{
bootstrap_parametric_linear(fit, reps)
}
\arguments{
\item{fit}{\code{lm} object defining the model for which the parameters are
estimated.}

\item{reps}{scalar; number of bootstrap replications to perform.}
}
\value{
matrix with the same number of rows as coefficients in \code{fit} and
\code{reps} columns. Each row contains the bootstrap estimates of the
corresponding parameters.
}
\description{
This is a workhorse function which performs a parametric bootstrap without
assuming constant variance. This is not meant to be called by the user
directly.
}
