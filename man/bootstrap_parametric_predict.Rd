% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap_glm.R
\name{bootstrap_parametric_predict}
\alias{bootstrap_parametric_predict}
\title{Perform parametric bootstrap for predictions of generalized linear model.}
\usage{
bootstrap_parametric_predict(fit, newdata, reps)
}
\arguments{
\item{fit}{\code{glm} object defining the model for which the parameters are
estimated.}

\item{newdata}{new data set used to construct the predictions.}

\item{reps}{scalar; number of bootstrap replications to perform.}
}
\value{
matrix with the same number of rows as rows of \code{newdat} and
\code{reps} columns. Each row contains the bootstrap estimates of the
corresponding prediction.
}
\description{
This is the workhorse for the \code{estimate_mean_response()} function when a
generalized linear model is specified and parametric bootstrap is required.
This is not meant to be called by the user directly.
}
\examples{
\dontrun{
fit <- glm(mpg ~ hp, data = mtcars)
bootstrap_parametric_predict(fit, mtcars, reps = 4999)
}

}
