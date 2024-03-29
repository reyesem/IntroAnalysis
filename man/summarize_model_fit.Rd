% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modeling_phrases.R
\name{summarize_model_fit}
\alias{summarize_model_fit}
\alias{summarise_model_fit}
\title{Produce metrics for the quality of the model fit.}
\usage{
summarize_model_fit(mean.model)

summarise_model_fit(mean.model)
}
\arguments{
\item{mean.model}{\code{lm} or \code{glm} model fit defining the model for
which the goodness of fit should be summarized.}
}
\value{
A \code{data.frame} containing one row of summary metrics.
}
\description{
A wrapper for \code{\link[generics]{glance}} which obtains metrics on the
goodness of fit of the model. For the linear model in particular, some
metrics are suppressed.
}
\examples{
fit <- lm(mpg ~ hp, data = mtcars)
summarize_model_fit(fit)

}
