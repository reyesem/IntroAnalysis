#' Plot the sampling distribution of a statistic.
#'
#' Provides a wrapper for plotting a model for the sampling distribution of a
#' parameter estimate in a linear model.
#'
#' The \code{\link{estimate_parameters}} function returns an attribute that
#' contains a random sample from the sampling distribution of the parameter
#' estimate.  This is extracted and used to construct a graphic summarizing
#' this distribution.
#'
#' @param estimates result of a call to \code{\link{estimate_parameters}}.
#' @param parameters character string indicating for which parameter estimates
#' the sampling distribution should be plotted. Each will appear in a different
#' facet of the same graphic. If missing (default), all sampling distributions
#' are constructed.
#' @param conf.int boolean indicating whether the confidence region should be
#' constructed on the graphic.  If \code{TRUE}, the region is drawn; if
#' \code{FALSE} (default), the region is not drawn.
#'
#' @return \code{ggplot2} object.
#'
#' @seealso \code{\link{estimate_parameters}}
#'
#' @examples
#' test.df <- data.frame(x = seq(10), y = seq(10) + rnorm(10))
#' fit <- lm(y ~ x, data = test.df)
#' samp_distn <- estimate_parameters(fit,
#' assume.constant.variance = TRUE,
#' assume.normality = FALSE)
#' plot_sampling_distribution(samp_distn)
#'
#' @export
plot_sampling_distribution <- function(estimates,
                                       parameters,
                                       conf.int = FALSE){
  # restructure data for easy plotting
  .boot <- as.data.frame(attr(estimates, "Sampling Distribution"))
  .boot <- tidyr::gather(.boot, key = "term", value = "estimate")

  if (missing(parameters)){
    parameters <- attr(attr(estimates, "Sampling Distribution"),
                       "dimnames")[[2]]
  } else {
    .boot <- dplyr::filter(.boot,
                           is.element(term, parameters))

    estimates <- dplyr::filter(estimates,
                               is.element(term, parameters))
  }

  .out <- ggplot2::qplot(data = .boot, x = estimate, geom = "density") +
    ggplot2::labs(x = "Parameter Estimate Across Repeated Samples") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())

  if (conf.int){
    colnames(estimates)[c(4,5)] <- c("lower", "upper")
    .out <- .out +
      ggplot2::geom_vline(data = estimates,
                          mapping = ggplot2::aes(xintercept = lower),
                          color = "red", linetype = 2) +
      ggplot2::geom_vline(data = estimates,
                          mapping = ggplot2::aes(xintercept = upper),
                          color = "red", linetype = 2)
  }

  if (length(parameters) > 1){
    .out <- .out + ggplot2::facet_wrap(~ term,
                                       scales = "free")
  }

  return(.out)
}
