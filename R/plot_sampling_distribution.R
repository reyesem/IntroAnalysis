#' Plot the sampling distribution of a statistic.
#'
#' Provides a wrapper for plotting a model for the sampling distribution of a
#' parameter estimate in a linear model.
#'
#' The [estimate_parameters()] function returns an attribute that
#' contains a random sample from the sampling distribution of the parameter
#' estimate.  This is extracted and used to construct a graphic summarizing
#' this distribution.
#'
#' @param estimates result of a call to [estimate_parameters()].
#' @param parameter character string indicating for which parameter the modeled
#' sampling distribution should be plotted. This should match the output of the
#' "term" column in [estimate_parameters()] output. If missing
#' (default), the model for the sampling distribution of the first term is
#' plotted.
#' @param show.confidence.interval boolean indicating whether the confidence
#' region should be constructed on the graphic.  If \code{TRUE}, the region is
#' drawn; if \code{FALSE} (default), the region is not drawn.
#'
#' @return \code{ggplot2} object.
#'
#' @seealso [estimate_parameters()]
#'
#' @examples
#' fit <- lm(mpg ~ hp, data = mtcars)
#'
#' samp_distn <- estimate_parameters(fit,
#'   confidence.level = 0.95,
#'   assume.constant.variance = TRUE,
#'   assume.normality = FALSE)
#'
#' plot_sampling_distribution(samp_distn,
#'   show.confidence.interval = TRUE)
#'
#' @importFrom rlang .data
#' @export
plot_sampling_distribution <- function(estimates,
                                       parameter,
                                       show.confidence.interval = FALSE){

  if (is.null(.boot <- attr(estimates, "Sampling Distribution"))){
    stop(paste0("Sample from sampling distribution not taken. ",
                "Be sure to specify 'confidence.level' in the ",
                "'estimate_parameters()' function."))
  }

  # estimate density (similar to ggplot2 but with higher n)
  .boot <- as.data.frame(.boot)

  if (missing(parameter)) parameter <- colnames(.boot)[1]
  .boot <- subset(.boot, select = is.element(colnames(.boot), parameter))
  estimates <- dplyr::filter(estimates, is.element(.data$term, parameter))

  .dens <- density(.boot[, 1], n = 1024,
                   from = min(.boot[, 1]), to = max(.boot[, 1]))
  .dens <- data.frame(.dens[c(1, 2)])

  .out <- ggplot2::ggplot(data = .dens,
                          mapping = ggplot2::aes(x = .data$x,
                                                 y = .data$y)) +
    ggplot2::geom_area(fill = NA, color = "black") +
    ggplot2::labs(x = "Parameter Estimate Across Repeated Samples") +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())

  if (show.confidence.interval){
    colnames(estimates)[c(4, 5)] <- c("lower", "upper")
    .dens2 <- subset(.dens, .dens$x <= estimates$lower[1])
    .dens3 <- subset(.dens, .dens$x >= estimates$upper[1])

    .out <- .out +
      ggplot2::geom_area(data = .dens2, fill = "red", color = "black") +
      ggplot2::geom_area(data = .dens3, fill = "red", color = "black") +
      ggplot2::geom_vline(data = estimates,
                          mapping = ggplot2::aes(xintercept = .data$lower),
                          color = "red", linetype = 2) +
      ggplot2::geom_vline(data = estimates,
                          mapping = ggplot2::aes(xintercept = .data$upper),
                          color = "red", linetype = 2)
  }

  return(.out)
}
