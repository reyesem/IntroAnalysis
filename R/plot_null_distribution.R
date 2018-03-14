#' Plot the null distribution of a test statistic.
#'
#' Provides a wrapper for plotting a model for the null distribution of a test
#' statistic comparing two linear models.
#'
#' The \code{\link{compare_models}} function returns an attribute that
#' contains a random sample from the null distribution of the test statistic.
#' This is extracted and used to construct a graphic summarizing this
#' distribution.
#'
#' @param statistics result of a call to \code{\link{compare_models}}.
#' @param p.value boolean indicating whether the p-value region should be
#' drawn on the graphic.  If \code{TRUE}, the region is drawn; if
#' \code{FALSE} (default), the region is not drawn.
#'
#' @return \code{ggplot2} object.
#'
#' @seealso \code{\link{compare_models}}
#'
#' @examples
#' test.df <- data.frame(x = seq(10), y = seq(10) + rnorm(10))
#' fit <- lm(y ~ x, data = test.df)
#' fit0 <- lm(y ~ 1, data = test.df)
#' null_distn <- compare_models(fit, fit0,
#' assume.constant.variance = TRUE,
#' assume.normality = FALSE)
#' plot_null_distribution(null_distn)
#'
#' @export
plot_null_distribution <- function(statistics,
                                   p.value = FALSE){
  # restructure data for easy plotting
  .boot <- data.frame(statistic = attr(statistics, "Null Distribution"))

  .out <- ggplot2::qplot(data = .boot, x = statistic, geom = "density") +
    ggplot2::labs(x = paste("Standardized Statistic Across Repeated",
                            "Samples Under Null Hypothesis")) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())

  if (p.value){
    .out <- .out +
      ggplot2::geom_vline(data = data.frame(stat = statistics$statistic[1]),
                          mapping = ggplot2::aes(xintercept = stat),
                          color = "red", linetype = 2)
  }

  return(.out)
}
