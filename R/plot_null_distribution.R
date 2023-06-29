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
#' @param show.pvalue boolean indicating whether the p-value region should
#' be drawn on the graphic.  If \code{TRUE}, the region is drawn; if
#' \code{FALSE} (default), the region is not drawn.
#'
#' @return \code{ggplot2} object.
#'
#' @seealso \code{\link{compare_models}}
#'
#' @examples
#' fit1 <- lm(mpg ~ hp, data = mtcars)
#' fit0 <- lm(mpg ~ 1, data = mtcars)
#'
#' null_distn <- compare_models(fit1, fit0,
#'   assume.identically.distributed = TRUE,
#'   assume.normality = FALSE)
#'
#' plot_null_distribution(null_distn,
#'   show.pvalue = TRUE)
#'
#' @importFrom rlang .data
#' @export
plot_null_distribution <- function(statistics,
                                   show.pvalue = FALSE){

  # estimate density (similar to ggplot2 but with higher n)
  .stats <- attr(statistics, "Null Distribution")
  .dens <- density(.stats, n = 1024, from = min(.stats), to = max(.stats))
  .dens <- data.frame(.dens[c(1, 2)])

  .out <- ggplot2::ggplot(data = .dens,
                          mapping = ggplot2::aes(x = .data$x,
                                                 y = .data$y)) +
    ggplot2::geom_area(fill = NA, color = "black") +
    ggplot2::labs(x = paste("Standardized Statistic Across Repeated",
                            "Samples Under Null Hypothesis")) +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank())

  if (show.pvalue){
    .tstat <- statistics$standardized.statistic[1]
    .dens2 <- subset(.dens, .dens$x >= .tstat)

    .out <- .out +
      ggplot2::geom_area(data = .dens2,
                         fill = "red", color = "black") +
      ggplot2::geom_vline(data = data.frame(`stat` = .tstat),
                          mapping = ggplot2::aes(xintercept =
                                                   .data$stat),
                          color = "red", linetype = 2)
  }

  return(.out)
}
