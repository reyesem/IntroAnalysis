#' Compute p-value from a bootstrap sample of test statistics.
#'
#' Given bootstrap estimates of a test statistic, compute a p-value.
#'
#' The "two-sided" p-value is computed in that the test statistic considered is
#' the F-statistic from a nested F-test.
#'
#' @param bootobj vector containing the bootstrap test statistics returned by
#' \code{\link{boot_residual_p}}.
#'
#' @return data.frame containing an ANOVA table comparing the two models. The
#' p-value is determined from the bootstrap test statistics.
#'
#' @seealso \code{\link[stats]{anova}}
#'
#' @examples
#' \dontrun{
#' test.df <- data.frame(x = seq(10), y = seq(10) + rnorm(10))
#' fit1 <- lm(y ~ x, data = test.df)
#' fit0 <- lm(y ~ 1, data = test.df)
#' bootstrap_test_stats <- boot_residual_p(fit1, fit0, reps = 4999, wild = F)
#' boot_compute_p(bootstrap_test_stats)
#' }
#'
#' @import stats
boot_compute_p <- function(bootobj){
  .anova <- attr(bootobj, "ANOVA Table")
  .tstat <- .anova$statistic[1]
  .pvalue <- mean(c(bootobj, .tstat) >= .tstat)

  .anova$p.value[1] <- .pvalue

  .anova
}
