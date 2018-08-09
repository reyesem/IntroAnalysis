#' Compute p-value from a bootstrap sample of test statistics.
#'
#' Given bootstrap estimates of a test statistic, compute a p-value (always the
#' "two-sided" p-value).
#'
#' @param bootobj vector containing the bootstrap test statistics returned by a
#' workhorse bootstrap function under the null hypothesis:
#' \itemize{
#'   \item bootstrap_residual_null
#'   \item bootstrap_parametric_linear_null
#'   \item bootstrap_parametric_null
#'   \item bootstrap_case_null}
#' There should be one row for each parameter and each column contains a
#' bootstrap estimate.
#'
#' @return data.frame containing an ANOVA table comparing the two models. The
#' p-value is determined from the bootstrap test statistics.
#'
#' @seealso \code{\link[stats]{anova}}
#'
#' @import stats
bootstrap_compute_p <- function(bootobj){
  .anova <- attr(bootobj, "ANOVA Table")
  .tstat <- .anova$statistic[1]
  .pvalue <- mean(c(bootobj, .tstat) >= .tstat)

  .anova$p.value[1] <- .pvalue

  .anova
}
