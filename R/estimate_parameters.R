#' Estimate the parameters of a linear model with specified confidence.
#'
#' Provides point estimates and confidence intervals for the parameters of a
#' linear model.
#'
#' This wrapper provides a single interface for estimating parameters under
#' various conditions imposed on the error term in the linear model.  A
#' table of estimates, similar to \code{\link[stats]{summary.lm}} containing
#' point and interval estimates of the parameters.  However, the interval
#' estimates can differ depending on the conditions imposed on the error term.
#' The classical approach implmented by \code{\link[stats]{summary.lm}} can
#' be recovered as well as having the interval estimates computed based on a
#' residual or wild bootstrap. The following approaches are implemented.
#' \itemize{
#' \item classical: if both homoskedasticity and normality are assumed, the
#' sampling distribution of each estimate is modeled by an t-distribution.
#' \item White-Huber correction: if normality can be assumed but
#' homoskedasticity cannot, the variance-covariance matrix is replaced by the
#' White-Huber consistent variance-covariance estimate when computing the
#' standard errors, and the sampling distribution is modeled by a
#' t-distribution.
#' \item residual bootstrap: if homoskedasticity can be assumed, but normality
#' cannot, a residual bootstrap is used to compute the confidence intervals.
#' \item wild bootstrap: if neither homoskedasticity nor normality is assumed,
#' a wild bootstrap is used to compute the confidence intervals.}
#' We do not implement methods which assume normality but not homoskedasticity.
#' Further, all methods make additional requirements regarding independence of
#' the error terms and that the model has been correctly specified. The work is
#' done primarily by the workhorse function \code{\link{boot_residual_ci}}.
#' The percentile interval is constructed by taking the empirical
#' \eqn{100\alpha} and \eqn{100(1-\alpha)} percentiles from the bootstrap
#' values. If \eqn{\hat{F}} is the empirical distribution function of the
#' bootstrap values, then the \eqn{100(1 - 2\alpha)}% percentile interval is
#' given by
#' \deqn{(\hat{F}^{-1}(\alpha), \hat{F}^{-1}(1-\alpha))}
#' The bias-corrected (BC) interval corrects for median-bias.  It is given by
#' \deqn{(\hat{F}^{-1}(\alpha_1), \hat{F}^{-1}(1-\alpha_2))}
#' where
#' \deqn{\alpha_1 = \Phi{2\hat{z}_0 + \Phi^{-1}(\alpha)}}
#' \deqn{\alpha_2 = 1 - \Phi{2\hat{z}_0 + \Phi^{-1}(1-\alpha)}}
#' \deqn{\hat{z}_0 = \Phi^{-1}(\hat{F}(\hat{\beta}))}
#' where \eqn{\hat{\beta}} is the estimate from the original sample.
#' The bootstrap-t interval is based on the bootstrap distribution of
#' \deqn{t^{b} = \frac{\hat{\beta}^{b} -
#' \hat{\beta}}{\hat{\sigma}^{b}}}
#' where \eqn{\hat{\sigma}} is the estimate of the standard error of
#' \eqn{\hat{\beta}} and the superscript b denotes a bootstrap sample. Let
#' \eqn{\hat{G}} be the empirical distribution function of the bootstrap
#' standardized statistics given above.  Then, the bootstrap-t interval is given
#' by
#' \deqn{(\hat{\beta} - \hat{\sigma}\hat{G}^{-1}(1-\alpha),
#' \hat{\beta} - \hat{\sigma}\hat{G}^{-1}\alpha)}
#'
#' @param fit \code{lm} model fit defining the linear model and the parameters.
#' @param conf.level scalar between 0 and 1 indicating the confidence level
#' (default = 0.95).
#' @param assume.constant.variance boolean; if \code{TRUE} (default),
#' homoskedasticity is assumed for the error term. If \code{FALSE}, this is not
#' assumed.
#' @param assume.normality boolean; if \code{TRUE} (default), the errors are
#' assumed to follow a Normal distribution. If \code{FALSE}, this is not
#' assumed. This is ignored if \code{assume.constant.variance = FALSE}.
#' @param bootstrap.reps scalar indicating the number of bootstrap replications
#' to use (default = 4999).  This is ignored if both
#' \code{assume.constant.variance = TRUE} and
#' \code{assume.normality = TRUE}.
#' @param construct string defining the type of construct to use when generating
#' from the distribution for the wild bootrap (see \code{\link{rmammen}}). If
#' \code{assume.constant.variance = TRUE}, this is ignored
#' (default = \code{"normal-2"}).
#' @param type string defining the type of confidence interval to construct. If
#' \code{"percentile"} (default) an equal-tailed percentile interval is
#' constructed. If \code{"BC"} the bias-corrected percentile interval is
#' constructed. If \code{"bootstrap-t"} the bootstrap-t interval is constructed.
#'
#' @return data.frame containing a table of parameter estimates. The object
#' has an additional attribute "Sampling Distribution" which is a matrix with
#' \code{bootstrap.reps} rows and the same number of columns as parameters in
#' \code{fit}.  Each column contains a sample from the corresponding model of
#' the sampling distribution. This is useful for plotting the distribution for
#' pedagogical purposes.
#'
#' @seealso \code{\link[stats]{summary.lm}}, \code{\link{boot_residual_ci}}
#'
#' @examples
#' test.df <- data.frame(x = seq(10), y = seq(10) + rnorm(10))
#' fit <- lm(y ~ x, data = test.df)
#' estimate_parameters(fit,
#' assume.constant.variance = TRUE,
#' assume.normality = FALSE)
#'
#' @import stats
#' @export
estimate_parameters <- function(fit,
                                conf.level = 0.95,
                                assume.constant.variance = TRUE,
                                assume.normality = TRUE,
                                bootstrap.reps = 4999,
                                construct = "normal-2",
                                type = c("percentile",
                                         "BC",
                                         "bootstrap-t")){
  if(missing(fit)){
    stop("A valid linear model must be specified.")
  }

  conf.level <- conf.level[1]

  if(conf.level<=0){
    stop("Confidence level must be positive.")
  } else if(conf.level>=100){
    stop("Confidence level must be below 100%.")
  } else if(conf.level>=1){
    conf.level <- conf.level/100
  }

  .lowername <- paste(100*conf.level, "% lower", sep = "")
  .uppername <- paste(100*conf.level, "% upper", sep = "")

  .ests <- summary(fit)$coefficients
  .ests <- data.frame(
    term = rownames(.ests),
    estimate = .ests[, "Estimate"],
    std.error = .ests[, "Std. Error"]
  )

  if (assume.normality && assume.constant.variance){
    .ci <- confint(fit, level = conf.level)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )

    # compute sampling distributions
    .boot <- matrix(rt(bootstrap.reps, df = fit$df.residual),
                    nrow = nrow(.ests),
                    ncol = bootstrap.reps,
                    byrow = TRUE)
    .boot <- .boot*.ests$std.error + .ests$estimate
  } else if (assume.normality && !assume.constant.variance){
    # use White-Huber sandwich covariance matrix
    .Sigma <- car::hccm(fit, type = "hc0")
    .ci <- adjconfint(coef(fit),
                      .Sigma,
                      conf.level,
                      fit$df.residual)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )

    .ests$std.error <- sqrt(diag(.Sigma))

    # compute sampling distributions
    .boot <- matrix(rt(bootstrap.reps, df = fit$df.residual),
                    nrow = nrow(.ests),
                    ncol = bootstrap.reps,
                    byrow = TRUE)
    .boot <- .boot*.ests$std.error + .ests$estimate
  } else {
    .boot <- boot_residual_ci(fit, reps = bootstrap.reps,
                              wild = !assume.constant.variance,
                              construct = construct)

    .ses <- apply(.boot, 1, sd)
    .ci <- boot_compute_ci(.boot, conf.level = conf.level, type = type)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )

    .ests$std.error <- .ses
  }

  colnames(.ests)[c(4,5)] <- c(.lowername, .uppername)
  rownames(.ests) <- NULL

  attributes(.boot) <- list(dim = c(nrow(.ests), bootstrap.reps),
                            dimnames = list(.ests$term,
                                            NULL))
  attr(.ests, "Sampling Distribution") <- t(.boot)
  .ests
}
