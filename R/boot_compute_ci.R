#' Compute confidence intervals from a bootstrap sample.
#'
#' Given bootstrap estimates of a set of parameters, compute a confidence
#' interval.
#'
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
#' @param bootobj matrix containing the bootstrap estimates of the parameters
#' returned by \code{\link{boot_residual_ci}}. There should be one row for
#' each parameter and each column contains a bootstrap estimate.
#' @param conf.level scalar between 0 and 1 indicating the confidence level
#' (default = 0.95). If a value larger than 1 is provided, it is rescaled to be
#' between 0 and 1.
#' @param type string defining the type of confidence interval to construct. If
#' \code{"percentile"} (default) an equal-tailed percentile interval is
#' constructed. If \code{"BC"} the bias-corrected percentile interval is
#' constructed. If \code{"bootstrap-t"} the bootstrap-t interval is constructed.
#'
#' @return matrix with the same number of rows as rows in \code{bootobj} and
#' 2 columns. The first column gives the lower limit of the confidence interval
#' the second column gives the upper limit of the confidence interval.
#'
#' @examples
#' \dontrun{
#' test.df <- data.frame(x = seq(10), y = seq(10) + rnorm(10))
#' fit <- lm(y ~ x, data = test.df)
#' bootstrap_estimates <- boot_residual_ci(fit, reps = 4999, wild = F)
#' boot_compute_ci(bootstrap_estimates)
#' }
#'
#' @import stats
boot_compute_ci <- function(bootobj,
                            conf.level = 0.95,
                            type = c("percentile",
                                     "BC",
                                     "bootstrap-t")){
  conf.level <- conf.level[1]

  if(conf.level<=0){
    stop("Confidence level must be positive.")
  } else if(conf.level>=100){
    stop("Confidence level must be below 100%.")
  } else if(conf.level>=1){
    conf.level <- conf.level/100
  }

  type <- match.arg(type)

  .ses <- apply(bootobj, 1, sd)
  .plow <- 0.5*(1-conf.level)
  .phigh <- 0.5*(1+conf.level)

  # compute appropriate confidence interval
  if(type=="percentile"){
    .ci <- t(apply(bootobj, 1, quantile, probs = c(.plow, .phigh)))
  } else if(type=="BC"){
    .zhat <- qnorm(rowMeans(bootobj < attr(bootobj, "original.estimates")))
    .alpha1 <- pnorm(2*.zhat + qnorm(.plow))
    .alpha2 <- 1 - pnorm(2*.zhat + qnorm(.phigh))

    .ci <- t(sapply(seq.int(nrow(bootobj)), function(u){
      quantile(bootobj[u,], probs = c(.alpha1[u], .alpha2[u]))
    }))

    colnames(.ci) <- names(quantile(0, probs=c(.plow, .phigh)))
    rownames(.ci) <- rownames(bootobj)
  } else if(type=="bootstrap-t"){
    .t <- (bootobj - attr(bootobj, "original.estimates"))/
      attr(bootobj, "std.err")

    .quants <- apply(.t, 1, quantile, probs = c(.phigh, .plow))

    .ci <- attr(bootobj, "original.estimates") -
      t(.quants)*attr(bootobj, "original.std.err")

    colnames(.ci) <- rev(colnames(.ci))
  }

  .ci
}