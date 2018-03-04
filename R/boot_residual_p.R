#' Compare nested linear models via (wild) residual bootstrap.
#'
#' This is the workhorse for the \code{compare_models()} function.
#' Given two linear models, one nested within the other, the null distribution
#' of the test statistic comparing the two models is estimated via a
#' residual bootstrap (or wild bootstrap). This is not meant to be called by
#' the user directly.
#'
#' @param fit1 \code{lm} object defining the full model.
#' @param fit0 \code{lm} object defining the reduced model.
#' @param reps scalar; number of bootstrap replications to perform.
#' @param wild boolean; if \code{TRUE}, a wild bootstrap is performed. If
#' \code{FALSE} a traditional residual bootstrap is performed.
#' @param construct string defining the type of construct to use when generating
#' from the distribution for the wild bootrap (see \code{\link{rmammen}}). If
#' \code{wild = FALSE}, this is ignored.
#'
#' @return vector of length \code{reps} containing the test statistic from each
#' bootstrap replication.
#'
#' @examples
#' \dontrun{
#' test.df <- data.frame(x = seq(10), y = seq(10) + rnorm(10))
#' fit1 <- lm(y ~ x, data = test.df)
#' fit0 <- lm(y ~ 1, data = test.df)
#' boot_residual_p(fit1, fit0, reps = 4999, wild = FALSE)
#' }
#'
#' @import stats
boot_residual_p <- function(fit1,
                            fit0,
                            reps,
                            wild,
                            construct){

  .resid0 <- fit0$residuals
  .resid1 <- fit1$residuals
  .yhat0 <- fit0$fitted.values
  .n <- length(.resid0)

  if(length(.resid0) != length(.resid1)){
    stop(paste("Sample size must be equivalent for full and reduced models.",
               "Try removing missing values prior to fitting models."))
  }

  if(wild){
    # obtain new residuals through multiplication by random variable
    .booty <- (matrix(rmammen(.n*reps, construct = construct),
                      nrow = .n, ncol = reps) * .resid0) + .yhat0
  } else {
    # obtain new residuals through resampling of original residuals
    .booty <- matrix(sample(.resid0, size = .n*reps, replace = TRUE),
                     nrow = .n, ncol = reps) + .yhat0
  }

  # obtain projection matrix, accounting for no parameter models
  if (!is.null(fit0$qr)){
    .P0 <- (qr.Q(fit0$qr)) %*% t(qr.Q(fit0$qr))
  }
  .P1 <- (qr.Q(fit1$qr)) %*% t(qr.Q(fit1$qr))

  # compute test statistic
  if (!is.null(fit0$qr)){
    .num <- colSums(.booty * ((.P1 - .P0) %*% .booty))
  } else {
    .num <- colSums((.booty - 2*.yhat0) * (.P1 %*% .booty)) + (.yhat0[1]^2 * .n)
  }

  .den <- colSums(.booty * ((diag(.n) - .P1) %*% .booty))

  .tstat <- (.num/(fit0$df.residual - fit1$df.residual))/(.den/fit1$df.residual)


  # cleaned up ANOVA tables for each model
  .anova <- as.data.frame(anova(fit0, fit1))
  .anova <- cbind("Model" = c("Full Model", "Reduced Model"), .anova[c(2, 1), ])

  attr(.tstat, "ANOVA Table") <- .anova

  .tstat
}
