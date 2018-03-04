#' Perform (wild) residual bootstrap for linear model.
#'
#' This is the workhorse for the \code{estimate_parameters()} function.
#' Given a linear model fit, performs a residual bootstrap (or wild bootstrap)
#' to estimate the sampling distribution of each of the parameters in the model.
#' This is not meant to be called by the user directly.
#'
#' @param fit \code{lm} object defining the model for which the parameters are
#' estimated.
#' @param reps scalar; number of bootstrap replications to perform.
#' @param wild boolean; if \code{TRUE}, a wild bootstrap is performed. If
#' \code{FALSE} a traditional residual bootstrap is performed.
#' @param construct string defining the type of construct to use when generating
#' from the distribution for the wild bootrap (see \code{\link{rmammen}}). If
#' \code{wild = FALSE}, this is ignored.
#'
#' @return matrix with the same number of rows as coefficients in \code{fit} and
#' \code{reps} columns. Each row contains the bootstrap estimates of the
#' corresponding parameters.
#'
#' @examples
#' \dontrun{
#' test.df <- data.frame(x = seq(10), y = seq(10) + rnorm(10))
#' fit <- lm(y ~ x, data = test.df)
#' boot_residual_ci(fit, reps = 4999, wild = FALSE)
#' }
#'
#' @import stats
boot_residual_ci <- function(fit,
                             reps,
                             wild,
                             construct){

  .resid <- fit$residuals
  .n <- length(.resid)

  # obtain (X'X)^(-1) X'
  .H <- qr.coef(fit$qr, diag(.n))

  if(wild){
    # obtain new residuals through multiplication by random variable
    .bootr <- matrix(rmammen(.n*reps, construct = construct),
                     nrow = .n, ncol = reps) * .resid
  } else {
    # obtain new residuals through resampling of original residuals
    .bootr <- matrix(sample(.resid, size = .n*reps, replace = TRUE),
                     nrow = .n, ncol = reps)
  }

  # compute estimates
  .ests <- fit$coefficients + (.H %*% .bootr)

  # compute standard errors
  .sse <-
    matrix(colSums(.bootr * .bootr) -
             colSums(.bootr * (qr.Q(fit$qr) %*% t(qr.Q(fit$qr)) %*% .bootr)),
           nrow = nrow(.ests), ncol = ncol(.ests), byrow = TRUE)

  .stderrs <- sqrt(diag(chol2inv(qr.R(fit$qr))) * (.sse/fit$df.residual))

  attr(.ests, "std.err") <- .stderrs
  attr(.ests, "original.estimates") <- fit$coefficients
  attr(.ests, "original.std.err") <- sqrt(diag(vcov(fit)))

  .ests
}