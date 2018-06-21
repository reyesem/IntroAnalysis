#' Perform (optionally wild) residual bootstrap for linear model.
#'
#' This is the workhorse for the \code{estimate_parameters()} function when a
#' linear model is specified. Given a linear model fit, performs a residual
#' bootstrap (or optionally wild bootstrap) to estimate the standard error of
#' and compute confidence intervals for each of the parameters in the model.
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
#' @import stats
bootstrap_residual <- function(fit,
                               reps,
                               wild,
                               construct){

  .resid <- fit$residuals
  .n <- length(.resid)

  # obtain (X'X)^(-1) X'
  .w <- fit$weights
  if (is.null(.w)) .w <- rep(1, .n)

  .H <- qr.coef(fit$qr, sqrt(.w) * diag(.n))

  if (wild){
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
    matrix(colSums(.w * .bootr * .bootr) -
             colSums((sqrt(.w) * .bootr) * (qr.Q(fit$qr) %*% t(qr.Q(fit$qr)) %*%
                                              (sqrt(.w) * .bootr))),
           nrow = nrow(.ests), ncol = ncol(.ests), byrow = TRUE)


  .sse <- my_round(.sse)
  .stderrs <- sqrt(diag(chol2inv(qr.R(fit$qr))) * (.sse/fit$df.residual))

  attr(.ests, "std.err") <- .stderrs
  attr(.ests, "original.estimates") <- fit$coefficients
  attr(.ests, "original.std.err") <- sqrt(diag(vcov(fit)))

  .ests
}



#' Perform parametric bootstrap for linear models without constant variance.
#'
#' This is a workhorse function which performs a parametric bootstrap without
#' assuming constant variance. This is not meant to be called by the user
#' directly.
#'
#' @param fit \code{lm} object defining the model for which the parameters are
#' estimated.
#' @param reps scalar; number of bootstrap replications to perform.
#'
#' @return matrix with the same number of rows as coefficients in \code{fit} and
#' \code{reps} columns. Each row contains the bootstrap estimates of the
#' corresponding parameters.
#'
#' @import stats
bootstrap_parametric_linear <- function(fit,
                                        reps){

  # obtain parametric bootstraps
  .resid <- fit$residuals
  .n <- length(.resid)
  .yhat <- fit$fitted

  .booty <- matrix(rnorm(reps * .n, mean = .yhat, sd = abs(.resid)),
                   nrow = .n, ncol = reps)


  # obtain (X'X)^(-1) X'
  .w <- fit$weights
  if (is.null(.w)) .w <- rep(1, .n)

  .H <- qr.coef(fit$qr, sqrt(.w) * diag(.n))

  # compute estimates
  .ests <- .H %*% .booty

  # compute standard errors
  .sse <-
    matrix(colSums(.w * .booty * .booty) -
             colSums((sqrt(.w) * .booty) * (qr.Q(fit$qr) %*% t(qr.Q(fit$qr)) %*%
                                              (sqrt(.w) * .booty))),
           nrow = nrow(.ests), ncol = ncol(.ests), byrow = TRUE)

  .sse <- my_round(.sse)
  .stderrs <- sqrt(diag(chol2inv(qr.R(fit$qr))) * (.sse/fit$df.residual))

  attr(.ests, "std.err") <- .stderrs
  attr(.ests, "original.estimates") <- fit$coefficients
  attr(.ests, "original.std.err") <- sqrt(diag(vcov(fit)))

  .ests
}