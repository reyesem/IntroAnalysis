#' Compare nested linear models via (optionally wild) residual bootstrap.
#'
#' This is the workhorse for the \code{compare_models()} function when a linear
#' model is specified. Given two linear models, one nested within the other, the
#' null distribution of the test statistic comparing the two models is estimated
#' via a residual bootstrap (or wild bootstrap). This is not meant to be called
#' by the user directly.
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
#' bootstrap replication. It also has an attribute containing an ANOVA table
#' comparing the two models.
#'
#' @examples
#' \dontrun{
#' fit1 <- lm(mpg ~ hp, data = mtcars)
#' fit0 <- lm(mpg ~ 1, data =  mtcars)
#'
#' boot_residual_null(fit1, fit0, reps = 4999, wild = FALSE)
#' }
#'
#' @import stats
bootstrap_residual_null <- function(fit1,
                                    fit0,
                                    reps,
                                    wild,
                                    construct){

  .resid0 <- fit0$residuals
  .resid1 <- fit1$residuals
  .yhat0 <- fit0$fitted.values
  .n <- length(.resid0)

  if (length(.resid0) != length(.resid1)){
    stop(paste("Sample size must be equivalent for full and reduced models.",
               "Try removing missing values prior to fitting models."))
  }

  if (wild){
    # obtain new residuals through multiplication by random variable
    .booty <- (matrix(rmammen(.n*reps, construct = construct),
                      nrow = .n, ncol = reps) * .resid1) + .yhat0
  } else {
    # obtain new residuals through resampling of original residuals
    .booty <- matrix(sample(.resid1, size = .n*reps, replace = TRUE),
                     nrow = .n, ncol = reps) + .yhat0
  }

  .w <- fit1$weights
  if (is.null(.w)) .w <- rep(1, .n)

  # obtain proj matrix (accounting for no param models) and compute test stat
  .P1 <- (qr.Q(fit1$qr)) %*% t(qr.Q(fit1$qr))

  if (!is.null(fit0$qr)){
    .P0 <- (qr.Q(fit0$qr)) %*% t(qr.Q(fit0$qr))
    .num <- colSums((sqrt(.w)*.booty) * ((.P1 - .P0) %*% (sqrt(.w) * .booty)))
  } else {
    .num <- colSums(.w*(.booty - .yhat0)^2) -
      colSums((sqrt(.w)*.booty) * ((diag(.n) - .P1) %*% (sqrt(.w)*.booty)))
  }

  .den <- colSums((sqrt(.w)*.booty) * ((diag(.n) - .P1) %*% (sqrt(.w)*.booty)))

  .tstat <- (.num/(fit0$df.residual - fit1$df.residual))/(.den/fit1$df.residual)


  # cleaned up ANOVA table comparing the models
  attr(.tstat, "ANOVA Table") <- my_anova(fit0, fit1)

  .tstat
}



#' Compare nested linear models via parametric bootstrap.
#'
#' This is the workhorse for the \code{compare_models()} function when a linear
#' model is specified without constant variance. Given two linear models, one
#' nested within the other, the null distribution of the test statistic
#' comparing the two models is estimated via a parametric bootstrap.
#' This is not meant to be called by the user directly.
#'
#' @param fit1 \code{lm} object defining the full model.
#' @param fit0 \code{lm} object defining the reduced model.
#' @param reps scalar; number of bootstrap replications to perform.
#'
#' @return matrix with the same number of rows as coefficients in \code{fit} and
#' \code{reps} columns. Each row contains the bootstrap estimates of the
#' corresponding parameters.
#'
#' @examples
#' \dontrun{
#' fit1 <- lm(mpg ~ hp, data = mtcars)
#' fit0 <- lm(mpg ~ 1, data =  mtcars)
#'
#' boot_parametric_linear_null(fit1, fit0, reps = 4999)
#' }
#'
#' @import stats
bootstrap_parametric_linear_null <- function(fit1,
                                             fit0,
                                             reps){

  # obtain parametric bootstraps
  .resid0 <- fit0$residuals
  .resid1 <- fit1$residuals
  .yhat0 <- fit0$fitted.values
  .n <- length(.resid0)

  if (length(.resid0) != length(.resid1)){
    stop(paste("Sample size must be equivalent for full and reduced models.",
               "Try removing missing values prior to fitting models."))
  }

  .booty <- matrix(rnorm(reps * .n,
                         mean = rep(.yhat0, reps),
                         sd = rep(abs(.resid1), reps)),
                   nrow = .n, ncol = reps)


  .w <- fit1$weights
  if (is.null(.w)) .w <- rep(1, .n)

  # obtain proj matrix (accounting for no param models) and compute test stat
  .P1 <- (qr.Q(fit1$qr)) %*% t(qr.Q(fit1$qr))

  if (!is.null(fit0$qr)){
    .P0 <- (qr.Q(fit0$qr)) %*% t(qr.Q(fit0$qr))
    .num <- colSums((sqrt(.w)*.booty) * ((.P1 - .P0) %*% (sqrt(.w) * .booty)))
  } else {
    .num <- colSums(.w*(.booty - .yhat0)^2) -
      colSums((sqrt(.w)*.booty) * ((diag(.n) - .P1) %*% (sqrt(.w)*.booty)))
  }

  .den <- colSums((sqrt(.w)*.booty) * ((diag(.n) - .P1) %*% (sqrt(.w)*.booty)))

  .tstat <- (.num/(fit0$df.residual - fit1$df.residual))/(.den/fit1$df.residual)


  # cleaned up ANOVA table comparing the models
  attr(.tstat, "ANOVA Table") <- my_anova(fit0, fit1)

  .tstat
}

