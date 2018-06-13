#' Perform parametric bootstrap for generalized linear model.
#'
#' This is the workhorse for the \code{estimate_parameters()} function when a
#' generalized linear model is specified and parametric bootstrap is required.
#' This is not meant to be called by the user directly.
#'
#' @param fit \code{glm} object defining the model for which the parameters are
#' estimated.
#' @param reps scalar; number of bootstrap replications to perform.
#'
#' @return matrix with the same number of rows as coefficients in \code{fit} and
#' \code{reps} columns. Each row contains the bootstrap estimates of the
#' corresponding parameters.
#'
#' @examples
#' \dontrun{
#' fit <- glm(mpg ~ hp, data = mtcars)
#' bootstrap_parametric(fit, reps = 4999)
#' }
#'
#' @import stats
bootstrap_parametric <- function(fit, reps){

  # obtain parametric bootstraps
  .boot <- simulate(fit, reps)

  # construct original data
  .origdat <- get_all_vars(fit$formula, data = fit$data)

  if (!is.null(.na.action <- fit$na.action)){
    .origdata <- do.call(paste0("na.", class(.na.action)),
                         list(object = .origdata))
  }

  # refit model for each replicate
  .refit <- function(newy, fit, origdat){
    origdat[, 1] <- newy

    .newfit <- my_update(fit, paste(colnames(origdat)[1], " ~ ."),
                         data = origdat)

    c(coef(.newfit), sqrt(diag(vcov(.newfit))))
  }

  .all <- apply(.boot, 2, .refit, fit = fit, origdat = .origdat)

  .ests <- .all[1:length(coef(fit)), , drop = FALSE]

  attr(.ests, "std.err") <- .all[-c(1:length(coef(fit))), , drop = FALSE]
  attr(.ests, "original.estimates") <- fit$coefficients
  attr(.ests, "original.std.err") <- sqrt(diag(vcov(fit)))

  .ests
}



#' Perform nonparametric bootstrap for generalized linear model.
#'
#' This is the workhorse for the \code{estimate_parameters()} function when a
#' generalized linear model is specified and case resampling bootstrap is
#' required.  This is not meant to be called by the user directly.
#'
#' @param fit \code{glm} object defining the model for which the parameters are
#' estimated.
#' @param reps scalar; number of bootstrap replications to perform.
#'
#' @return matrix with the same number of rows as coefficients in \code{fit} and
#' \code{reps} columns. Each row contains the bootstrap estimates of the
#' corresponding parameters.
#'
#' @examples
#' \dontrun{
#' fit <- glm(mpg ~ hp, data = mtcars)
#' bootstrap_case(fit, reps = 4999)
#' }
#'
#' @import stats
bootstrap_case <- function(fit, reps){

  # construct original data
  .origdat <- get_all_vars(fit$formula, data = fit$data)

  if (!is.null(.na.action <- fit$na.action)){
    .origdata <- do.call(paste0("na.", class(.na.action)),
                         list(object = .origdata))
  }

  # obtain nonparametric bootstraps
  .n <- nrow(.origdat)
  .boot <- matrix(sample(seq(.n), size = .n*reps, replace = TRUE),
                  nrow = .n, ncol = reps)

  # refit model for each fit
  .refit <- function(obs, fit, origdat){
    origdat <- origdat[obs, , drop = FALSE]

    .newfit <- my_update(fit, data = origdat)

    c(coef(.newfit), sqrt(diag(vcov(.newfit))))
  }

  # obtain bootstrap estimates
  .all <- apply(.boot, 2, .refit, fit = fit, origdat = .origdat)

  .ests <- .all[1:length(coef(fit)), , drop = FALSE]

  attr(.ests, "std.err") <- .all[-c(1:length(coef(fit))), , drop = FALSE]
  attr(.ests, "original.estimates") <- fit$coefficients
  attr(.ests, "original.std.err") <- sqrt(diag(vcov(fit)))

  .ests
}



#' Perform parametric bootstrap for predictions of generalized linear model.
#'
#' This is the workhorse for the \code{estimate_mean_response()} function when a
#' generalized linear model is specified and parametric bootstrap is required.
#' This is not meant to be called by the user directly.
#'
#' @param fit \code{glm} object defining the model for which the parameters are
#' estimated.
#' @param newdata new data set used to construct the predictions.
#' @param reps scalar; number of bootstrap replications to perform.
#'
#' @return matrix with the same number of rows as rows of \code{newdat} and
#' \code{reps} columns. Each row contains the bootstrap estimates of the
#' corresponding prediction.
#'
#' @examples
#' \dontrun{
#' fit <- glm(mpg ~ hp, data = mtcars)
#' bootstrap_parametric_predict(fit, mtcars, reps = 4999)
#' }
#'
#' @import stats
bootstrap_parametric_predict <- function(fit, newdata, reps){

  # obtain parametric bootstraps
  .boot <- simulate(fit, reps)

  # construct original data
  .origdat <- get_all_vars(fit$formula, data = fit$data)

  if (!is.null(.na.action <- fit$na.action)){
    .origdata <- do.call(paste0("na.", class(.na.action)),
                         list(object = .origdata))
  }

  # refit model for each fit
  .refit <- function(newy, fit, newdata, origdat){
    origdat[, 1] <- newy

    .newfit <- my_update(fit, paste(colnames(origdat)[1], " ~ ."),
                         data = origdat)

    .yhat <- predict(.newfit, newdata = newdata, type = "response",
                     se.fit = TRUE)

    c(.yhat$fit, .yhat$se.fit)
  }

  .all <- apply(.boot, 2, .refit, fit = fit,
                newdata = newdata, origdat = .origdat)

  .ests <- .all[1:nrow(newdata), , drop = FALSE]
  .orig <- predict(fit, newdata = newdata, type = "response", se.fit = TRUE)

  attr(.ests, "std.err") <- .all[-c(1:nrow(newdata)), , drop = FALSE]
  attr(.ests, "original.estimates") <- .orig$fit
  attr(.ests, "original.std.err") <- .orig$se.fit

  .ests
}



#' Perform nonparametric bootstrap for predictions of generalized linear model.
#'
#' This is the workhorse for the \code{estimate_mean_response()} function when a
#' generalized linear model is specified and case resampling bootstrap is
#' required.  This is not meant to be called by the user directly.
#'
#' @param fit \code{glm} object defining the model for which the parameters are
#' estimated.
#' @param newdata new data set used to construct the predictions.
#' @param reps scalar; number of bootstrap replications to perform.
#'
#' @return matrix with the same number of rows as rows of \code{newdat} and
#' \code{reps} columns. Each row contains the bootstrap estimates of the
#' corresponding prediction.
#'
#' @examples
#' \dontrun{
#' fit <- glm(mpg ~ hp, data = mtcars)
#' bootstrap_case_predict(fit, mtcars, reps = 4999)
#' }
#'
#' @import stats
bootstrap_case_predict <- function(fit, newdata, reps){

  # construct original data
  .origdat <- get_all_vars(fit$formula, data = fit$data)

  if (!is.null(.na.action <- fit$na.action)){
    .origdata <- do.call(paste0("na.", class(.na.action)),
                         list(object = .origdata))
  }

  # obtain nonparametric bootstraps
  .n <- nrow(.origdat)
  .boot <- matrix(sample(seq(.n), size = .n*reps, replace = TRUE),
                  nrow = .n, ncol = reps)

  # refit model for each fit
  .refit <- function(obs, fit, newdata, origdat){
    origdat <- origdat[obs, , drop = FALSE]

    .newfit <- my_update(fit, data = origdat)

    .yhat <- predict(.newfit, newdata = newdata, type = "response",
                     se.fit = TRUE)

    c(.yhat$fit, .yhat$se.fit)
  }

  # obtain bootstrap estimates
  .all <- apply(.boot, 2, .refit, fit = fit,
                newdata = newdata, origdat = .origdat)

  .ests <- .all[1:nrow(newdata), , drop = FALSE]
  .orig <- predict(fit, newdata = newdata, type = "response", se.fit = TRUE)

  attr(.ests, "std.err") <- .all[-c(1:nrow(newdata)), , drop = FALSE]
  attr(.ests, "original.estimates") <- .orig$fit
  attr(.ests, "original.std.err") <- .orig$se.fit

  .ests
}