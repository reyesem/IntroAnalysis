#' @describeIn estimate_mean_response Estimates mean response for linear models.
#'
#' @param assume.constant.variance boolean; if \code{TRUE} (default), errors are
#' assumed to have the same variance.  If \code{FALSE}, each error term is
#' allowed to have a different variance.
#' @param assume.normality boolean; if \code{TRUE}, the errors are assumed to
#' follow a Normal distribution. If \code{FALSE} (default), this is not
#' assumed.
#' @param construct string defining the type of construct to use when generating
#' from the distribution for the wild bootstrap (see [rmammen()]). If
#' \code{assume.constant.variance = TRUE}, this is ignored
#' (default = \code{"normal-2"}).
#' @param type string defining the type of confidence interval to construct. If
#' \code{"percentile"} (default) an equal-tailed percentile interval is
#' constructed. If \code{"BC"} the bias-corrected percentile interval is
#' constructed. Currently, the bootstrap-t interval is not supported.
#'
#' @import stats
#' @export
estimate_mean_response.lm <- function(mean.model,
                                      confidence.level,
                                      simulation.replications = 4999,
                                      assume.constant.variance = TRUE,
                                      assume.normality = FALSE,
                                      construct = c("normal-2",
                                                    "normal-1",
                                                    "two-point mass"),
                                      type = c("percentile",
                                               "BC"),
                                      ...){

  construct <- match.arg(construct)
  type <- match.arg(type)

  # construct new data set containing predictions
  .newdat <- tidyr::expand_grid(...)
  if (nrow(.newdat) == 0) .newdat <- model.frame(mean.model)

  # if no confidence level specified, only return point estimate
  if (missing(confidence.level)){
    .newdat$point.estimate <- predict(mean.model,
                                 newdata = .newdat,
                                 type = "response",
                                 se.fit = FALSE)

    return(.newdat)
  }


  # adjust confidence level if specified strangely
  confidence.level <- confidence.level[1]
  if (confidence.level <= 0){
    stop("Confidence level must be positive.")
  } else if (confidence.level >= 100){
    stop("Confidence level must be below 100%.")
  } else if (confidence.level >= 1){
    confidence.level <- confidence.level/100
  }

  .lowername <- paste(100*confidence.level, "% lower", sep = "")
  .uppername <- paste(100*confidence.level, "% upper", sep = "")


  # classical theory
  if (assume.normality && assume.constant.variance){
    .yhat <- predict(mean.model, newdata = .newdat, se.fit = TRUE,
                     interval = "confidence",
                     level = confidence.level,
                     type = "response")

    .newdat$point.estimate <- .yhat$fit[, "fit"]
    .newdat$standard.error <- .yhat$se.fit
    .newdat$.lwr <- .yhat$fit[, "lwr"]
    .newdat$.upr <- .yhat$fit[, "upr"]

    .boot <- matrix(rt(simulation.replications, df = mean.model$df.residual),
                    nrow = nrow(.newdat),
                    ncol = simulation.replications,
                    byrow = TRUE)

    .boot <- .boot*.newdat$standard.error + .newdat$point.estimate

  } else {
    if (assume.normality && !assume.constant.variance){
      .boot <- bootstrap_parametric_linear(mean.model,
                                           reps = simulation.replications)
    } else {
      .boot <- bootstrap_residual(mean.model,
                                  reps = simulation.replications,
                                  wild = !assume.constant.variance,
                                  construct = construct)
    }

    .boot <- predict_coef(mean.model, .newdat, beta = .boot)

    .newdat$point.estimate <- predict(mean.model,
                                 newdata = .newdat,
                                 se.fit = FALSE,
                                 type = "response")

    attr(.boot, "original.estimates") <- .newdat$point.estimate

    .newdat$standard.error <- apply(.boot, 1, sd)
    .ci <- bootstrap_compute_ci(.boot,
                                level = confidence.level,
                                type = type)

    .newdat <- cbind(
      .newdat,
      data.frame(.ci)
    )
  }

  colnames(.newdat)[ncol(.newdat) - c(1, 0)] <- c(.lowername, .uppername)
  rownames(.newdat) <- NULL

  attributes(.boot) <- list(dim = c(nrow(.newdat), simulation.replications))
  attr(.newdat, "Sampling Distribution") <- t(.boot)
  .newdat
}



#' @describeIn estimate_mean_response Estimates mean response for generalized
#' linear models.
#'
#' @param method string defining the methodology to employ. If
#' \code{"classical"} (default), the model is assumed correct and classical
#' large-sample theory is used. If \code{"parametric"}, a parametric bootstrap
#' is performed. If \code{"case-resampling"}, a case-resampling bootstrap is
#' performed.
#'
#' @import stats
#' @export
estimate_mean_response.glm <- function(mean.model,
                                       confidence.level,
                                       simulation.replications = 4999,
                                       method = c("classical",
                                                  "parametric",
                                                  "case-resampling"),
                                       type = c("percentile",
                                                "BC",
                                                "bootstrap-t"),
                                       ...){

  method <- match.arg(method)
  type <- match.arg(type)

  # construct new data set containing predictions
  .newdat <- tidyr::expand_grid(...)
  if (nrow(.newdat) == 0) .newdat <- model.frame(mean.model)

  # if no confidence level specified, only return point estimate
  if (missing(confidence.level)){
    .newdat$.oint.estimate <- predict(mean.model,
                                 newdata = .newdat,
                                 type = "response",
                                 se.fit = FALSE)

    return(.newdat)
  }


  # adjust confidence level if specified strangely
  confidence.level <- confidence.level[1]
  if (confidence.level <= 0){
    stop("Confidence level must be positive.")
  } else if (confidence.level >= 100){
    stop("Confidence level must be below 100%.")
  } else if (confidence.level >= 1){
    confidence.level <- confidence.level/100
  }

  .lowername <- paste(100*confidence.level, "% lower", sep = "")
  .uppername <- paste(100*confidence.level, "% upper", sep = "")


  # classical theory
  if (method == "classical"){
    .yhat <- predict(mean.model, newdata = .newdat, se.fit = TRUE,
                     type = "response")

    .newdat$point.estimate <- .yhat$fit
    .newdat$standard.error <- .yhat$se.fit
    .newdat$.lwr <- .yhat$fit - qnorm((1 + confidence.level)/2)*.yhat$se.fit
    .newdat$.upr <- .yhat$fit + qnorm((1 + confidence.level)/2)*.yhat$se.fit

    .boot <- matrix(rnorm(simulation.replications),
                    nrow = nrow(.newdat),
                    ncol = simulation.replications,
                    byrow = TRUE)

    .boot <- .boot*.newdat$standard.error + .newdat$point.estimate

  } else if (method == "parametric"){
    .boot <- bootstrap_parametric_predict(mean.model,
                                          newdata = .newdat,
                                          reps = simulation.replications)

    .newdat$standard.error <- apply(.boot, 1, sd)
    .ci <- bootstrap_compute_ci(.boot,
                                level = confidence.level,
                                type = type)

    .newdat <- cbind(
      .newdat,
      data.frame(.ci)
    )

  } else {
    .boot <- bootstrap_case_predict(mean.model,
                                    newdata = .newdat,
                                    reps = simulation.replications)

    .newdat$standard.error <- apply(.boot, 1, sd)
    .ci <- bootstrap_compute_ci(.boot,
                                level = confidence.level,
                                type = type)

    .newdat <- cbind(
      .newdat,
      data.frame(.ci)
    )
  }

  colnames(.newdat)[ncol(.newdat) - c(1, 0)] <- c(.lowername, .uppername)
  rownames(.newdat) <- NULL

  attributes(.boot) <- list(dim = c(nrow(.newdat), simulation.replications))
  attr(.newdat, "Sampling Distribution") <- t(.boot)
  .newdat
}

