#' @describeIn estimate_parameters Estimates for linear models.
#'
#' @param assume.constant.variance if \code{TRUE} (default), assume the errors
#' have the same variability for every observation. If \code{FALSE}, the
#' variability in the errors is allowed to differ across observations.
#' @param assume.normality boolean; if \code{TRUE}, the errors are assumed to
#' follow a Normal distribution. If \code{FALSE} (default), this is not
#' assumed.
#' @param construct string defining the type of construct to use when generating
#' from the distribution for the wild bootstrap (see \code{\link{rmammen}}). If
#' \code{assume.constant.variance = TRUE}, this is ignored
#' (default = \code{"normal-2"}).
#' @param type string defining the type of confidence interval to construct. If
#' \code{"percentile"} (default) an equal-tailed percentile interval is
#' constructed. If \code{"BC"} the bias-corrected percentile interval is
#' constructed. If \code{"bootstrap-t"} the bootstrap-t interval is constructed.
#'
#' @import stats
#' @export
estimate_parameters.lm <- function(mean.model,
                                   confidence.level,
                                   simulation.replications = 4999,
                                   assume.constant.variance = TRUE,
                                   assume.normality = FALSE,
                                   construct = c("normal-2",
                                                 "normal-1",
                                                 "two-point mass"),
                                   type = c("percentile",
                                            "BC",
                                            "bootstrap-t"),
                                   ...){

  construct <- match.arg(construct)
  type <- match.arg(type)

  # construct basic frame for output
  .ests <- summary(mean.model)$coefficients
  .ests <- data.frame(
    term = rownames(.ests),
    point.estimate = .ests[, "Estimate"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )


  # if no confidence level specified, only return point estimate
  if (missing(confidence.level)){
    return(.ests)
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
    .ests$standard.error <- summary(mean.model)$coefficients[, "Std. Error"]
    .ci <- confint(mean.model, level = confidence.level)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )

    .boot <- matrix(rt(simulation.replications, df = mean.model$df.residual),
                    nrow = nrow(.ests),
                    ncol = simulation.replications,
                    byrow = TRUE)
    .boot <- .boot*.ests$standard.error + .ests$point.estimate
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

    .ests$standard.error <- apply(.boot, 1, sd)
    .ci <- bootstrap_compute_ci(.boot,
                                level = confidence.level,
                                type = type)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )
  }

  colnames(.ests)[c(4, 5)] <- c(.lowername, .uppername)
  rownames(.ests) <- NULL

  attributes(.boot) <- list(dim = c(nrow(.ests), simulation.replications),
                            dimnames = list(.ests$term,
                                            NULL))
  attr(.ests, "Sampling Distribution") <- t(.boot)
  .ests
}



#' @describeIn estimate_parameters Estimates for generalized linear models.
#'
#' @param method string defining the methodology to employ. If
#' \code{"classical"} (default), the model is assumed correct and classical
#' large-sample theory is used. If \code{"parametric"}, a parametric bootstrap
#' is performed. If \code{"case-resampling"}, a case-resampling bootstrap is
#' performed.
#'
#' @import stats
#' @export
estimate_parameters.glm <- function(mean.model,
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

  # construct basic frame for output
  .ests <- summary(mean.model)$coefficients
  .ests <- data.frame(
    term = rownames(.ests),
    point.estimate = .ests[, "Estimate"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )


  # if no confidence level specified, only return point estimate
  if (missing(confidence.level)){
    return(.ests)
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
    .ests$standard.error <- summary(mean.model)$coefficients[, "Std. Error"]
    .ci <- stats:::profile.glm(mean.model) |>
      confint(level = confidence.level)

    if (is.null(dim(.ci))) .ci <- t(.ci)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )

    .boot <- matrix(rnorm(simulation.replications),
                    nrow = nrow(.ests),
                    ncol = simulation.replications,
                    byrow = TRUE)
    .boot <- .boot*.ests$standard.error + .ests$point.estimate
  } else if (method == "parametric"){
    # use a parametric bootstrap
    .boot <- bootstrap_parametric(mean.model,
                                  reps = simulation.replications)

    .ests$standard.error <- apply(.boot, 1, sd)
    .ci <- bootstrap_compute_ci(.boot,
                                level = confidence.level,
                                type = type)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )
  } else {
    # use a case-based resampling for estimates
    .boot <- bootstrap_case(mean.model,
                            reps = simulation.replications)

    .ests$standard.error <- apply(.boot, 1, sd)
    .ci <- bootstrap_compute_ci(.boot,
                                level = confidence.level,
                                type = type)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )
  }

  colnames(.ests)[c(4, 5)] <- c(.lowername, .uppername)
  rownames(.ests) <- NULL

  attributes(.boot) <- list(dim = c(nrow(.ests), simulation.replications),
                            dimnames = list(.ests$term,
                                            NULL))
  attr(.ests, "Sampling Distribution") <- t(.boot)
  .ests
}

