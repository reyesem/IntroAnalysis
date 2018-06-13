#' @describeIn estimate_parameters Estimates for linear models.
#'
#' @inheritParams estimate_parameters
#' @param assume.identically.distributed boolean; if \code{TRUE} (default),
#' homoskedasticity is assumed for the error term. If \code{FALSE}, this is not
#' assumed.
#' @param assume.constant.variance another way of specifying
#' \code{assume.identically.distributed}. Both should not be specified.
#' @param assume.normality boolean; if \code{TRUE}, the errors are assumed to
#' follow a Normal distribution. If \code{FALSE} (default), this is not
#' assumed.
#' @param construct string defining the type of construct to use when generating
#' from the distribution for the wild bootrap (see \code{\link{rmammen}}). If
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
                                   assume.identically.distributed = TRUE,
                                   assume.constant.variance =
                                     assume.identically.distributed,
                                   assume.normality = FALSE,
                                   construct = c("normal-2",
                                                 "normal-1",
                                                 "two-point mass"),
                                   type = c("percentile",
                                            "BC",
                                            "bootstrap-t")){

  construct <- match.arg(construct)
  type <- match.arg(type)

  if (!missing(assume.identically.distributed) &&
      !missing(assume.constant.variance) &&
      assume.identically.distributed != assume.constant.variance){
    stop(paste0("specify 'assume.identically.distributed' ",
                "or 'assume.constant.variance' but not both."))
  }

  # construct basic frame for output
  .ests <- summary(mean.model)$coefficients
  .ests <- data.frame(
    term = rownames(.ests),
    estimate = .ests[, "Estimate"],
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
    .boot <- .boot*.ests$standard.error + .ests$estimate
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
#' @inheritParams estimate_parameters
#' @param method string defining the methodology to employ. If
#' \code{"classical"} (default), the model is assumed correct and classical
#' large-sample theory is used. If \code{"parametric"}, a parametric bootstrap
#' is performed. If \code{"case-resampling"}, a case-resampling bootstrap is
#' performed.
#' @param type string defining the type of confidence interval to construct. If
#' \code{"percentile"} (default) an equal-tailed percentile interval is
#' constructed. If \code{"BC"} the bias-corrected percentile interval is
#' constructed. If \code{"bootstrap-t"} the bootstrap-t interval is constructed.
#'
#' @import stats
#' @import MASS
#' @export
estimate_parameters.glm <- function(mean.model,
                                    confidence.level,
                                    simulation.replications = 4999,
                                    method = c("classical",
                                               "parametric",
                                               "case-resampling"),
                                    type = c("percentile",
                                             "BC",
                                             "bootstrap-t")){

  method <- match.arg(method)
  type <- match.arg(type)

  # construct basic frame for output
  .ests <- summary(mean.model)$coefficients
  .ests <- data.frame(
    term = rownames(.ests),
    estimate = .ests[, "Estimate"],
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
    .ci <- confint(profile(mean.model), level = confidence.level)

    if (is.null(dim(.ci))) .ci <- t(.ci)

    .ests <- cbind(
      .ests,
      data.frame(.ci)
    )

    .boot <- matrix(rnorm(simulation.replications),
                    nrow = nrow(.ests),
                    ncol = simulation.replications,
                    byrow = TRUE)
    .boot <- .boot*.ests$standard.error + .ests$estimate
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

