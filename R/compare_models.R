#' @describeIn compare_models Computes p-value comparing nested linear models.
#'
#' @inheritParams compare_models
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
#'
#' @import stats
#' @export
compare_models.lm <- function(full.mean.model,
                              reduced.mean.model,
                              simulation.replications = 4999,
                              assume.identically.distributed = TRUE,
                              assume.constant.variance =
                                assume.identically.distributed,
                              assume.normality = FALSE,
                              construct = c("normal-2",
                                            "normal-1",
                                            "two-point mass")){

  if (!missing(assume.identically.distributed) &&
      !missing(assume.constant.variance) &&
      assume.identically.distributed != assume.constant.variance){
    stop(paste0("specify 'assume.identically.distributed' ",
                "or 'assume.constant.variance' but not both."))
  }

  if (any(class(full.mean.model) != class(reduced.mean.model))){
    stop("Both the full and reduced models must be linear models.")
  }

  if (assume.normality && assume.constant.variance){
    .anova <- my_anova(reduced.mean.model, full.mean.model)

    .boot <- rf(simulation.replications, df1 = .anova$df[1], df2 = .anova$df[2])
  } else if (assume.normality && !assume.constant.variance){
    .boot <- bootstrap_parametric_linear_null(full.mean.model,
                                              reduced.mean.model,
                                              reps = simulation.replications)

    .anova <- bootstrap_compute_p(.boot)

  } else {
    .boot <- bootstrap_residual_null(full.mean.model,
                                     reduced.mean.model,
                                     reps = simulation.replications,
                                     wild = !assume.constant.variance,
                                     construct = construct)

    .anova <- bootstrap_compute_p(.boot)
  }

  attr(.anova, "Null Distribution") <- as.numeric(.boot)
  .anova
}



#' @describeIn compare_models Computes p-value comparing nested generalized
#' linear models.
#'
#' @inheritParams compare_models
#' @param method string defining the methodology to employ. If
#' \code{"classical"} (default), the model is assumed correct and classical
#' large-sample theory is used. If \code{"parametric"}, a parametric bootstrap
#' is performed.
#'
#' @import stats
#' @export
compare_models.glm <- function(full.mean.model,
                               reduced.mean.model,
                               simulation.replications = 4999,
                               method = c("classical",
                                          "parametric")){

  if (any(class(full.mean.model) != class(reduced.mean.model))){
    stop("Both the full and reduced models must be linear models.")
  }

  if (method == "classical"){
    .knownvar <- is.element(full.mean.model$family$family,
                            c("binomial", "poisson"))

    .anova <- my_anova(reduced.mean.model, full.mean.model,
                       test = ifelse(.knownvar, "Chisq", "F"))

    if (.knownvar){
      .boot <- rchisq(simulation.replications, df = .anova$df[1])
    } else {
      .boot <- rf(simulation.replications,
                  df1 = .anova$df[1],
                  df2 = .anova$df[2])
    }
  } else if (method == "parametric"){
    .boot <- bootstrap_parametric_null(full.mean.model,
                                       reduced.mean.model,
                                       reps = simulation.replications)

    .anova <- bootstrap_compute_p(.boot)
  }

  attr(.anova, "Null Distribution") <- as.numeric(.boot)
  .anova
}

