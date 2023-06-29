#' @describeIn compare_models Computes p-value comparing nested linear models.
#'
#' @param assume.identically.distributed boolean; if \code{TRUE},
#' homoskedasticity is assumed for the error term. If \code{FALSE} (default),
#' this is not assumed.
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
                              alternative = c('ne', 'not equal', '!=',
                                              'lt', 'less than', '<',
                                              'gt', 'greater than', '>',
                                              'at least one differs'),
                              simulation.replications = 4999,
                              assume.identically.distributed = FALSE,
                              assume.constant.variance =
                                assume.identically.distributed,
                              assume.normality = FALSE,
                              construct = c("normal-2",
                                            "normal-1",
                                            "two-point mass"),
                              ...){

  if (!missing(assume.identically.distributed) &&
      !missing(assume.constant.variance) &&
      assume.identically.distributed != assume.constant.variance){
    stop(paste0("specify 'assume.identically.distributed' ",
                "or 'assume.constant.variance' but not both."))
  }

  if (any(class(full.mean.model) != class(reduced.mean.model))){
    stop("Both the full and reduced models must be linear models.")
  }

  if (full.mean.model$df.residual >= reduced.mean.model$df.residual){
    stop(paste0("The reduced model has as many parameters as the full model. ",
                "The full and reduced models were probably switched."))
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

  alternative <- match.arg(alternative)
  alternative <- switch(alternative,
                        `not equal` = 'ne',
                        `!=` = 'ne',
                        `less than` = 'lt',
                        `<` = 'lt',
                        `greater than` = 'gt',
                        `>` = 'gt',
                        `at least one differs` = 'ne',
                        alternative
  )

  if (.anova$df[1] > 1 && alternative != 'ne') {
    alternative <- 'ne'

    warning('One-sided hypotheses can only be performed with single ',
            'parameter tests. Converted to two-sided test.')
  }

  if (alternative != 'ne') {
    .sign <- determine_sign(full.mean.model, reduced.mean.model)

    if ((.sign == 1 & alternative == 'gt') |
        (.sign == -1 & alternative == 'lt')) {
      .anova$p.value[1] <- .anova$p.value[1] * 0.5
    } else if ((.sign == 1 & alternative == 'lt') |
               (.sign == -1 & alternative == 'gt')) {
      .anova$p.value[1] <- 1 - (.anova$p.value[1] * 0.5)
    }
  }

  colnames(.anova)[5] <- 'standardized.statistic'

  .anova
}



#' @describeIn compare_models Computes p-value comparing nested generalized
#' linear models.
#'
#' @param method string defining the methodology to employ. If
#' \code{"classical"} (default), the model is assumed correct and classical
#' large-sample theory is used. If \code{"parametric"}, a parametric bootstrap
#' is performed.
#'
#' @import stats
#' @export
compare_models.glm <- function(full.mean.model,
                               reduced.mean.model,
                               alternative = c('ne', 'not equal', '!=',
                                               'lt', 'less than', '<',
                                               'gt', 'greater than', '>',
                                               'at least one differs'),
                               simulation.replications = 4999,
                               method = c("classical",
                                          "parametric"),
                               ...){

  if (any(class(full.mean.model) != class(reduced.mean.model))){
    stop("Both the full and reduced models must be linear models.")
  }

  if (full.mean.model$df.residual >= reduced.mean.model$df.residual){
    stop(paste0("The reduced model has as many parameters as the full model. ",
                "The full and reduced models were probably switched."))
  }

  method = match.arg(method)

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

  alternative <- match.arg(alternative)
  alternative <- switch(alternative,
                        `not equal` = 'ne',
                        `!=` = 'ne',
                        `less than` = 'lt',
                        `<` = 'lt',
                        `greater than` = 'gt',
                        `>` = 'gt',
                        `at least one differs` = 'ne',
                        alternative
  )

  if (.anova$df[1] > 1 && alternative != 'ne') {
    alternative <- 'ne'

    warning('One-sided hypotheses can only be performed with single ',
            'parameter tests. Converted to two-sided test.')
  }

  if (alternative != 'ne') {
    .sign <- determine_sign(full.mean.model, reduced.mean.model)

    if ((.sign == 1 & alternative == 'gt') |
        (.sign == -1 & alternative == 'lt')) {
      .anova$p.value[1] <- .anova$p.value[1] * 0.5
    } else if ((.sign == 1 & alternative == 'lt') |
               (.sign == -1 & alternative == 'gt')) {
      .anova$p.value[1] <- 1 - (.anova$p.value[1] * 0.5)
    }
  }

  colnames(.anova)[4] <- 'standardized.statistic'
  .anova
}

