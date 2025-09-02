#' Compare nested generalized linear models via parametric bootstrapping.
#'
#' This is the workhorse for the [compare_models()] function when a
#' generalized linear model is specified and parametric bootstrap is required.
#' This is not meant to be called by the user directly.
#'
#' @param fit1 \code{glm} object defining the full model.
#' @param fit0 \code{glm} object defining the reduced model.
#' @param reps scalar; number of bootstrap replications to perform.
#'
#' @return vector of length \code{reps} containing the test statistic from each
#' bootstrap replication. It also has an attribute containing an ANOVA table
#' comparing the two models.
#'
#' @examples
#' \dontrun{
#' fit1 <- glm(mpg ~ hp, data = mtcars)
#' fit0 <- glm(mpg ~ 1, data = mtcars)
#'
#' bootstrap_parametric_null(fit, fit0, reps = 4999)
#' }
#'
#' @import stats
bootstrap_parametric_null <- function(fit1, fit0, reps){

  # obtain parametric bootstraps
  .boot <- simulate(fit0, reps)

  # determine form of models
  .knownvar <- is.element(fit1$family$family, c("binomial", "poisson"))

  # construct original data
  .origdat <- get_all_vars(fit1$formula, data = fit1$data)

  if (!is.null(.na.action <- fit1$na.action)){
    .origdata <- do.call(paste0("na.", class(.na.action)),
                         list(object = .origdata))
  }

  # refit model for each fit
  .refit <- function(newy, fit1, fit0, knownvar, origdat){
    origdat[, 1] <- newy

    .newfit1 <- my_update(fit1, paste(colnames(origdat)[1], " ~ ."),
                          data = origdat)
    .newfit0 <- my_update(fit0, paste(colnames(origdat)[1], " ~ ."),
                          data = origdat)

    .anova <- my_anova(.newfit0, .newfit1,
                       test = ifelse(.knownvar, "Chisq", "F"))

    return(.anova$statistic[1])
  }

  .tstat <- apply(.boot, 2, .refit,
                  fit1 = fit1, fit0 = fit0, knownvar = .knownvar,
                  origdat = .origdat)


  # cleaned up ANOVA table comparing the models
  attr(.tstat, "ANOVA Table") <-
    my_anova(fit0, fit1, test = ifelse(.knownvar, "Chisq", "F"))

  .tstat
}

