#' Specify the mean model for the data generating process.
#'
#' A wrapper for \code{\link[stats]{lm}} and \code{\link[stats]{glm}} which
#' allows the user to specify the form of the model for the mean response of
#' the data generating process.
#'
#' \code{specify_mean_model} provides a single interface to both \code{lm} and
#' \code{glm} with two noticable differences:
#' \enumerate{
#'   \item The intercept is \emph{not} specified by default.
#'   \item The notation "constant" can be used in the formula statement in
#'   order to specify terms which do not require parameters, which are typically
#'   specified by \code{offset()} in base R. The difference is that "constant"
#'   allows scalar terms.}
#'
#' If \code{family} is not specified, then \code{lm} is used to fit the model.
#' Otherwise, \code{glm} is used to fit the model.
#'
#'
#'
#' @param formula an object of class "\code{\link[stats]{formula}}": a symbolic
#' description of the form of the mean response model. Allows some notation
#' not typical in base R.
#' @param data an optional data frame containing the variables appearing in
#' \code{formula}.
#' @param family a description of the error distribution and link function to
#' be used in the model. See \code{\link[stats]{glm}} for details. If missing
#' (default), a linear model is used.
#' @param ... additional arguments to pass to \code{\link[stats]{lm}} or
#' \code{\link[stats]{glm}}.
#'
#' @return an objet of type \code{lm} or \code{glm} depending on whether
#' \code{family} is specified.
#'
#' @examples
#' # specify a linear model
#' fit_reg <- specify_mean_model(mpg ~ 1 + hp, data = mtcars)
#' summary(fit_reg)
#'
#' # intercept is not included by default
#' fit_0_intercept <- specify_mean_model(mpg ~ hp, data = mtcars)
#' summary(fit_0_intercept)
#'
#' # use 'constant()' to prevent parameter from being fit
#' fit_restricted <- specify_mean_model(mpg ~ constant(3) + hp, data = mtcars)
#' summary(fit_restricted)
#'
#' # specify a logistic regression model
#' fit_logistic <- specify_mean_model(am ~ 1 + hp,
#'   family = binomial,
#'   data = mtcars)
#' summary(fit_logistic)
#'
#' @aliases specify_process
#'
#' @import stats
#' @export
specify_mean_model <- function(formula,
                               data,
                               family,
                               ...){

  formula <- as.character(formula)

  # check if intercept is specified (look for 1)
  reduced.formula.3 <- gsub(pattern = "\\s",
                            replacement = "",
                            formula[3])

  reduced.formula.3 <- gsub(pattern = "\\+*((constant)|(offset))\\(.+\\)\\+*",
                            replacement = "",
                            reduced.formula.3)

  reduced.formula.3 <- gsub(pattern = "\\(|\\)",
                            replacement = "",
                            reduced.formula.3)

  formula[3] <- ifelse(
    grepl("(^|\\+)1($|\\+)", reduced.formula.3),
    formula[3],
    paste0(formula[3], "-1")
  )

  # allow use of 'constant' to specify offsets
  formula[3] <- gsub(pattern = "constant\\(",
                     replacement =
                       paste0("offset\\(rep\\(0,length\\(",
                              formula[2], "\\)\\)\\+"),
                     formula[3])

  formula <- as.formula(paste(formula[c(2, 1, 3)], collapse = " "),
                        env = parent.frame(n = 2))


  # ensure call is similar to use of lm or glm with correct environment
  .call <- match.call()
  .call[["formula"]] <- formula

  if(missing(family)){
    .call[[1]] <- quote(lm)
  } else {
    .call[[1]] <- quote(glm)
  }

  eval.parent(.call, n = 2)
}



#' Estimate the parameters of linear or generalized linear models.
#'
#' Provides point estimates and confidence intervals for the parameters of a
#' linear or generalized linear model via bootstrapping or classical theory.
#'
#' This wrapper provides a single interface for estimating parameters under
#' various conditions imposed on the model. Similar to
#' \code{\link[base]{summary}}, point and interval estimates of the parameters
#' are available. However, interval estimates can be constructed via
#' bootstrapping or classical theory.
#'
#' For linear models, the following approaches are implemented:
#' \itemize{
#'   \item classical: if both homoskedasticity and normality are assumed, the
#' sampling distributions of a standardized statistic is modeled by a
#' t-distribution.
#'   \item parametric bootstrap: if normality can be assumed but
#' homoskedasticity cannot, a parametric bootstrap can be peformed in which the
#' variance for each observation is estimated by the square of the corresponding
#' residual (similar to a White's correction).
#'   \item residual bootstrap: if homoskedasticity can be assumed, but normality
#' cannot, a residual bootstrap is used to compute standard errors and
#' confidence intervals.
#'   \item wild bootstrap: if neither homoskedasticity nor normality is assumed,
#' a wild bootstrap is used to compute standard errors and confidence intervals.
#' }
#' All methods make additional requirements regarding independence of the error
#' terms and that the model has been correctly specified. Note: for parametric
#' bootstrap assuming constant variance, use a generalized linear model
#' approach.
#'
#' For generalized linear models, the following approaches are implemented:
#' \itemize{
#'   \item classical: if the distributional family is assumed correct, large
#'   sample theory is used to justify modeling the sampling distribution of a
#'   standardized statistic using a standard normal distribution.
#'   \item parametric bootstrap: the distributional family is assumed and
#'   a parametric bootstrap is performed to compute standard errors and
#'   confidence intervals.
#'   \item nonparametric bootstrap: a case resampling bootstrap algorithm is
#'   used to estimate standard errors and confidence intervals.
#' }
#' All methods require observations to be independent of one another.
#'
#' Confidence intervals constructed via bootstrapping can take on various forms.
#' The percentile interval is constructed by taking the empirical
#' \eqn{100\alpha} and \eqn{100(1-\alpha)} percentiles from the bootstrap
#' statistics. If \eqn{\hat{F}} is the empirical distribution function of the
#' bootstrap values, then the \eqn{100(1 - 2\alpha)}% percentile interval is
#' given by
#' \deqn{(\hat{F}^{-1}(\alpha), \hat{F}^{-1}(1-\alpha))}
#' The bias-corrected (BC) interval corrects for median-bias.  It is given by
#' \deqn{(\hat{F}^{-1}(\alpha_1), \hat{F}^{-1}(1-\alpha_2))}
#' where
#' \deqn{\alpha_1 = \Phi{2\hat{z}_0 + \Phi^{-1}(\alpha)}}
#' \deqn{\alpha_2 = 1 - \Phi{2\hat{z}_0 + \Phi^{-1}(1-\alpha)}}
#' \deqn{\hat{z}_0 = \Phi^{-1}(\hat{F}(\hat{\beta}))}
#' where \eqn{\hat{\beta}} is the estimate from the original sample.
#' The bootstrap-t interval is based on the bootstrap distribution of
#' \deqn{t^{b} = \frac{\hat{\beta}^{b} -
#' \hat{\beta}}{\hat{\sigma}^{b}}}
#' where \eqn{\hat{\sigma}} is the estimate of the standard error of
#' \eqn{\hat{\beta}} and the superscript b denotes a bootstrap sample. Let
#' \eqn{\hat{G}} be the empirical distribution function of the bootstrap
#' standardized statistics given above.  Then, the bootstrap-t interval is given
#' by
#' \deqn{(\hat{\beta} - \hat{\sigma}\hat{G}^{-1}(1-\alpha),
#' \hat{\beta} - \hat{\sigma}\hat{G}^{-1}\alpha)}
#'
#' @param mean.model \code{lm} or \code{glm} model fit defining the model
#' therefore the parameters of the mean model to be estimated.
#' @param confidence.level scalar between 0 and 1 indicating the confidence
#' level for all confidence intervals constructed. If missing (default), only
#' point estimates are returned.
#' @param simulation.replications scalar indicating the number of samples to
#' draw from the model for the sampling distribution (default = 4999). This will
#' either be the number of bootstrap relications or the number of samples from
#' the classical sampling distribution. This is ignored if
#' \code{confidence.level} is not specified.
#' @param ... additional arguments to be passed to other methods.
#'
#' @return data.frame containing a table of parameter estimates. The object
#' has an additional attribute "Sampling Distribution" which is a matrix with
#' \code{simulation.replications} rows and the same number of
#' columns as parameters in \code{mean.model}.  Each column contains a sample
#' from the corresponding model of the sampling distribution. This is useful for
#' plotting the modeled sampling distribution.
#'
#' @examples
#' fit <- lm(mpg ~ 1 + hp, data = mtcars)
#'
#' # confidence intervals for linear model via a residual bootstrap
#' estimate_parameters(fit,
#'   confidence.level = 0.95,
#'   assume.identically.distributed = TRUE,
#'   assume.normality = FALSE)
#'
#' # classical inference
#' estimate_parameters(fit,
#'   confidence.level = 0.95,
#'   assume.identically.distributed = TRUE,
#'   assume.normality = TRUE)
#'
#' @import stats
#' @export
estimate_parameters <- function(mean.model,
                                confidence.level,
                                simulation.replications = 4999,
                                ...){
  UseMethod("estimate_parameters")
}



#' Compute a p-value comparing two nested models.
#'
#' Tests whether a reduced (nested) model is sufficient for explaining the
#' variability in the response compared to a more complex model.
#'
#' This wrapper provides a single interface for commparing models under various
#' conditions imposed on the model. Similar to \code{\link[stats]{anova}}.
#' Howevever, the p-value provided can be computed using classical methods or
#' bootstrapping.
#'
#' For linear models, the following approaches are implemented:
#' \itemize{
#'   \item classical: if both homoskedasticity and normality are assumed, the
#' sampling distributions of a standardized statistic is modeled by an
#' F-distribution.
#'   \item parametric bootstrap: if normality can be assumed but
#' homoskedasticity cannot, a parametric bootstrap can be peformed in which the
#' variance for each observation is estimated by the square of the corresponding
#' residual (similar to a White's correction).
#'   \item residual bootstrap: if homoskedasticity can be assumed, but normality
#' cannot, a residual bootstrap is used to compute the p-value.
#'   \item wild bootstrap: if neither homoskedasticity nor normality is assumed,
#' a wild bootstrap is used to compute the p-value.
#' }
#' All methods make additional requirements regarding independence of the error
#' terms and that the model has been correctly specified.
#'
#' For generalized linear models, the following approaches are implemented:
#' \itemize{
#'   \item classical: if the distributional family is assumed correct, large
#'   sample theory is used to justify modeling the sampling distribution of a
#'   standardized statistic using a chi-squared distribution.
#'   \item parametric bootstrap: the distributional family is assumed and
#'   a parametric bootstrap is performed to compute the p-value.
#' }
#' All methods require observations to be independent of one another.
#'
#' @param full.mean.model \code{lm} or \code{glm} model object defining the
#' full model.
#' @param reduced.mean.model model object of the same type as
#' \code{full.mean.model} defining the reduced model under the null hypothesis.
#' @param simulation.replications scalar indicating the number of samples to
#' draw from the model for the null distribution (default = 4999). This will
#' either be the number of bootstrap relications or the number of samples from
#' the classical null distribution.
#' @param ... additional arguments to be passed to other methods.
#'
#' @return data.frame containing an ANOVA table comparing the two models. The
#' data.frame has a single attribute "Null Distribution" which is a numeric
#' vector of length \code{simulation.replications} which
#' contains a sample from the model of the null distribution of the test
#' statistic. This is useful for plotting the null distribution.
#'
#' @seealso \code{\link[stats]{anova}}
#'
#' @examples
#' fit1 <- lm(mpg ~ 1 + hp, data = mtcars)
#' fit0 <- lm(mpg ~ 1, data = mtcars)
#'
#' # p-value computed via residual bootstrap
#' compare_models(fit1, fit0,
#'   assume.identically.distributed = TRUE,
#'   assume.normality = FALSE)
#'
#' # classical inference
#' compare_models(fit1, fit0,
#'   assume.identically.distributed = TRUE,
#'   assume.normality = TRUE)
#'
#'
#' @import stats
#' @export
compare_models <- function(full.mean.model,
                           reduced.mean.model,
                           simulation.replications = 4999,
                           ...){
  UseMethod("compare_models")
}



#' Estimate the mean response for a given set of covariates.
#'
#' Provides point estimates and confidence intervals for the mean response of a
#' linear or generalized linear model via bootstrapping or classical theory.
#'
#' This wrapper provides a single interface for estimating the mean response
#' under various various conditions imposed on the model. Similar to
#' \code{\link[stats]{predict}}, point and interval estimates of the mean
#' response are available. However, interval estimates can be constructed via
#' bootstrapping or classical theory.
#'
#' For linear models, the following approaches are implemented:
#' \itemize{
#'   \item classical: if both homoskedasticity and normality are assumed, the
#' sampling distributions of a standardized statistic is modeled by a
#' t-distribution.
#'   \item parametric bootstrap: if normality can be assumed but
#' homoskedasticity cannot, a parametric bootstrap can be peformed in which the
#' variance for each observation is estimated by the square of the corresponding
#' residual (similar to a White's correction).
#'   \item residual bootstrap: if homoskedasticity can be assumed, but normality
#' cannot, a residual bootstrap is used to compute standard errors and
#' confidence intervals.
#'   \item wild bootstrap: if neither homoskedasticity nor normality is assumed,
#' a wild bootstrap is used to compute standard errors and confidence intervals.
#' }
#' All methods make additional requirements regarding independence of the error
#' terms and that the model has been correctly specified.
#'
#' For generalized linear models, the following approaches are implemented:
#' \itemize{
#'   \item classical: if the distributional family is assumed correct, large
#'   sample theory is used to justify modeling the sampling distribution of a
#'   standardized statistic using a standard normal distribution.
#'   \item parametric bootstrap: the distributional family is assumed and
#'   a parametric bootstrap is performed to compute standard errors and
#'   confidence intervals.
#'   \item nonparametric bootstrap: a case resampling bootstrap algorithm is
#'   used to estimate standard errors and confidence intervals.
#' }
#' All methods require observations to be independent of one another.
#'
#' Confidence intervals constructed via bootstrapping can take on various forms.
#' The percentile interval is constructed by taking the empirical
#' \eqn{100\alpha} and \eqn{100(1-\alpha)} percentiles from the bootstrap
#' statistics. If \eqn{\hat{F}} is the empirical distribution function of the
#' bootstrap values, then the \eqn{100(1 - 2\alpha)}% percentile interval is
#' given by
#' \deqn{(\hat{F}^{-1}(\alpha), \hat{F}^{-1}(1-\alpha))}
#' The bias-corrected (BC) interval corrects for median-bias.  It is given by
#' \deqn{(\hat{F}^{-1}(\alpha_1), \hat{F}^{-1}(1-\alpha_2))}
#' where
#' \deqn{\alpha_1 = \Phi{2\hat{z}_0 + \Phi^{-1}(\alpha)}}
#' \deqn{\alpha_2 = 1 - \Phi{2\hat{z}_0 + \Phi^{-1}(1-\alpha)}}
#' \deqn{\hat{z}_0 = \Phi^{-1}(\hat{F}(\hat{\beta}))}
#' where \eqn{\hat{\beta}} is the estimate from the original sample.
#' The bootstrap-t interval is based on the bootstrap distribution of
#' \deqn{t^{b} = \frac{\hat{\beta}^{b} -
#' \hat{\beta}}{\hat{\sigma}^{b}}}
#' where \eqn{\hat{\sigma}} is the estimate of the standard error of
#' \eqn{\hat{\beta}} and the superscript b denotes a bootstrap sample. Let
#' \eqn{\hat{G}} be the empirical distribution function of the bootstrap
#' standardized statistics given above.  Then, the bootstrap-t interval is given
#' by
#' \deqn{(\hat{\beta} - \hat{\sigma}\hat{G}^{-1}(1-\alpha),
#' \hat{\beta} - \hat{\sigma}\hat{G}^{-1}\alpha)}
#'
#' @param mean.model \code{lm} or \code{glm} model fit defining the model and
#' therefore the parameters of the mean model to be estimated.
#' @param confidence.level scalar between 0 and 1 indicating the confidence
#' level for all confidence intervals constructed. If missing (default), only
#' point estimates are returned.
#' @param simulation.replications scalar indicating the number of samples to
#' draw from the model for the sampling distribution (default = 4999). This will
#' either be the number of bootstrap relications or the number of samples from
#' the classical sampling distribution. This is ignored if
#' \code{confidence.level} is not specified.
#' @param ... additional arguments to be passed to other methods including the
#' list of variables and their values at which to estimate the mean response.
#'
#' @return data.frame containing a table of estimates. The object has an
#' additional attributed "Sampling Distribution" which is a matrix with
#' \code{simulation.replications} rows and the same number of
#' columns as predictions to be made for the mean.  Each column contains a
#' sample from the corresponding model of the sampling distribution. This is
#' useful for plotting the modeled sampling distribution.
#'
#' @examples
#' fit <- lm(mpg ~ 1 + hp, data = mtcars)
#'
#' # estimate the mean response for vehicle with 120 and 130 horse power.
#' estimate_mean_response(fit,
#'   confidence.level = 0.95,
#'   assume.identically.distributed = TRUE,
#'   assume.normality = TRUE,
#'   hp = c(120, 130))
#'
#' @import stats
#' @export
estimate_mean_response <- function(mean.model,
                                   confidence.level,
                                   simulation.replications = 4999,
                                   ...){
  UseMethod("estimate_mean_response")
}



#' Obtain residuals and fitted values for diagnostics.
#'
#' A wrapper for \code{\link[broom]{augment}} which obtains residuals and
#' fitted values only for a specified data generating process.
#'
#' @param mean.model \code{lm} or \code{glm} model fit defining the model from
#' which residuals and predicted values are to be computed.
#' @param data the original data set to which the fitted values and residuals
#' should be added. Defaults to extracting the data from the model.
#'
#' @return A \code{data.frame} containing one row for each observations, with
#' two columns added to the original data:
#' \itemize{
#'   \item \code{.fitted}: the fitted values for the model.
#'   \item \code{.resid}: the residuals for the model.}
#'
#' @examples
#' fit <- lm(mpg ~ hp, data = mtcars)
#' mtcars.aug <- obtain_diagnostics(fit, data = mtcars)
#'
#' @import stats
#' @export
obtain_diagnostics <- function(mean.model,
                               data = stats::model.frame(mean.model)){
  .out <- broom::augment(mean.model, data = data,
                  type.predict = "response",
                  type.residuals = "deviance")

  .out <- subset(.out, select =  which(!is.element(colnames(.out),
                                                   c(".hat",
                                                     ".sigma",
                                                     ".cooksd",
                                                     ".se.fit",
                                                     ".std.resid",
                                                     ".rownames"))))

  # adjust column names to undo what broom does
  colnames(.out)[1:ncol(data)] <- colnames(data)

  .out
}



#' Produce metrics for the quality of the model fit.
#'
#' A wrapper for \code{\link[broom]{glance}} which obtains metrics on the
#' goodness of fit of the model. For the linear model in particular, some
#' metrics are suppressed.
#'
#' @param mean.model \code{lm} or \code{glm} model fit defining the model for
#' which the goodness of fit should be summarized.
#'
#' @return A \code{data.frame} containing one row of summary metrics.
#'
#' @examples
#' fit <- lm(mpg ~ hp, data = mtcars)
#' summarize_model_fit(fit)
#'
#' @import stats
#' @export
summarize_model_fit <- function(mean.model){
  .out <- broom::glance(mean.model)

  # adjust linear model version
  if (class(mean.model) == "lm"){
    .out <- subset(.out, select = which(!is.element(colnames(.out),
                                                    c("adj.r.squared",
                                                      "statistic",
                                                      "p.value",
                                                      "df"))))
  }
}