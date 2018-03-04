#' Compute a p-value comparing two models.
#'
#' Tests whether a reduced (nested) linear model is sufficient for explaining
#' the variability in the response compared to a full linear model.
#'
#' This wrapper provides a single interface for comparing models under various
#' conditions imposed on the error term in the linear model.  The ANOVA table
#' comparing the two models is returned, similar to
#' \code{\link[stats]{anova.lm}}.  However, the p-value provided can differ
#' depending on the conditions imposed on the error term.  The classical
#' approach implemented by \code{\link[stats]{summary.lm}} and
#' \code{\link[stats]{anova.lm}} can be recovered as well as having the p-value
#' computed based on a residual or wild bootstrap. The following approaches
#' are implemented:
#' \itemize{
#' \item classical: if both homoskedasticity and normality are assumed, the
#' null distribution is modeled by an F-distribution.
#' \item White-Huber correction: if normality can be assumed but
#' homoskedasticity cannot, the variance-covariance matrix is replaced by the
#' White-Huber consistent variance-covariance estimate when computing the test
#' statistic, and the null distribution is modeled by an F-distribution.
#' \item residual bootstrap: if homoskedasticity can be assumed, but normality
#' cannot, a residual bootstrap is used to compute the p-value.
#' \item wild bootstrap: if neither homoskedasticity nor normality is assumed,
#' a wild bootstrap is used to compute the p-value.}
#' We do not implement methods which assume normality but not homoskedasticity.
#' Further, all methods make additional requirements regarding independence of
#' the error terms and that the model has been correctly specified. The work is
#' done primarily by the workhorse function \code{\link{boot_residual_p}}.
#'
#' @param fit.h1 \code{lm} model fit defining the full model.
#' @param fit.h0 \code{lm} model fit defining the reduced model under the null
#' hypothesis. It can also be a numeric scalar for short-hand in the single
#' mean case (see examples).
#' @param assume.constant.variance boolean; if \code{TRUE} (default),
#' homoskedasticity is assumed for the error term. If \code{FALSE}, this is not
#' assumed.
#' @param assume.normality boolean; if \code{TRUE} (default), the errors are
#' assumed to follow a Normal distribution. If \code{FALSE}, this is not
#' assumed. This is ignored if \code{assume.constant.variance = FALSE}.
#' @param bootstrap.reps scalar indicating the number of bootstrap replications
#' to use (default = 4999).  This is ignored if both
#' \code{assume.constant.variance = TRUE} and
#' \code{assume.normality = TRUE}.
#' @param construct string defining the type of construct to use when generating
#' from the distribution for the wild bootrap (see \code{\link{rmammen}}). If
#' \code{assume.constant.variance = TRUE}, this is ignored
#' (default = \code{"normal-2"}).
#'
#' @return data.frame containing an ANOVA table comparing the two models. The
#' data.frame has a single attribute "Null Distribution" which is a numeric
#' vector of length \code{bootstrap.reps} which contains a sample from the
#' model of the null distribution of the test statistic. This is useful for
#' pedagogical purposes in plotting the null distribution of the statistic.
#'
#' @seealso \code{\link[stats]{anova.lm}}, \code{\link{boot_residual_p}}
#'
#' @examples
#' test.df <- data.frame(x = seq(10), y = seq(10) + rnorm(10))
#' fit1 <- lm(y ~ x, data = test.df)
#' fit0 <- lm(y ~ 1, data = test.df)
#' compare_models(fit1, fit0,
#' assume.constant.variance = TRUE,
#' assume.normality = FALSE)
#'
#' # Testing overall mean response
#' #  H0: mu = 5  vs.  H1: mu =/= 5
#' fit1 <- lm(y ~ 1, data = test.df)
#' fit0 <- lm(y ~ -1, offset = rep(5, length(y)), data = test.df)
#'
#' # the following two methods are equivalent
#' set.seed(123)
#' compare_models(fit1, fit0,
#' assume.constant.variance = TRUE,
#' assume.normality = FALSE)
#'
#' set.seed(123)
#' compare_models(fit1, 5,
#' assume.constant.variance = TRUE,
#' assume.normality = FALSE)
#'
#' @import stats
#' @export
compare_models <- function(fit.h1,
                           fit.h0,
                           assume.constant.variance = TRUE,
                           assume.normality = TRUE,
                           bootstrap.reps = 4999,
                           construct = "normal-2"){
  if (missing(fit.h0) || missing(fit.h1)){
    stop("Both a full model and reduce model must be specified.")
  }

  if (class(fit.h0)[1]=="numeric"){
    .offsetconst <- rep(fit.h0, length(fit.h1$residuals))
    fit.h0 <- my_update(fit.h1,
                        formula = . ~ -1,
                        offset = .offsetconst)
  }

  if(!is.null(fit.h0$offset) && assume.normality && !assume.constant.variance){
    warning(paste("White-Huber estimator cannot be used with offset term.",
                  "Recommend using linearHypothesis() within the car",
                  "package. Continuing with imposing constant variance."))

    assume.constant.variance <- TRUE
  }

  if (assume.normality && assume.constant.variance){
    .anova <- as.data.frame(anova(fit.h0, fit.h1))
    .anova <- cbind("Model" = c("Full Model", "Reduced Model"),
                    .anova[c(2, 1), ])
    colnames(.anova) <- c("model", "error.df", "error.sumsq", "df",
                          "sumsq", "statistic", "p.value")
    rownames(.anova) <- NULL

    .boot <- rf(bootstrap.reps, df1 = .anova$df[1], df2 = .anova$error.df[1])
  } else if (assume.normality && !assume.constant.variance){
    .anova <- as.data.frame(anova(fit.h0, fit.h1))
    .anova <- cbind("Model" = c("Full Model", "Reduced Model"),
                    .anova[c(2, 1), ])
    colnames(.anova) <- c("model", "error.df", "error.sumsq", "df",
                          "sumsq", "statistic", "p.value")
    rownames(.anova) <- NULL

    # use White-Huber adjustment
    .namesh0 <- names(fit.h0$coefficients)
    .namesh1 <- names(fit.h1$coefficients)

    if (!all(is.element(.namesh0, .namesh1))){
      stop("Models are not nested.")
    }

    .K <- diag(length(.namesh1))[!(.namesh1 %in% .namesh0),, drop = FALSE]
    .test <- car::linearHypothesis(fit.h1, .K, white.adjust = "hc0")

    .anova[1, "statistic"] <- .test$F[2]
    .anova[1, "p.value"] <- .test$`Pr(>F)`[2]

    .boot <- rf(bootstrap.reps, df1 = .anova$df[1], df2 = .anova$error.df[1])

  } else {
    .boot <- boot_residual_p(fit.h1, fit.h0, reps = bootstrap.reps,
                             wild = !assume.constant.variance,
                             construct = construct)

    .anova <- boot_compute_p(.boot)
  }

  attr(.anova, "Null Distribution") <- as.numeric(.boot)
  .anova
}
