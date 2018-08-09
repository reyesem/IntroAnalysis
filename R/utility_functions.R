# round numbers below machine epsilon to 0
my_round <- function(u){
  u * (abs(u) > .Machine$double.eps)
}

# Update \code{update()} function to work within function call.
my_update <- function(mod, formula = NULL, data = NULL, offset = NULL) {
  call <- getCall(mod)
  if (is.null(call)) {
    stop("Model object does not support updating (no call)", call. = FALSE)
  }
  term <- terms(mod)
  if (is.null(term)) {
    stop("Model object does not support updating (no terms)", call. = FALSE)
  }

  if (!is.null(data)) call$data <- data
  if (!is.null(offset)) call$offset <- offset
  if (!is.null(formula)) call$formula <- update.formula(call$formula, formula)
  env <- attr(term, ".Environment")

  eval(call, env, parent.frame())
}


# Produce confidence intervals given covariance matrix and estimates.
#
# Extension of \code{confint()} to allow for adjusted variance-covariance
# matrices to be used.
adjconfint <- function(ests, Sigma, level, dfresid){
  parm <- names(ests)
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  if(missing(dfresid)){
    fac <- qnorm(a)
  } else {
    fac <- qt(a, dfresid)
  }
  pct <- paste(format(100 * a, trim = TRUE, scientific = FALSE, digits = 3),
               "%")

  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ses <- sqrt(diag(Sigma))
  ci[] <- ests + ses %o% fac
  ci
}


# Produces a classical-looking ANOVA table from the anova() function.
my_anova <- function(object, ...){
  .table <- as.matrix(anova(object, ...))

  if (class(object)[1] == "lm"){
    .table <- data.frame(
      source = c("Additional Terms", "Error"),
      df = .table[2, c(3, 1)],
      ss = .table[2, c(4, 2)],
      ms = .table[2, c(4, 2)]/.table[2, c(3,1)],
      statistic = .table[c(2, 1), 5],
      p.value = .table[c(2, 1), 6],
      row.names = NULL
    )
  } else if (class(object)[1] == "glm"){
    .table <- data.frame(
      source = c("Additional Terms", "Error"),
      df = .table[2, c(3, 1)],
      deviance = .table[2, c(4, 2)],
      statistic = .table[c(2, 1), ncol(.table) - 1],
      p.value = .table[c(2, 1), ncol(.table)],
      row.names = NULL
    )
  }

  .table
}


# Produces predictions using robust standard errors following predict()
predict_robust <- function(object, newdata, se.fit = FALSE, scale = NULL,
                           df = Inf, interval = c("none", "confidence"),
                           level = 0.95, na.action = na.pass,
                           pred.var = res.var/weights, weights = 1, ...){
  tt <- terms(object)
  if (!inherits(object, "lm"))
    warning("calling predict.lm(<fake-lm-object>) ...")
  if (missing(newdata) || is.null(newdata)) {
    mm <- X <- model.matrix(object)
    mmDone <- TRUE
    offset <- object$offset
  }
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.action,
                     xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset")))
      for (i in off.num){
        offset <- offset + eval(attr(tt,"variables")[[i + 1]], newdata)
      }
    if (!is.null(object$call$offset))
      offset <- offset + eval(object$call$offset, newdata)
    mmDone <- FALSE
  }
  n <- length(object$residuals)
  p <- object$rank
  p1 <- seq_len(p)
  piv <- if (p)
    qr(object)$pivot[p1]
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata)))
    warning("prediction from a rank-deficient fit may be misleading")
  beta <- object$coefficients
  predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
  if (!is.null(offset))
    predictor <- predictor + offset
  interval <- match.arg(interval)

  if (se.fit || interval != "none") {
    w <- object$weights
    res.var <- if (is.null(scale)) {
      r <- object$residuals
      rss <- sum(if (is.null(w)) r^2 else r^2 * w)
      df <- object$df.residual
      rss/df
    }
    else scale^2

    if (p > 0) {
      Sigma <- my_hccm(object, type = "hc0")

      ip <- apply(t(X[, piv, drop = FALSE]) *
                    tcrossprod(Sigma, X[, piv, drop = FALSE]), 2, sum)
    }
    else ip <- rep(0, n)
  }

  if (interval != "none") {
    tfrac <- qt((1 - level)/2, df)
    hwid <- tfrac * sqrt(ip)
    predictor <- cbind(predictor, predictor + hwid %o% c(1, -1))
    colnames(predictor) <- c("fit", "lwr", "upr")
  }
  if (se.fit || interval != "none") {
    se <- sqrt(ip)
  }
  if (missing(newdata) && !is.null(na.act <- object$na.action)) {
    predictor <- napredict(na.act, predictor)
    if (se.fit)
      se <- napredict(na.act, se)
  }
  else if (se.fit)
    list(fit = predictor, se.fit = se, df = df, residual.scale = sqrt(res.var))
  else predictor
}



# Produces predictions given coefficients
predict_coef <- function(object, newdata, beta = object$coefficients){
  tt <- terms(object)
  if (!inherits(object, "lm"))
    warning("calling predict.lm(<fake-lm-object>) ...")
  if (missing(newdata) || is.null(newdata)) {
    mm <- X <- model.matrix(object)
    mmDone <- TRUE
    offset <- object$offset
  }
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.pass,
                     xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset")))
      for (i in off.num){
        offset <- offset + eval(attr(tt, "variables")[[i + 1]], newdata)
      }
    if (!is.null(object$call$offset))
      offset <- offset + eval(object$call$offset, newdata)
    mmDone <- FALSE
  }
  n <- length(object$residuals)
  p <- object$rank
  p1 <- seq_len(p)
  piv <- if (p)
    qr(object)$pivot[p1]
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata)))
    warning("prediction from a rank-deficient fit may be misleading")

  predictor <- drop(X[, piv, drop = FALSE] %*% as.matrix(beta)[piv, ])

  if (!is.null(offset)) predictor <- predictor + offset

  predictor
}

# create a version of car::hccm that works with a single column design matrix
# and if intercept-only model, use unbiased estimate of sigma2
my_hccm <- function (model, type = c("hc3", "hc0", "hc1", "hc2", "hc4"),
          singular.ok = TRUE, ...) {
  e <- na.omit(residuals(model))
  removed <- attr(e, "na.action")
  wts <- if (is.null(weights(model)))
    1
  else weights(model)
  type <- match.arg(type)
  if (any(aliased <- is.na(coef(model))) && !singular.ok)
    stop("there are aliased coefficients in the model")
  sumry <- summary(model, corr = FALSE)
  s2 <- sumry$sigma^2
  V <- sumry$cov.unscaled
  if (type == FALSE)
    return(s2 * V)
  h <- hatvalues(model)
  if (!is.null(removed)) {
    wts <- wts[-removed]
    h <- h[-removed]
  }
  X <- model.matrix(model)[, !aliased, drop = FALSE]
  df.res <- df.residual(model)
  n <- length(e)
  e <- wts * e
  p <- ncol(X)

  if (p == 1 && type != "hc3" && all(X[, 1, drop = TRUE] == 1)){
    return(vcov(model))
  }

  factor <- switch(type, hc0 = 1, hc1 = df.res/n, hc2 = 1 -
                     h, hc3 = (1 - h)^2, hc4 = (1 - h)^pmin(4, n * h/p))
  V %*% t(X) %*% apply(X, 2, "*", (e^2)/factor) %*% V
}