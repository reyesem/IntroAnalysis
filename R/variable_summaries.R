#' Compute summary statistics for a variable.
#'
#' Compute summary statistics, such as the mean or median, for a variable,
#' potentially within various groups.
#'
#' @param formula an object of class "\code{\link[stats]{formula}}": a symbolic
#' description identifying the response and the predictors of interest.
#' @param data an optional data frame containing the variables appearing in
#' \code{formula}.
#' @param ... a comma separated list of functions, each of which computes a
#' statistic of interest.
#' @param .args a named list of additional arguments to be added to all function
#' calls.
#'
#' @return data.frame containing the values of interest
#'
#' @examples
#' summarize_variable(mpg ~ am, data = mtcars, mean, median)
#'
#' @aliases summarise_variable
#'
#' @export
summarize_variable <- function(formula,
                               data = NULL,
                               ...,
                               .args = list()){

  .mf <- model.frame.default(formula, data)
  .mf <- dplyr::group_by_at(.mf, -1)

  as.data.frame(dplyr::summarise_at(.mf, 1, dplyr::funs(..., .args = .args)))
}



#' Compute statistics summarizing a relationship between variables.
#'
#' Compute statistics, such as a correlation coefficient, summarizing the
#' relationship between two variables.
#'
#' The formula is used to specify the response, the predictor variable and
#' optionally a grouping variable which is specified in the form
#' \code{Response ~ Predictor + Group}.
#'
#' @param formula an object of class "\code{\link[stats]{formula}}": a symbolic
#' description identifying the response and the predictor of interest and
#' optionally a grouping variable.
#' @param data an optional data frame containing the variables appearing in
#' \code{formula}.
#' @param FUN a function taking at least two arguments, in which the first
#' two arguments are the two variables to be related.
#' @param ... additional parameters to pass to FUN.
#'
#' @return data.frame containing the statistics of interest
#'
#' @examples
#' summarize_relationship(mpg ~ hp, data = mtcars, cor)
#' summarize_relationship(mpg ~ hp, data = mtcars, cor, method = "spearman")
#'
#' @aliases summarise_relationship
#'
#' @export
summarize_relationship <- function(formula,
                                   data = NULL,
                                   FUN = cor,
                                   ...){
  .mf <- model.frame.default(formula, data)

  if (ncol(.mf) == 1){
    stop("Must specify both a response and predictor in formula.")
  } else if (ncol(.mf) > 3){
    warning("More than two predictors specified, only using first two.")

    .mf <- .mf[, c(1, 2, 3)]
  }

  if (ncol(.mf) == 3){
    .mf <- dplyr::group_by_at(.mf, 3)
  }

  FUN <- match.fun(FUN)

  # group data and then apply function to each group
  .mf <- tidyr::nest(.mf, .key = "statistic")

  .out <- dplyr::mutate(.mf,
                        statistic = purrr::map(statistic, function(u){
                          FUN(u[, 1], u[, 2], ...)
                        }))

  as.data.frame(tidyr::unnest(.out))
}



#' Compute specific percentiles of a numeric variable.
#'
#' These are wrappers for the more common \code{\link[stats]{quantile}} function
#' which computes the most commonly used percentiles. Functions beginning with
#' \code{perc} compute the corresponding percentile; functions beginning with
#' \code{Q} compute the corresponding quartile, and \code{IQR} computes the
#' interquartile range.
#'
#' @param x numeric vector whose sample quantiles are wanted.  \code{NA} and
#' \code{NaN} values are not allowed in numeric vectors unless \code{na.rm} is
#' \code{TRUE}.
#' @param na.rm logical; if \code{TRUE}, any \code{NA} or \code{NaN}'s are
#' removed from \code{x} before the quantiles are computed.
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#' algorithms detailed in \code{\link[stats]{quantile}}.
#'
#' @examples
#' summarize_variable(mpg ~ 1, data = mtcars, perc01)
#'
#' @export
perc01 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.01, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
perc05 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.05, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
perc10 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.10, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
perc20 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.20, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
perc80 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.80, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
perc85 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.85, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
perc90 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.90, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
perc95 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.95, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
perc99 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.99, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
Q1 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.25, na.rm = na.rm, type = type, names = FALSE)
}


#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
Q2 <- function(x, na.rm = FALSE, type = 7){
  quantile(x, probs = 0.75, na.rm = na.rm, type = type, names = FALSE)
}

#' @describeIn perc01
#'
#' @inheritParams perc01
#'
#' @export
IQR <- function(x, na.rm = FALSE, type = 7){
  diff(quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, type = type,
                names = FALSE))
}


#' Compute percent for a binary variable.
#'
#' This is just shorthand for taking \code{100*mean(x)}.
#'
#' @param x numeric vector or logical vector for which the percent of 1's is
#' desired.  \code{NA} and \code{NaN} values are not allowed in numeric vectors
#' unless \code{na.rm} is \code{TRUE}.
#' @param na.rm logical; if \code{TRUE}, any \code{NA} or \code{NaN}'s are
#' removed from \code{x} before the quantiles are computed.
#'
#' @examples
#' summarize_variable(am ~ 1, data = mtcars, percent)
#'
#' @export
percent <- function(x, na.rm = FALSE){
  100*mean(x, na.rm = na.rm)
}