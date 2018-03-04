# There seems to be a problem with normal-1 as described in Mammen.  The
# distribution does not have E(U^3) = 1 but instead is 2.5.

#' Distributions suggested by Mammen for Wild Bootstrap.
#'
#' Generates random variates from one of three distributions suggested by
#' Mammen (2003) for use in Wild Bootstrap.
#'
#' Generates a random variable \eqn{U}.  If \code{construct = "two-point mass"},
#' the random variable \eqn{U} is defined as
#' \deqn{Pr(U = u) = \begin{cases}
#' \frac{\sqrt{5} + 1}{2\sqrt{5}} & u = \frac{-(\sqrt{5}-1)}{2} \\
#' \frac{\sqrt{5} - 1}{2\sqrt{5}} & u = \frac{\sqrt{5}+1}{2} \end{cases}}
#' If \code{construct = "normal-1"}, the random variable \eqn{U} is defined as
#' \deqn{U = \frac{V}{\sqrt{2} + \frac{V^2 - 1}{2}}}
#' where \eqn{V \sim N(0,1)}.
#' If \code{construct = "normal-2"}, the random variable \eqn{U} is defined as
#' \deqn{U = \left(\delta_1 + V_1/\sqrt{2}\right)
#' \left(\delta_2 + V_2/\sqrt{2}\right) - \delta_1\delta_2}
#' where \eqn{V_1,V_2} are independent \eqn{N(0,1)} random variables and
#' \deqn{\delta_1 = \left(3/4 + \sqrt{17}/12\right)^{1/2}}
#' \deqn{\delta_2 = \left(3/4 - \sqrt{17}/12\right)^{1/2}}
#'
#' @param n Number of observations. If \code{length(n) > 1}, the length is taken
#' to be the number required.
#' @param construct Character string; indicates which of three constructs from
#' which to generate.  One of "two-point mass", "normal-1", normal-2" (default).
#'
#' @return A vector of length \code{n}.
#'
#' @examples
#' mammen_sample <- rmammen(10000, construct = "normal-2")
#' mean(mammen_sample)
#' mean(mammen_sample^2)
#' mean(mammen_sample^3)
#'
#' plot(density(mammen_sample,
#' main = "Mammen's Suggested Distribution for the Wild Bootstrap"))
#'
#' @references
#' Mammen E. "Bootstrap and Wild Bootstrap for High Dimensional Linear Models."
#' \emph{The Annals of Statistics} (1993) 21(1):255-285.
#'
#' @import stats
#' @export
rmammen <- function(n,
                    construct = c("normal-2", "normal-1", "two-point mass")){
  construct <- match.arg(construct)

  if(length(n)>1) n <- length(n)

  if(construct=="two-point mass"){
    .vals <- c(-(sqrt(5)-1)/2, (sqrt(5)+1)/2)
    .probs <- rev(abs(.vals)/sqrt(5))

    sample(.vals, size = n, replace = TRUE, prob = .probs)
  } else if(construct=="normal-1"){
    .v <- rnorm(n)

    (.v/sqrt(2)) + (0.5*((.v*.v) - 1))
  } else {
    .delta1 <- sqrt((3/4) + (sqrt(17)/12))
    .delta2 <- sqrt((3/4) - (sqrt(17)/12))
    .v <- matrix(rnorm(2*n), nrow=n, ncol=2)

    (.delta1 + (.v[,1]/sqrt(2)))*(.delta2 + (.v[,2]/sqrt(2))) -
      (.delta1*.delta2)
  }
}

