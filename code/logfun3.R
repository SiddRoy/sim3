# Logsum functions and log(1 +- exp(x))

#' Logsum functions
#'
#' log(sum(exp(x))) or log(cumsum(exp(x)))
#'
#' @param x input logged values
#' @return logsum or logcumsum
#' @export
#' @examples
#' set.seed(5)
#' x <- rnorm(10)
#' logsum(x) - log(sum(exp(x)))
#' logcumsum(x) - log(cumsum(exp(x)))
logsum <- function(x) {
  if(length(x) == 1) return(x)
  w <- which.max(x)
  if(is.infinite(x[w])) return(log(sum(exp(x))))
  x[w] + log1p(sum(exp(x[-w] - x[w])))
}

#' @rdname logsum
#' @export
logcumsum <- function(x) {
  y <- rep(0, length(x))
  y[1] <- x[1]
  for(i in 2:length(x)) {
    y[i] <- logsum(c(y[i-1], x[i]))
  }
  y
}

#' Numerically accurate log(1+exp(x)) and log(1-exp(x))
#'
#' log(1 + exp(x)) = log(plogis(-x)) and
#' log(1 - exp(x)) = log(pexp(-x)) for x < 0
#'
#' @param x must be negative for log_neg_expm1
#' @return \eqn{log(1 \pm e^x)}
#' @export
#' @examples
#'
#' A <- -8; B <- -9
#' log(exp(A) + exp(B)); A + log1p_exp(B - A)
#' log(exp(A) - exp(B)); A + log_neg_expm1(B - A)
#'
log1p_exp <- function(x) -plogis(-x, log.p = TRUE)

#' @rdname log1p_exp
#' @export
log_neg_expm1 <- function(x) pexp(-x, log.p = TRUE)

