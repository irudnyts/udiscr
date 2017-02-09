#' @include p_functions.R
NULL

#' Extended Pearson-Tukey three-point approximation
#'
#' Returns a discrete approximation of a continuous distribution using
#' extended Person-Tukey method.
#'
#' Method approximate a continuous distribution by 0.05, 0.5, and 0.95 quantiles
#' with probabilities 0.185, 0.630, and 0.185, respectively.
#'
#' @param q_fun quantile function.
#' @param params a list of parameters of the distribution.
#' @return a list of pairs probability-outcome.
#'
#' @examples
#' extended_pearson_tukey(q_fun = qnorm, params = list(mean = 3, sd = 2))
#' extended_pearson_tukey(q_fun = qlnorm, params = list(meanlog = 3, sdlog = 2))
#' extended_pearson_tukey(q_fun = qexp)
#' # using sample data
#' sample <- rnorm(100) + rexp(100)
#' q <- function(p) quantile(x = sample, probs = p)
#' extended_pearson_tukey(q_fun = q)
#'
#' @references \itemize{
#' \item Smith J.E. \emph{Moment Methods for Decision Analysis}. Management
#' Science 39(3):340-358, 1993.
#' \item Keefer D.L., Bodily S.E. \emph{Three-Point Approximations for
#' Continuous Random Variables}. Management Science 29(5):595-609, 1983.
#' \item Pearson E.S., Tukey J.W. \emph{Approximate Means and Standard Deviations
#' Based on Distances between Percentage Points of Frequency Curves}.
#' Biometrika 52(3/4):533-546, 1965.
#' }
#'
#' @seealso \code{\link{extended_swanson_megill}}, \code{\link{mcnamee_celona}},
#' \code{\link{brown_kahr_peterson_3}}
#'
#' @export
extended_pearson_tukey <- function(q_fun, params) {
    if(missing(params)) params <- list()
    q <- qf(q_fun = q_fun, params = params)

    return(list(list(prob = 0.185,
                     point = q(0.05)),
                list(prob = 0.630,
                     point = q(0.5)),
                list(prob = 0.185,
                     point = q(0.95))))
}
