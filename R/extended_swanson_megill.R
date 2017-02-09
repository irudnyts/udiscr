#' Extended Swanson-Megill Three-Point approximation
#'
#' Returns a discrete approximation of a continuous distribution using
#' extended Swanson-Megill method.
#'
#' Method approximate a continuous distribution by 0.1, 0.5, and 0.9 quantiles
#' with probabilities 0.3, 0.4, and 0.3, respectively.
#'
#' @param q_fun quantile function.
#' @param params a list of parameters of the distribution.
#' @return a list of pairs probability-outcome.
#'
#' @examples
#' extended_swanson_megill(q_fun = qnorm, params = list(mean = 3, sd = 2))
#' extended_swanson_megill(q_fun = qgamma, params = list(shape = 3, rate = 2.2))
#' extended_swanson_megill(q_fun = qexp)
#'
#' @references \itemize{
#' \item Keefer D.L., Bodily S.E. \emph{Three-Point Approximations for
#' Continuous Random Variables}. Management Science 29(5):595-609, 1983.
#' \item Keefer L.D. \emph{Certainty Equivalents for Three-Point Discrete-
#' Distribution Approximations}, Management Science 40(6): 760-773, 1994.
#' }
#'
#' @seealso \code{\link{extended_pearson_tukey}}, \code{\link{mcnamee_celona}}
#'
#' @export
extended_swanson_megill <- function(q_fun, params) {
    if(missing(params)) params <- list()
    q <- qf(q_fun = q_fun, params = params)

    return(list(list(prob = 0.3,
                     point = q(0.1)),
                list(prob = 0.4,
                     point = q(0.5)),
                list(prob = 0.3,
                     point = q(0.9))))
}
