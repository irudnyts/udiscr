#' @include p_functions.R
NULL

#' McNamee-Celona "Shortcut" approximation
#'
#' Returns a discrete approximation of a continuous distribution using
#' McNamee-Celona "Shortcut" method.
#'
#' Method approximate a continuous distribution by 0.1, 0.5, and 0.9 quantiles
#' with probabilities 0.25, 0.5, and 0.25, respectively.
#'
#' @param q_fun quantile function.
#' @param params a list of parameters of the distribution.
#' @return a list of pairs probability-outcome.
#'
#' @examples
#' mcnamee_celona(q_fun = qnorm, params = list(mean = 3, sd = 2))
#' mcnamee_celona(q_fun = qgamma, params = list(shape = 3, rate = 2.2))
#' mcnamee_celona(q_fun = qexp)
#' # using sample data
#' sample <- rnorm(100) + rexp(100)
#' q <- function(p) quantile(x = sample, probs = p)
#' mcnamee_celona(q_fun = q)
#'
#' @references \itemize{
#' \item McNamee P., Celona J. \emph{Decision Analysis for the Professional
#' with Supertree}, Scientific Press, Redwood City, CA, 1987.
#' \item Keefer L.D. \emph{Certainty Equivalents for Three-Point Discrete-
#' Distribution Approximations}, Management Science 40(6): 760-773, 1994.
#' }
#'
#' @seealso \code{\link{extended_swanson_megill}},
#' \code{\link{extended_pearson_tukey}}, \code{\link{brown_kahr_peterson_3}}
#'
#' @export
mcnamee_celona <- function(q_fun, params) {
    if(missing(params)) params <- list()
    q <- qf(q_fun = q_fun, params = params)

    return(list(list(prob = 0.25,
                     point = q(0.1)),
                list(prob = 0.5,
                     point = q(0.5)),
                list(prob = 0.25,
                     point = q(0.9))))
}
