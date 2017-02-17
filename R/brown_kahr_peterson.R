#' @include p_functions.R
NULL

#' Three-point approximation, proposed by Brown, Kahr, and Peterson
#'
#' Returns a discrete approximation of a continuous distribution using
#' three-point approximation, proposed by Brown, Kahr, and Peterson.
#'
#' Note: only distribution with finite quantiles are supported.
#'
#' @param q_fun quantile function.
#' @param params a list of parameters of the distribution.
#' @return a list of pairs probability-outcome.
#'
#' @examples
#' brown_kahr_peterson_3(q_fun = qbeta, params = list(shape1 = 2, shape2 = 3))
#' brown_kahr_peterson_3(q_fun = qunif)
#' # using sample data
#' sample <- rnorm(100) + rexp(100)
#' q <- function(p) quantile(x = sample, probs = p)
#' brown_kahr_peterson_3(q_fun = q)
#'
#' \dontrun{
#' brown_kahr_peterson_3(q_fun = qnorm)
#' }
#'
#' @references \itemize{
#' \item Keefer D.L., Bodily S.E. \emph{Three-Point Approximations for
#' Continuous Random Variables}. Management Science 29(5):595-609, 1983.
#' \item Brown R.V., Kahr A.S., and Peterson C. \emph{Decision Analysis for the
#' Management}, Holt, Rinehart, and Winston, New York, 1974.
#' }
#'
#' @seealso \code{\link{extended_swanson_megill}}, \code{\link{mcnamee_celona}},
#' \code{\link{extended_pearson_tukey}}
#'
#' @export
brown_kahr_peterson_3 <- function(q_fun, params) {
    if(missing(params)) params <- list()
    q <- qf(q_fun = q_fun, params = params)
    if(q(0) == -Inf || q(1)  == Inf)
        stop(paste("Only distribution with finite quantiles are supported",
                   "by this approximation."))
    return(list(list(prob = 0.25,
                     point = (3 * q(0) + 5 * q(0.5)) / 8),
                list(prob = 0.5,
                     point = (q(0) + 14 * q(0.5) + q(1)) / 16),
                list(prob = 0.25,
                     point = (5 * q(0.5) + 3 * q(1)) / 8)))
}


#' Five-point approximation, proposed by Brown, Kahr, and Peterson
#'
#' Returns a discrete approximation of a continuous distribution using
#' five-point approximation, proposed by Brown, Kahr, and Peterson.
#'
#' Note: only distribution with finite quantiles are supported.
#'
#' @param q_fun quantile function.
#' @param params a list of parameters of the distribution.
#' @return a list of pairs probability-outcome.
#'
#' @examples
#' brown_kahr_peterson_5(q_fun = qbeta, params = list(shape1 = 2, shape2 = 3))
#' brown_kahr_peterson_5(q_fun = qunif)
#' # using sample data
#' sample <- rnorm(100) + rexp(100)
#' q <- function(p) quantile(x = sample, probs = p)
#' brown_kahr_peterson_5(q_fun = q)
#'
#' \dontrun{
#' brown_kahr_peterson_5(q_fun = qnorm)
#' }
#'
#' @references \itemize{
#' \item Keefer D.L., Bodily S.E. \emph{Three-Point Approximations for
#' Continuous Random Variables}. Management Science 29(5):595-609, 1983.
#' \item Brown R.V., Kahr A.S., and Peterson C. \emph{Decision Analysis for the
#' Management}, Holt, Rinehart, and Winston, New York, 1974.
#' }
#'
#' @seealso \code{\link{extended_swanson_megill}}, \code{\link{mcnamee_celona}},
#' \code{\link{extended_pearson_tukey}}
#'
#' @export
brown_kahr_peterson_5 <- function(q_fun, params) {
    if(missing(params)) params <- list()
    q <- qf(q_fun = q_fun, params = params)
    if(q(0) == -Inf || q(1)  == Inf)
        stop(paste("Only distribution with finite quantiles are supported",
                   "by this approximation."))

    v2 <- q(0.25); p2 <- 0.5 * (q(0.5) - q(0.25)) / (q(0.25) - q(0))
    v4 <- q(0.75); p4 <- 0.5 * (q(0.75) - q(0.5)) / (q(1) - q(0.75))
    v1 <- q(0.25) - 0.25 * (1 + 2 * p2) * (q(0.5) - q(0))
    p1 <- 0.25 * (1 - 2 * p2)
    v5 <- q(0.75) + 0.25 * (1 + 2 * p4) * (q(1) - q(0.5))
    p5 <- 0.25 * (1 - 2 * p4)
    s <- 0.5 * (q(0.25) + q(0.5)) + 0.5 * p2 * (q(0.5) - q(0))
    t <- 0.5 * (q(0.5) + q(0.75)) + 0.5 * p4 * (q(1) - q(0.5))
    p3 <- p1 + p5; v3 <- (s * p1 + t * p5) / p3

    return(list(list(prob = p1, point = v1),
                list(prob = p2, point = v2),
                list(prob = p3, point = v3),
                list(prob = p4, point = v4),
                list(prob = p5, point = v5)))
}
