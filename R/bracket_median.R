#' Bracket-median approximation
#'
#' Returns a discrete approximation of a continuous distribution using
#' bracket-median method.
#'
#' If \code{n} is specified, the function splits the range of possible values
#' into \code{n} intervals by using 1/n, 2/n, ... (n-1)/n-quantiles. Otherwise,
#' if \code{p} is specified, the range is split by corresponding cumulative
#' probabilities quantiles. \code{q_fun} is used to calculate quantiles. Each
#' interval is represented by pair of probability and median of the interval.
#'
#' @param n numeric of length 1; represents a number of intervals (or a number
#'     of point of the approximation). Default value is 3. Either \code{n} or
#'     \code{p} should be used.
#' @param p numeric of length grater than 2; specifies the corresponding
#'     probabilities of intervals. If \code{p} does not sum up to 1, will be
#'     automatically standardized.
#' @param q_fun quantile function.
#' @param params a list of parameters of the distribution.
#' @return a list of parirs probability-outcome.
#'
#' @examples
#' bracket_median(n = 5, q_fun = qnorm, params = list(mean = 3, sd = 2))
#' bracket_median(p = c(1/2, 1/3, 1/6), q_fun = qexp, params = list(rate = 1))
#' bracket_median(q_fun = qnorm, d_fun = dnorm)
#'
#' @references \itemize{
#' \item Miller III A.C., Rice T.R. \emph{Discrete Approximations of Probability
#' Distributions}. Management Science, 29(3):352-362, 1983.
#' \item Smith J.E. \emph{Moment Methods for Decision Analysis}. Management
#' Science 39(3):340-358, 1993.
#' \item Keefer D.L., Bodily S.E. \emph{Three-Point Approximations for
#' Continuous Random Variables}. Management Science 29(5):595-609, 1983.
#' }
#'
#' @seealso \code{\link{bracket_mean}}
#'
#' @export
bracket_median <- function(n = 3, p = rep(1 / n, n), q_fun, params) {

    # check arguments
    if(!missing(n)) {
        if(!missing(p)){
            if(all(rep(1 / n, n) == p))
                warning("Either n or p should be specified.")
            else
                stop("Either n or p should be specified.")
        }
        if(!is.numeric(n)) stop("n should be numeric.")
        if(length(n) != 1) stop("Length of n should be 1.")
        if(n %% 1 != 0) {
            n <- round(n)
            message("n will be rounded.")
        }
        if(n < 2) stop("n must be at least 2.")
    } else if(!missing(p)) {
        if(!is.numeric(p)) stop("p should be numeric.")
        if(length(p) < 2) stop("Length of p should be at least 2.")
        if(any(is.na(p))) {
            p <- p(!is.na(p))
            message("NA will be deleted from p.")
        }
        if(sum(p) != 1) {
            p <- p / sum(p)
            message("p will be standardized")
        }
    }

    if(missing(params)) params <- list()

    # generate probabilities at which quantiles will be calculated
    probs <- c(0, cumsum(p))
    probs <- probs[-length(probs)]
    probs <- probs + p * 1 / 2

    # define quantile function, which includes all parameters and  depends only
    # on probability
    q <- function(p) {
        args <- params
        args[["p"]] <- p
        do.call(what = q_fun, args = args)
    }

    discr <- Map(function(x, y) list(prob = x, point = y),
                 p, q(probs))

    return(discr)
}
