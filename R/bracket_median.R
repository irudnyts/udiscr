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
