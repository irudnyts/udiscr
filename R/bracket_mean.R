#' @export
bracket_mean <- function(n = 3, p = rep(1 / n, n), q_fun, d_fun, params) {

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

    cum_probs <- cumsum(p)

    # define quantile function, which includes all parameters and  depends only
    # on probability
    q <- function(p) {
        args <- params
        args[["p"]] <- p
        do.call(what = q_fun, args = args)
    }

    # calculate lower and upper bounds of integrals by applying quantile
    # function
    lower_bounds <- q(c(0, cum_probs[-length(cum_probs)]))
    upper_bounds <- q(cum_probs)

    # integrated function, which icludes all parameters and depends only on x
    f <- function(x) {
        args <- params
        args[["x"]] <- x
        x * do.call(what = d_fun, args = args)
    }

    discr <- list()
    for(i in seq_along(p)) {
        discr[[i]] <- list(prob = p[i],
                           point = integrate(f = f,
                                             lower = lower_bounds[i],
                                             upper = upper_bounds[i])$value /
                               p[i])
    }
    return(discr)
}
