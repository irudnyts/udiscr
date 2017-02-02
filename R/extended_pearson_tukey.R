#' @export
extended_pearson_tukey <- function(q_fun, params) {
    q <- function(p) {
        args <- params
        args[["p"]] <- p
        do.call(what = q_fun, args = args)
    }

    return(list(list(prob = 0.185,
                     point = q(0.05)),
                list(prob = 0.630,
                     point = q(0.5)),
                list(prob = 0.185,
                     point = q(0.95))))
}
