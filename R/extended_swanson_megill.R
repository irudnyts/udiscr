#' @export
extended_swanson_megill <- function(q_fun, params) {
    q <- function(p) {
        args <- params
        args[["p"]] <- p
        do.call(what = q_fun, args = args)
    }

    return(list(list(prob = 0.3,
                     point = q(0.1)),
                list(prob = 0.4,
                     point = q(0.5)),
                list(prob = 0.3,
                     point = q(0.9))))
}
