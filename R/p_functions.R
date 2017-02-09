qf <- function(q_fun, params) {
    function(p) {
        args <- params
        args[["p"]] <- p
        do.call(what = q_fun, args = args)
    }
}

x_pdf <- function(d_fun, params) {
    function(x) {
        args <- params
        args[["x"]] <- x
        x * do.call(what = d_fun, args = args)
    }
}
