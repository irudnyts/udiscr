context("Bracket-mean method")

test_that("bracket_mean checks argument n on validity corretly", {
    expect_error(bracket_mean(n = "a", q_fun = qnorm, d_fun = dnorm))
    expect_error(bracket_mean(n = c(1, 2), q_fun = qnorm, d_fun = dnorm))
    expect_message(bracket_mean(n = 3.3, q_fun = qnorm, d_fun = dnorm),
                   "n will be rounded.")
    expect_error(bracket_mean(n = 1.1, q_fun = qnorm, d_fun = dnorm))
})

test_that("bracket_mean checks argument p on validity corretly", {
    expect_error(bracket_mean(p = 1, q_fun = qnorm, d_fun = dnorm))
    expect_error(bracket_mean(p = "a", q_fun = qnorm, d_fun = dnorm))
    expect_message(bracket_mean(p = c(1, 1), q_fun = qnorm, d_fun = dnorm))
    expect_warning(bracket_mean(n = 3, p = c(1 / 3, 1 / 3, 1 / 3),
                                q_fun = qnorm, d_fun = dnorm))
    expect_error(bracket_mean(n = 4, p = c(1 / 4, 0 / 4, 2 / 4, 1 / 4),
                              q_fun = qnorm, d_fun = dnorm))
})

test_that("bracket_mean checks argument params on validity corretly", {
    expect_silent(bracket_mean(q_fun = qnorm, d_fun = dnorm, params = list()))
    expect_silent(bracket_mean(q_fun = qnorm, d_fun = dnorm))
    expect_error(bracket_mean(q_fun = qnorm, d_fun = dnorm, params = list(a = 1)))
})

test_that("bracket_mean returns corret result", {

    rslt_3 <- list(list(prob = 1 / 3,
                        point = integrate(f = function(x) x * dnorm(x),
                                          lower = qnorm(0),
                                          upper = qnorm(1 / 3))$value * 3),
                   list(prob = 1 / 3,
                        point = integrate(f = function(x) x * dnorm(x),
                                          lower = qnorm(1 / 3),
                                          upper = qnorm(2 / 3))$value * 3),
                   list(prob = 1 / 3,
                        point = integrate(f = function(x) x * dnorm(x),
                                          lower = qnorm(2 / 3),
                                          upper = qnorm(1))$value * 3))
    m <- 3; s <- 5
    rslt_4 <- list(list(prob = 1 / 4,
                        point = integrate(f = function(x) x * dnorm(x, m, s),
                                          lower = qnorm(0, m, s),
                                          upper = qnorm(1 / 4, m, s))$value * 4),
                   list(prob = 1 / 4,
                        point = integrate(f = function(x) x * dnorm(x, m, s),
                                          lower = qnorm(1 / 4, m, s),
                                          upper = qnorm(2 / 4, m, s))$value * 4),
                   list(prob = 1 / 4,
                        point = integrate(f = function(x) x * dnorm(x, m, s),
                                          lower = qnorm(2 / 4, m, s),
                                          upper = qnorm(3 / 4, m, s))$value * 4),
                   list(prob = 1 / 4,
                        point = integrate(f = function(x) x * dnorm(x, m, s),
                                          lower = qnorm(3 / 4, m, s),
                                          upper = qnorm(1, m, s))$value * 4))
    rslt_p <- list(list(prob = 1 / 2,
                        point = integrate(f = function(x) x * dnorm(x, m, s),
                                          lower = qnorm(0, m, s),
                                          upper = qnorm(1 / 2, m, s))$value * 2),
                   list(prob = 1 / 3,
                        point = integrate(f = function(x) x * dnorm(x, m, s),
                                          lower = qnorm(1 / 2, m, s),
                                          upper = qnorm(5 / 6, m, s))$value * 3),
                   list(prob = 1 / 6,
                        point = integrate(f = function(x) x * dnorm(x, m, s),
                                          lower = qnorm(5 / 6, m, s),
                                          upper = qnorm(1, m, s))$value * 6))

    expect_equal(bracket_mean(q_fun = qnorm, d_fun = dnorm), rslt_3)
    expect_equal(bracket_mean(n = 4, q_fun = qnorm, d_fun = dnorm,
                              params = list(mean = m, sd = s)), rslt_4)
    expect_equal(bracket_mean(p = c(1 / 2, 1 / 3, 1 / 6), q_fun = qnorm,
                              d_fun = dnorm, params = list(mean = m, sd = s)),
                 rslt_p)

})
