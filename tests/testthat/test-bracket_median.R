context("Bracket-median method")

test_that("bracket_median checks argument n on validity corretly", {
    expect_error(bracket_median(n = "a", q_fun = qnorm))
    expect_error(bracket_median(n = c(1, 2), q_fun = qnorm))
    expect_message(bracket_median(n = 3.3, q_fun = qnorm), "n will be rounded.")
    expect_error(bracket_median(n = 1, q_fun = qnorm))
    expect_error(bracket_median(n = 1.1, q_fun = qnorm))
    expect_message(bracket_median(n = 1.5, q_fun = qnorm))
})

test_that("bracket_median checks argument p on validity corretly", {
    expect_error(bracket_median(p = 1, q_fun = qnorm))
    expect_error(bracket_median(p = "a", q_fun = qnorm))
    expect_message(bracket_median(p = c(1, 1), q_fun = qnorm))
    expect_warning(bracket_median(n = 3, p = c(1 / 3, 1 / 3, 1 / 3),
                                  q_fun = qnorm))
    expect_error(bracket_median(n = 4, p = c(1 / 4, 0 / 4, 2 / 4, 1 / 4),
                              q_fun = qnorm))
})

test_that("bracket_median checks argument params on validity corretly", {
    expect_silent(bracket_median(q_fun = qnorm, params = list()))
    expect_silent(bracket_median(q_fun = qnorm))
    expect_error(bracket_median(q_fun = qnorm, params = list(a = 1)))
})

test_that("bracket_median returns corret result", {

    rslt_3 <- list(list(prob = 1 / 3,
                        point = qnorm(1 / 6)),
                   list(prob = 1 / 3,
                        point = qnorm(3 / 6)),
                   list(prob = 1 / 3,
                        point = qnorm(5 / 6)))
    m <- 3; s <- 5
    rslt_4 <- list(list(prob = 1 / 4,
                        point = qnorm(1 / 8, m, s)),
                   list(prob = 1/4,
                        point = qnorm(3 / 8, m, s)),
                   list(prob = 1/4,
                        point = qnorm(5 / 8, m, s)),
                   list(prob = 1/4,
                        point = qnorm(7 / 8, m, s)))
    rslt_p <- list(list(prob = 1/2,
                        point = qnorm(1 / 4, m, s)),
                   list(prob = 1/3,
                        point = qnorm(4 / 6, m, s)),
                   list(prob = 1/6,
                        point = qnorm(11 / 12, m, s)))

    expect_equal(bracket_median(q_fun = qnorm), rslt_3)
    expect_equal(bracket_median(n = 4, q_fun = qnorm,
                                params = list(mean = m, sd = s)), rslt_4)
    expect_equal(bracket_median(p = c(1 / 2, 1 / 3, 1 / 6), q_fun = qnorm,
                                params = list(mean = m, sd = s)),
                 rslt_p)

})
