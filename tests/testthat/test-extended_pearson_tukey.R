context("Extended Pearson-Tukey method")

test_that("extended_pearson_tukey checks argument params on validity corretly", {
    expect_silent(extended_pearson_tukey(q_fun = qnorm, params = list()))
    expect_silent(extended_pearson_tukey(q_fun = qnorm))
    expect_silent(extended_pearson_tukey(q_fun = qnorm, params = list(mean = 1,
                                                                      sd = 2)))
    expect_error(extended_pearson_tukey(q_fun = qnorm, params = list(a = 1)))
})

test_that("extended_pearson_tukey returns corret result", {
    # exponential
    rslt_3 <- list(list(prob = 0.185,
                        point = qexp(0.05)),
                   list(prob = 0.630,
                        point = qexp(0.5)),
                   list(prob = 0.185,
                        point = qexp(0.95)))
    # normal
    m <- 3; s <- 5
    rslt_4 <- list(list(prob = 0.185,
                        point = qnorm(0.05, mean = m, sd = s)),
                   list(prob = 0.630,
                        point = qnorm(0.5, mean = m, sd = s)),
                   list(prob = 0.185,
                        point = qnorm(0.95, mean = m, sd = s)))

    expect_equal(extended_pearson_tukey(q_fun = qexp), rslt_3)
    expect_equal(extended_pearson_tukey(q_fun = qnorm, params = list(mean = m,
                                                                     sd = s)),
                 rslt_4)

})
