context('McNamee-Celona "Srortcut" method')

test_that("mcnamee_celona checks argument params on validity corretly", {
    expect_silent(mcnamee_celona(q_fun = qnorm, params = list()))
    expect_silent(mcnamee_celona(q_fun = qnorm))
    expect_silent(mcnamee_celona(q_fun = qnorm, params = list(mean = 1,
                                                                       sd = 2)))
    expect_error(mcnamee_celona(q_fun = qnorm, params = list(a = 1)))
})

test_that("extended_swanson_megill returns corret result", {
    # exponential
    rslt_3 <- list(list(prob = 0.25,
                        point = qexp(0.1)),
                   list(prob = 0.5,
                        point = qexp(0.5)),
                   list(prob = 0.25,
                        point = qexp(0.9)))
    # normal
    m <- 3; s <- 5
    rslt_4 <- list(list(prob = 0.25,
                        point = qnorm(0.1, mean = m, sd = s)),
                   list(prob = 0.5,
                        point = qnorm(0.5, mean = m, sd = s)),
                   list(prob = 0.25,
                        point = qnorm(0.9, mean = m, sd = s)))

    expect_equal(mcnamee_celona(q_fun = qexp), rslt_3)
    expect_equal(mcnamee_celona(q_fun = qnorm, params = list(mean = m,
                                                                      sd = s)),
                 rslt_4)

})
