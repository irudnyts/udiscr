context("Brown, Kahr, and Peterson methods")

test_that("brown_kahr_peterson_3 checks argument params on validity corretly", {
    expect_silent(brown_kahr_peterson_3(q_fun = qunif, params = list()))
    expect_silent(brown_kahr_peterson_3(q_fun = qunif))
    expect_silent(brown_kahr_peterson_3(q_fun = qbeta, params = list(shape1 = 4,
                                                                     shape2 = 3)))
    expect_error(brown_kahr_peterson_3(q_fun = qbeta, params = list(a = 1)))
    expect_error(brown_kahr_peterson_3(q_fun = qnorm))
    expect_error(brown_kahr_peterson_3(q_fun = qexp))
})

test_that("brown_kahr_peterson_3 returns corret result", {
    # exponential
    rslt_3 <- list(list(prob = 0.25,
                        point = (3 * qunif(0) + 5 * qunif(0.5)) / 8),
                   list(prob = 0.5,
                        point = (qunif(0) + 14 * qunif(0.5) + qunif(1)) / 16),
                   list(prob = 0.25,
                        point = (5 * qunif(0.5) + 3 * qunif(1)) / 8))
    # beta
    shape1 <- 3; shape2 <- 5
    rslt_4 <- list(list(prob = 0.25,
                        point = (3 * qbeta(0, shape1 = shape1, shape2 = shape2)
                                 + 5 * qbeta(0.5, shape1 = shape1, shape2 = shape2)) / 8),
                   list(prob = 0.5,
                        point = (qbeta(0, shape1 = shape1, shape2 = shape2) +
                                     14 * qbeta(0.5, shape1 = shape1, shape2 = shape2) +
                                     qbeta(1, shape1 = shape1, shape2 = shape2)) / 16),
                   list(prob = 0.25,
                        point = (5 * qbeta(0.5, shape1 = shape1, shape2 = shape2) +
                                     3 * qbeta(1, shape1 = shape1, shape2 = shape2)) / 8))

    expect_equal(brown_kahr_peterson_3(q_fun = qunif), rslt_3)
    expect_equal(brown_kahr_peterson_3(q_fun = qbeta, params = list(shape1 = shape1,
                                                                    shape2 = shape2)),
                 rslt_4)

})
