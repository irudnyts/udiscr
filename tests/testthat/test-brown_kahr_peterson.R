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
    # uniform
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

test_that("brown_kahr_peterson_5 checks argument params on validity corretly", {
    expect_silent(brown_kahr_peterson_5(q_fun = qunif, params = list()))
    expect_silent(brown_kahr_peterson_5(q_fun = qunif))
    expect_silent(brown_kahr_peterson_5(q_fun = qbeta, params = list(shape1 = 4,
                                                                     shape2 = 3)))
    expect_error(brown_kahr_peterson_5(q_fun = qbeta, params = list(a = 1)))
    expect_error(brown_kahr_peterson_5(q_fun = qnorm))
    expect_error(brown_kahr_peterson_5(q_fun = qexp))
})

test_that("brown_kahr_peterson_5 returns corret result", {
    # uniform
    min_ <- 2; max_ <- 10
    q_unif <- function(x) qunif(x, min = min_, max = max_)

    v2 <- q_unif(0.25)
    p2 <- 0.5 * (q_unif(0.5) - q_unif(0.25)) / (q_unif(0.25) - q_unif(0))
    v4 <- q_unif(0.75)
    p4 <- 0.5 * (q_unif(0.75) - q_unif(0.5)) / (q_unif(1) - q_unif(0.75))
    v1 <- q_unif(0.25) - 0.25 * (1 + 2 * p2) * (q_unif(0.5) - q_unif(0))
    p1 <- 0.25 * (1 - 2 * p2)
    v5 <- q_unif(0.75) + 0.25 * (1 + 2 * p4) * (q_unif(1) - q_unif(0.5))
    p5 <- 0.25 * (1 - 2 * p4)
    s <- 0.5 * (q_unif(0.25) + q_unif(0.5)) + 0.5 * p2 * (q_unif(0.5) - q_unif(0))
    t <- 0.5 * (q_unif(0.5) + q_unif(0.75)) + 0.5 * p4 * (q_unif(1) - q_unif(0.5))
    p3 <- p1 + p5; v3 <- (s * p1 + t * p5) / p3

    rslt_1 <- list(list(prob = p1, point = v1),
                   list(prob = p2, point = v2),
                   list(prob = p3, point = v3),
                   list(prob = p4, point = v4),
                   list(prob = p5, point = v5))
    # beta
    shape1 <- 3; shape2 <- 5
    q_beta <- function(x) qbeta(x, shape1 = shape1, shape2 = shape2)

    v2 <- q_beta(0.25)
    p2 <- 0.5 * (q_beta(0.5) - q_beta(0.25)) / (q_beta(0.25) - q_beta(0))
    v4 <- q_beta(0.75)
    p4 <- 0.5 * (q_beta(0.75) - q_beta(0.5)) / (q_beta(1) - q_beta(0.75))
    v1 <- q_beta(0.25) - 0.25 * (1 + 2 * p2) * (q_beta(0.5) - q_beta(0))
    p1 <- 0.25 * (1 - 2 * p2)
    v5 <- q_beta(0.75) + 0.25 * (1 + 2 * p4) * (q_beta(1) - q_beta(0.5))
    p5 <- 0.25 * (1 - 2 * p4)
    s <- 0.5 * (q_beta(0.25) + q_beta(0.5)) + 0.5 * p2 * (q_beta(0.5) - q_beta(0))
    t <- 0.5 * (q_beta(0.5) + q_beta(0.75)) + 0.5 * p4 * (q_beta(1) - q_beta(0.5))
    p3 <- p1 + p5; v3 <- (s * p1 + t * p5) / p3

    rslt_2 <- list(list(prob = p1, point = v1),
                   list(prob = p2, point = v2),
                   list(prob = p3, point = v3),
                   list(prob = p4, point = v4),
                   list(prob = p5, point = v5))


    expect_equal(brown_kahr_peterson_5(q_fun = qunif,
                                       list(min = min_, max = max_)),
                 rslt_1)
    expect_equal(brown_kahr_peterson_5(q_fun = qbeta,
                                       params = list(shape1 = shape1,
                                                     shape2 = shape2)),
                 rslt_2)

})
