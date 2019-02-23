context("Mediascores Estimation")
library(mediascores)

test_that("mediascores function reproduces simulated parameters", {
    set.seed(444555)
    d <- simulate_data(n_user = 10, n_domain = 20)
    out <- mediascores(Y = d$Y, group = d$group, anchors = d$anchors, 
                       chains = 1, iter = 500, warmup = 100)
    expect_is(out, 'stanfit')
})