context("Mediascores Estimation")
library(mediascores)
seed <- 444555
set.seed(seed)
d <- simulate_data(n_user = 10, n_domain = 10)
out <- mediascores(Y = d$Y, group = d$group, anchors = d$anchors, 
                   chains = 1, cores = 1, iter = 1000, warmup = 100,
                   seed = seed)

test_that("mediascores function produced a valid stanfit object", {
    expect_equal(median(unlist(rstan::extract(out, 'theta[1]'))), 0.283313,
                 tolerance = 10e-5)
})

test_that("point_est works", {
    theta_1_median = median(unlist(rstan::extract(out, 'theta[1]')))
    expect_equal(theta_1_median, 0.28331,
                 tolerance = 10e-5)
})

