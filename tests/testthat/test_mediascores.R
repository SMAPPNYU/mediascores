context("Mediascores Estimation")
library(mediascores)
seed <- 444555
set.seed(seed)
d <- simulate_data(n_user = 10, n_domain = 10)
out <- mediascores(Y = d$Y, group = d$group, anchors = d$anchors,
                   chains = 1, cores = 1, iter = 1000, warmup = 100,
                   seed = seed)
test_that("point_est returns correctly and params returned by mediascores are
           dimension right", {
    pe <- point_est(out)
    expect_equal(dim(pe), c(61, 3))
})

test_that("mediascores function produces sufficent correlation of estimates
          with true parameters", {
    pe <- point_est(out, pars = 'theta')
    # Check if correlation is at leat 0.9
    expect_equal(cor(pe[,1], d$parameters$theta), 1,
                 tolerance = 0.1)
})

test_that("rhat works", {
    rhat = rhat(out)
    expect_equal(dim(rhat), c(61, 1))
})
