context("Example test: bayesian linear regression")
library(mediascores)

test_that("linear regression runs without errors", {
    set.seed(3321)
    n = 100
    X = cbind(1, rnorm(n), rnorm(n))
    beta = c(0, 1, 2)
    sigma = 0.01
    y = as.numeric(X%*%beta + rnorm(n, 0, sigma))
    out = blm(y, X)   
    expect_is(out, 'stanfit')
})