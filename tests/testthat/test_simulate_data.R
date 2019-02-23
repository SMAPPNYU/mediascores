context("Data Simulation Function")
library(mediascores)

test_that("Simulate data runs without error", {
    set.seed(1123)
    d <- simulate_data()
    expect_equal(length(d), 4L)
})