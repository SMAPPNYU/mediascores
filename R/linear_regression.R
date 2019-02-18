#' Fit a bayesian linear regression
#' @param y numeric, vector of outcomes
#' @param X matrix, matrix of predictors (1st col intercept)
#' 
#' @examples 
#' 
#' n = 100
#' X = cbind(1, rnorm(n), rnorm(n))
#' beta = c(0, 1, 2)
#' sigma = 0.01
#' y = as.numeric(X%*%beta + rnorm(n, 0, sigma))
#' blm(y, X) 
#' @export
blm = function(y, X) {
    n = nrow(X)
    k = ncol(X)
    data = list(n = n, k = k, y = y, X = X)
    return(rstan::sampling(stanmodels$linear_regression, data))
}