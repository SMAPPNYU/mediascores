#' Fit a bayesian linear regression
#' @param data, a list with the data in stan format
#' 
#' @examples 
#' 
#' n = 100
#' X = cbind(rnorm(n), rnorm(n))
#' beta = c(1, 2)
#' sigma = 0.01
#' y = X%*%beta + rnorm(n, 0, sigma)
#' 
#' @export
blm = function(y, X) {
    n = nrow(X)
    k = ncol(X)
    data = list(n = n, k = k, y = y, X = X)
    return(rstan::sampling(stanmodels$linear_regression, data))
}