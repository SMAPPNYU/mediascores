#' Calculate point estimates and credible intervals for parameters of interest
#' 
#' \code{point_est} is used to calculate point estimates and credible intervals
#' for the parameters of interest to the user.
#' 
#' @section Details:
#' 
#' \code{point_est} is a helper function for users to quickly caluclate the
#' median of the posterior for parameters of interest, and credible intervals
#' for those parameters with quantiles defined by the user. These estimates can
#' be calculated for the following parameters inputted (alone or in combination)
#' by the user: \code{"theta", "theta_mu", "theta_sigma", "zeta", "alpha",
#' "alpha_mu", "alpha_sigma", "gamma", "gamma_sigma", "omega_domain",
#' "omega_user"}. Indexed variants of these parameter names can also be
#' retrieved (e.g. \code{c("theta[4]", "zeta[1]")}.
#' 
#' @param posterior object of class \code{rstan::\link[rstan]{stanfit}} as fit
#'      by the \code{\link{mediascores}} function
#' @param pars character vector, a vector of model parameter names e.g.
#'      \code{c("theta", "zeta[1]", "alpha")}. See Details below.
#' @param prob numeric, a number between 0 and 1 indicating the
#'      probability mass to include in the credible interval. Defaults to 0.9.
#'     
#' @return
#' Returns a three-columm matrix containing the median of the posterior of each
#' parameter of interest and the lower and upper quantile to form a credible
#' interval around the median.
#' 
#' @examples
#' \dontrun{
#' simulated_data <- simulate_data()
#' posterior <- mediascores(Y = simulated_data$Y, group = simulated_data$group, 
#'                          anchors = simulated_data$anchors, 
#'                          variational = TRUE)
#' point_est(posterior, pars = c("theta", "theta_mu"), prob = 0.90)
#' }
#' @export
point_est <- function(posterior, 
                      pars = c("theta", "theta_mu", "theta_sigma", "zeta",
                               "alpha", "alpha_mu", "alpha_sigma", "gamma", 
                               "gamma_sigma", "omega_domain", "omega_user"),
                      prob = 0.90) {
  if (length(prob) != 1L || prob <= 0 || prob >= 1)
    stop("The argument 'prob' should be a number between 0 and 1")

  pars_grep <- gsub("\\[", "\\\\[", pars)
  pars_grep <- gsub("\\]", "\\\\]", pars_grep)
  keep_pars <- unlist(sapply(pars_grep, function(x) {
      grep(paste0("^", x, "$|^", x, "\\["), names(posterior), value = TRUE)
  }))

  X <- as.matrix(posterior)[, keep_pars]
  if(class(X) != "matrix") X <- matrix(X, dimnames = list(NULL, keep_pars))
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  labels <- c("median", paste0(100 * probs, "%"))
  out <- t(apply(X, 2, stats::quantile, probs = c(0.5, probs)))
  structure(out, dimnames = list(colnames(X), labels))
}


#' Display R-hat values for parameters of interest
#' 
#' \code{rhat} calculates R-hat statistics for each specifide parameter of
#' interest.
#' 
#' @section Details:
#' 
#' \code{rhat} is a helper function for users to quickly caluclate a common
#' convergence statistic, R-hat, for each parameter. These statistics can
#' be calculated for the following parameters inputted (alone or in combination)
#' by the user: \code{"theta", "theta_mu", "theta_sigma", "zeta", "alpha",
#' "alpha_mu", "alpha_sigma", "gamma", "gamma_sigma", "omega_domain",
#' "omega_user"}. Indexed variants of these parameter names can also be
#' retrieved (e.g. \code{c("theta[4]", "zeta[1]")}.
#'
#' @param posterior, object of class \code{rstan::\link[rstan]{stanfit}}.
#' @param pars character vector, a vector of model parameter names e.g.
#'      \code{c("theta", "zeta[1]", "alpha")}. See Details below.
#'     
#' @return
#' Returns a named vector of R-hat values for the parameters specified by the
#' \code{pars} argument.
#' 
#' @examples
#' \dontrun{
#' simulated_data <- simulate_data()
#' posterior <- mediascores(Y = simulated_data$Y, group = simulated_data$group, 
#'                          anchors = simulated_data$anchors, 
#'                          variational = TRUE)
#' rhat(posterior, pars = c("theta", "zeta"))
#' }
#' @export
rhat <- function(posterior,
                 pars = c("theta", "theta_mu", "theta_sigma",
                          "zeta", "zeta_sigma",
                          "alpha", "alpha_mu", "alpha_sigma",
                          "gamma", "gamma_sigma",
                          "omega_domain", "omega_user")) {

  pars_grep <- gsub("\\[", "\\\\[", pars)
  pars_grep <- gsub("\\]", "\\\\]", pars_grep)
  keep_pars <- unlist(sapply(pars_grep, function(x) {
      grep(paste0("^", x, "$|^", x, "\\["), names(posterior), value = TRUE, 
           perl = TRUE)
  }))

  out <- data.frame(
      rhat = rstan::summary(posterior, keep_pars)$summary[, "Rhat"])
  if (nrow(out) == 1) rownames(out) <- keep_pars

  large_rhat <- data.frame(table(gsub("\\[.*", "", 
                                      rownames(out)[which(out > 1.1)])))

  # Warning message
  if (nrow(large_rhat) > 0) {
    warning_message <- ""
    for(i in 1:nrow(large_rhat)) {
      warning_message <- paste0(warning_message, large_rhat[i, 2], 
                                " Rhat values of ", large_rhat[i, 1], 
                                " are greater than 1.1")
      if(i < nrow(large_rhat)) warning_message <- paste0(warning_message, 
                                                         "\n  ")
    }
    warning(warning_message)
  }

  return(out)

}




