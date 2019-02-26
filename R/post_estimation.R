#' Provide a point estimate for each parameter of interest and its
#' credible interval
#' 
#' [TODO: EXTENDED HEADER]
#' 
#' [TODO: THIS IS THE DETAILS SECTION: FILL IN INFORMATION ON THE MODEL PARAMERS] 
#' 
#' @param posterior, object of class \code{rstan::\link[rstan]{stanfit}}.
#' @param pars [TODO: FILL IN].
#' @param prob [TODO: FILL IN].
#'     
#' @return
#' Returns a three-columm matrix containing the median of the posterior of each
#' paramter of interest and a credible interval
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


#' Display Rhat values for the parameters of interest
#' 
#' [TODO: EXTENDED HEADER]
#' 
#' [TODO: THIS IS THE DETAILS SECTION: FILL IN INFORMATION ON THE MODEL PARAMERS] 
#' 
#' @param posterior, object of class \code{rstan::\link[rstan]{stanfit}}.
#' @param pars [TODO: FILL IN].
#'     
#' @return
#' Returns a vector of rhat values for the parameters specified by the pars
#' argument
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
                 pars = c("theta", "theta_mu", "theta_sigma", "zeta",
                          "alpha", "alpha_mu", "alpha_sigma", "gamma", 
                          "gamma_sigma", "omega_domain", "omega_user")) {

  pars_grep <- gsub("\\[", "\\\\[", pars)
  pars_grep <- gsub("\\]", "\\\\]", pars_grep)
  keep_pars <- unlist(sapply(pars_grep, function(x) {
      grep(paste0("^", x, "$|^", x, "\\["), names(posterior), value = TRUE, perl = TRUE)
  }))

  out <- data.frame(rhat = rstan::summary(posterior, keep_pars)$summary[, "Rhat"])

  large_rhat <- data.frame(table(gsub("\\[.*", "", rownames(out)[which(out > 1.1)])))

  # Warning message
  if (nrow(large_rhat) > 0) {
    warning_message <- ""
    for(i in 1:nrow(large_rhat)) {
      warning_message <- paste0(warning_message, large_rhat[i, 2], " Rhat values of ", large_rhat[i, 1], " are greater than 1.1")
      if(i < nrow(large_rhat)) warning_message <- paste0(warning_message, "\n  ")
    }
    warning(warning_message)
  }

  return(out)

}




