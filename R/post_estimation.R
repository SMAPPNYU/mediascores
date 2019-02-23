#' Simulate data according to the media score model's data generating process
#' 
#' [TODO: EXTENDED HEADER]
#' 
#' [TODO: THIS IS THE DETAILS SECTION: FILL IN INFORMATION ON THE MODEL PARAMERS] 
#' 
#' @param n_user, int, number of users in the data
#'     
#' @return
#' Returns a list with elements:
#' 
#' @examples
#' simulated_data <- simulate_data() 
#' 
#' @export
point_est <- function(object, pars = c("theta", "theta_mu", "theta_sigma",
                                       "zeta",
                                       "alpha", "alpha_mu", "alpha_sigma",
                                       "gamma", "gamma_sigma",
                                       "omega_domain", "omega_user"),
               prob = 0.90) {
  if (length(prob) != 1L || prob <= 0 || prob >= 1)
    stop("The argument 'prob' should be a number between 0 and 1")

  pars_grep <- gsub("\\[", "\\\\[", pars)
  pars_grep <- gsub("\\]", "\\\\]", pars_grep)
  keep_pars <- unlist(sapply(pars_grep, function(x) grep(paste0("^", x, "$|^", x, "\\["), names(posterior), value = TRUE)))

  X <- as.matrix(object)[, keep_pars]
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  labels <- paste0(100 * c(0.5, probs), "%")
  out <- t(apply(X, 2, quantile, probs = c(0.5, probs)))
  structure(out, dimnames = list(colnames(X), labels))
}
