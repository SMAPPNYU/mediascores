# Check if requested parameters are available in posterior object
#
# See \code{point_est} and \code{rhat} for argument documentation.
#
# Returns a list of all valid requested parameters. If pars is \code{NULL}
# it returns all parameters.
check_parameters <- function(posterior, pars) {
  assert_class(posterior, 'stanfit')
  if(is.null(pars)) {
    pars <- names(posterior)
  }

  # Check if all parameter names passed in pars are available in the model
  # and extract the available parameters
  pars_grep <- gsub("\\[", "\\\\[", pars)
  pars_grep <- gsub("\\]", "\\\\]", pars_grep)
  keep_pars <- sapply(pars_grep, function(x) {
      grep(paste0("^", x, "$|^", x, "\\["), names(posterior), value = TRUE)
  })
  for(name in names(keep_pars)) {
    if(length(keep_pars[[name]]) == 0) {
      warning(paste0('Requested parameter "', name, '" not in posterior.',
                     ' This parameter will be ignored. Use `names(posterior)`',
                     ' to see all available parameters.'))
    }
  }
  keep_pars <- unlist(keep_pars)
  if(length(keep_pars) == 0) {
    stop('No valid parameters requested.')
  }
  return(keep_pars)
}

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
#' simulated_data <- simulate_data(5, 20)
#' posterior <- mediascores(Y = simulated_data$Y, group = simulated_data$group,
#'                          anchors = simulated_data$anchors,
#'                          variational = FALSE, chains = 1, iter = 500)
#' point_est(posterior, pars = c("theta", "theta_mu"), prob = 0.9)
#' }
#' @export
point_est <- function(posterior, pars = NULL, prob = 0.9) {
  stopifnot(prob > 0 & prob < 1)
  keep_pars <- check_parameters(posterior, pars)

  X <- as.matrix(posterior)[, keep_pars]
  if(all(class(X) != "matrix")) X <- matrix(X, dimnames = list(NULL, keep_pars))
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  labels <- c("median", paste0(100 * probs, "%"))
  out <- t(apply(X, 2, stats::quantile, probs = c(0.5, probs)))
  return(structure(out, dimnames = list(colnames(X), labels)))
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
rhat <- function(posterior, pars = NULL) {

  if(posterior@stan_args[[1]]$method != "sampling") {
    stop("R-hat values are not available when the model is fit using variational inference (i.e. mediascores(..., variational = TRUE).")
  }
  keep_pars <- check_parameters(posterior, pars)

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

