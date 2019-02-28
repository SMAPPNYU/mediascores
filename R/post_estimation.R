# Check if requested parameters are available in posterior object
#
# See `poit_est` and `rhat` for argument documentation.
# @return Returns list of all valid requested parameters
check_parameters <- function(posterior, pars) {
  assert_class(posterior, 'stanfit')

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

#' Provide a point estimate for each parameter of interest and its
#' credible interval
#'
#' [TODO: EXTENDED HEADER]
#'
#' @import checkmate
#'
#' @param posterior, object of class \code{rstan::\link[rstan]{stanfit}}.
#' @param pars character vector specifying parameters to extract point
#'  estimates for. Groups of parameters can be extracted by specifying the
#'  parameter name without the index value. E.g. \code{"theta"} produces point
#'  estimates for all \code{theta} parameter (i.e. \code{"theta[1]",
#'  "theta[2]", ect.}). See \code{\link{mediascores}} for model details and run
#'     \code{names(posterior)} to see all available parameters in the model).
#' @param prob [TODO: FILL IN].
#'
#' @return
#' Returns a three-columm matrix containing the median of the posterior of each
#' paramter of interest and a credible interval.
#'
#' @examples
#' \dontrun{
#' simulated_data <- simulate_data(5, 20)
#' posterior <- mediascores(Y = simulated_data$Y, group = simulated_data$group,
#'                          anchors = simulated_data$anchors,
#'                          variational = FALSE, chains = 1, iter = 500)
#' point_est(posterior, pars = c("theta", "theta_mu"), prob = 0.90)
#' }
#' @export
point_est <- function(posterior, pars, prob = 0.90) {
  qassert(prob, 'R1[0,1]')
  keep_pars <- check_parameters(posterior, pars)

  X <- as.matrix(posterior)[, keep_pars]
  if(class(X) != "matrix") X <- matrix(X, dimnames = list(NULL, keep_pars))
  alpha <- (1 - prob) / 2
  probs <- c(alpha, 1 - alpha)
  labels <- c("median", paste0(100 * probs, "%"))
  out <- t(apply(X, 2, stats::quantile, probs = c(0.5, probs)))
  return(structure(out, dimnames = list(colnames(X), labels)))
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
rhat <- function(posterior, pars) {

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




