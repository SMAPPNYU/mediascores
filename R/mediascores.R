#' Estimate the mediascores model
#' 
#' [TODO: EXTENDED HEADER FILL IN]
#' 
#' [TODO: FILL IN MODEL DETAILS HERE]
#' 
#' @param Y matrix or dataframe of dimension (n_user x n_domains) containing 
#'     counts of how often each user (row) shared a given domain (column). No 
#'     missing data is permitted.
#' @param group vector of length \code{n_users} indicating group membership of 
#'    each user. If \code{NULL} every user is assigned to the same group.
#' @param anchors vector of length 2 indicating index/column position of the 
#'     anchor domains
#' @param variational logical, should variational inference be used 
#'     (\code{rstan::\link[rstan]{vb}}). If set to \code{FALSE} exact sampling 
#'     (\code{rstan::\link[rstan]{sampling}}) is used.
#' @param adapt_delta [TODO: FILL IN]
#' @param ... arguments passed to \code{rstan::\link[rstan]{sampling}}
#'     (for \code{variational = TRUE}) or \code{rstan::\link[rstan]{vb}} 
#'     (for \code{variational = FALSE})
#' 
#' @return 
#' An object of S4 class stanfit (see \code{\link[rstan]{stanfit-class}}) 
#' representing the fitted results.
#' 
#' @examples
#' \dontrun{
#' sim_data <- simulate_data(200, 500)
#' posterior <- mediascores(sim_data$Y, sim_data$group, sim_data$anchors, 
#'                          variational = FALSE, chains = 2)
#' }
#' @export
mediascores <- function(Y, group = NULL, anchors, variational = FALSE,
                        adapt_delta = 0.8, ...) {

  n_row <- nrow(Y)
  n_col <- ncol(Y)
  n_groups <- 1
  if (!is.null(group)) {
    n_groups <- length(unique(group))
  } else {
    group <- rep(1, n_row)
    n_groups <- 1
  }

  # N = number of users; M = number of domains; G = number of groups;
  # group = group each user belongs to; Y = user-domain count matrix
  model_data <- list(N = n_row, M = n_col, G = n_groups,
                     group = group, Y = Y, anchors = anchors)

  if (variational) {
    posterior <- rstan::vb(stanmodels$mediascores, data = model_data, ...)
  } else {
    posterior <- rstan::sampling(stanmodels$mediascores, data = model_data, ...)
  }

  return(posterior)
}
