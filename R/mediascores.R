#' Estimate the mediascores model
#' 
#' [TODO: EXTENDED HEADER FILL IN]
#' 
#' [TODO: FILL IN MODEL DETAILS HERE]
#' 
#' @import checkmate
#' 
#' @param Y matrix or dataframe of dimension (n_user x n_domains) containing 
#'      counts of how often each user (row) shared a given domain (column). No 
#'      missing data is permitted.
#' @param group vector of length \code{n_users} indicating group membership of 
#'     each user. If \code{NULL} every user is assigned to the same group.
#' @param anchors vector of length 2 indicating index/column position of the 
#'     anchor domains.
#' @param user_variance logical, whether to include a variance parameter for
#'     each user (omega_i). Note that doing so makes the model more
#'     computationally demanding and there is typically too few data to identify
#'     these parameters.
#' @param variational logical, should variational inference be used 
#'     (\code{rstan::\link[rstan]{vb}}). If set to \code{FALSE} exact sampling 
#'     (\code{rstan::\link[rstan]{sampling}}) is used.
#' @param chains integer, the number of Markov chains to run. The default is 4.
#' @param cores integer, the number of cores to use when running chains in
#'     parallel.
#' @param threads integer, the number of total threads to use when running
#'      within-chain parallelization. Defaults is no within-chain
#'      parallelization.
#' @param iter integer, the number of total iterations per chain.
#'     Defaults to 2000.
#' @param warmup integer, the number of warmup/burnin iterations per chain.
#'     Defaults to iter/2.
#' @param refresh integer, the number of iterations per chain before sampling
#'     progress on each chain is displayed.
#' @param ... arguments passed to \code{rstan::\link[rstan]{sampling}}
#'     (for \code{variational = TRUE}) or \code{rstan::\link[rstan]{vb}} 
#'     (for \code{variational = FALSE})
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
mediascores <- function(Y, group = NULL, anchors, user_variance = FALSE, 
                        variational = FALSE, chains = 4,
                        cores = getOption("mc.cores", 1L), threads = cores,
                        iter = 2000, warmup = iter/2, refresh = 50,
                        ...) {
    
  # Check user inputs
  assert(
    check_data_frame(Y, any.missing = FALSE, min.rows = 2,
                     min.cols = 2),
    check_matrix(Y, any.missing = FALSE, min.rows = 2,
                 min.cols = 2)
  )
  assert_integerish(group, any.missing = FALSE, len = nrow(Y),
                    null.ok = TRUE)
  qassert(anchors, 'X2')
  qassert(user_variance, 'B1')
  qassert(variational, 'B1')
  qassert(chains, 'X1')
  qassert(cores, 'X1')
  qassert(threads, 'X1')
  qassert(iter, 'X1')
  qassert(warmup, 'X1') # Can iter be float?
  qassert(refresh, 'X1')

  n_row <- nrow(Y)
  n_col <- ncol(Y)
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

  pars_to_include <- c("theta", "theta_mu", "theta_sigma",
                       "zeta", "zeta_sigma",
                       "alpha", "alpha_mu", "alpha_sigma",
                       "gamma", "gamma_sigma",
                       "omega_domain",
                       "omega_domain_a", "omega_domain_b")

  if(user_variance) pars_to_include <- c(pars_to_include,
                                         "omega_user",
                                         "omega_user_a", "omega_user_b")

  if (variational) {
    if(user_variance) {
      posterior <- rstan::vb(stanmodels$mediascores_vb,
                             data = model_data, include = TRUE,
                             pars = pars_to_include, 
                             ...)
    } else {
      posterior <- rstan::vb(stanmodels$mediascores_domain_vb,
                             data = model_data, include = TRUE,
                             pars = pars_to_include,
                             ...)
    }
  } else {

    Sys.setenv("STAN_NUM_THREADS" = threads)

    if(user_variance) {
      posterior <- rstan::sampling(stanmodels$mediascores,
                                   chains = chains, cores = cores,
                                   warmup = warmup, iter = iter,
                                   refresh = refresh,
                                   data = model_data, include = TRUE,
                                   pars = ,
                                   ...)
    } else {
      posterior <- rstan::sampling(stanmodels$mediascores_domain,
                                   chains = chains, cores = cores,
                                   warmup = warmup, iter = iter,
                                   refresh = refresh,
                                   data = model_data, include = TRUE,
                                   pars = pars_to_include,
                                   ...)
    }
  }

  return(posterior)
}
