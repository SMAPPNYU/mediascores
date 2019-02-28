#' Estimate the mediascores model
#' 
#' This function is the workhorse of the mediascores library. It fits the model
#' of news media sharing as described in more detail in the library vignette and
#' in the "Model Details" section further below.
#' 
#' @section Model details:
#' 
#' The model fit by the \code{mediascores()} function is a negative binomial
#' item-response-style model of the following form:
#' 
#' \deqn{NegBin(\pi_{img}, \omega_i\omega_m)}
#' \deqn{\pi_{img} = \alpha_i + \gamma_m - ||\vartheta_i - \zeta_m||^2,}
#' 
#' where \eqn{\alpha_i} denotes a user-level intercept; \eqn{\gamma_m}, a news
#' media domain intercept; \eqn{\vartheta_i} the sharing-ideology of user
#' \eqn{i}; \eqn{zeta_m} the ideology of news media domain \eqn{m}; and
#' \eqn{\omega_i} and \eqn{\omega_m}, user- and domain-level variance paramters.
#' Details regarding the priors for these parameters are discussed in the
#' library's vignette. A group-specific common prior distribution can be placed
#' on users' news-sharing ideology parameters \eqn{\vartheta_i} through the
#' integer-valued \code{group} argument (a vector of integers specifying the
#' affiliation of each user).
#' 
#' @import checkmate
#' 
#' @param Y matrix or dataframe of dimension \code{n_users x n_domains}
#'      containing counts of the frequency with which each user (row) shares a
#'      given URL domain (column). No missing data are permitted.
#' @param group vector of length \code{n_users} indicating group membership of 
#'     each user. If \code{NULL}, every user is assigned to the same group.
#' @param anchors vector of length 2 indicating the index/column position of the 
#'     anchor domains. The first index defines the meaning of lower values of
#'     the scale; the second, upper values. For example, setting the first
#'     value to indicate the column representing the New York Times and the
#'     second value to represent the FOX News column would render lower values
#'     of \eqn{\vartheta_i} and \eqn{\zeta_m} to indicate liberalism; higher
#'     values, conservativism.
#' @param user_variance logical, whether to include a variance parameter for
#'     each user (i.e. whether to include or exclude \eqn{\omega_i}. Setting
#'     this argument to \code{TRUE} results in a more computationally demanding
#'     model. There are typically too few data to identify these additional
#'     parameters.
#' @param variational logical, whether variational inference is used for
#'     estimation (\code{rstan::\link[rstan]{vb}}). If set to \code{FALSE},
#'     the standard NUTS sampler in Stan is used
#'     (\code{rstan::\link[rstan]{sampling}}). Note: variational Bayes is many
#'     orders of magnitude faster, but it is recommended that its estimates be
#'     used for final inference unless the size of the data make use of sampling
#'     infeasible.
#' @param chains integer, the number of Markov chains to run (for
#'     \code{variational = FALSE}. The default is 4.
#' @param cores integer, the number of cores to use when running chains in
#'     parallel.
#' @param threads integer, the number of total threads to use for
#'      within-chain parallelization. Defaults to the value of \code{cores}.
#' @param iter integer, the number of total iterations per chain.
#'     Defaults to 2000.
#' @param warmup integer, the number of warmup/burnin iterations per chain.
#'     Defaults to \code{floor(iter/2)}.
#' @param refresh integer, the number of iterations per chain before sampling
#'     progress on each chain is displayed to the user.
#' @param ... additional arguments passed to \code{rstan::\link[rstan]{sampling}}
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
                        iter = 2000, warmup = floor(iter/2), refresh = 50,
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
  qassert(warmup, 'X1')
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
