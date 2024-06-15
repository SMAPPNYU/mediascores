#' Simulate data from the model's data-generating process
#' 
#' \code{simulate_data} simulates data according to the data-generating process
#' defined by the model.
#' 
#' @section Model details:
#' 
#' The data-generating process is defined as the following:
#' 
#' \deqn{NegBin(\pi_{img}, \omega_i\omega_m)}
#' \deqn{\pi_{img} = \alpha_i + \gamma_m - ||\vartheta_i - \zeta_m||^2,}
#' 
#' where \eqn{\alpha_i} denotes a user-level intercept; \eqn{\gamma_m}, a news
#' media domain intercept; \eqn{\vartheta_i} the sharing-ideology of user
#' \eqn{i}; \eqn{zeta_m} the ideology of news media domain \eqn{m}; and
#' \eqn{\omega_i} and \eqn{\omega_m}, user- and domain-level variance parameters.
#' Further details regarding can be found in the library's vignette.
#' 
#' @param n_users, int, number of users in the data
#' @param n_domains, int, number of domains that users can share
#' @param n_groups, int, number of groups users can belong to
#' @param group_prob, numeric vector of length \code{n_groups} specifying the 
#'     probability of a user belonging to each of the groups
#' @param user_variance, logical, whether to fit the model with an additional 
#'     variance parameter per user (i.e. omega_user)
#' @param params, list containing complete (or partial) list of parameter values
#'     of the model. See Details section for more information.
#' 
#' @return 
#' Returns a list with elements:
#' \itemize{
#'     \item \code{'Y'}: A matrix (n_users x n_domains) of simulated counts of
#'         times each user shared a domain
#'     \item \code{'group'}: Vector of length \code{n_users} indicating group
#'         membership of each user
#'     \item \code{'anchors'}: Index position (column in \code{'shares_data'})
#'         with the most extreme domains (minimum, maximum on latent scale)
#'     \item \code{'parameters'}: Simulated parameters:
#'     \itemize{
#'         \item \code{'alpha'}: Numeric vector of length \code{n_users}, 
#'             user-level intercepts
#'         \item \code{'gamma'}: Numeric vector of length \code{n_domains}, 
#'             domain-level intercepts
#'         \item \code{'theta'}: Numeric vector of length \code{n_users},
#'             ideology of each user
#'         \item \code{'zeta'}: Numeric vector of length \code{n_domains},
#'             ideology of each domain
#'         \item \code{'omega_domain'}: Numeric vector of length \code{n_domains},
#'             variance parameter for domain
#'         \item \code{'omega_user'}: Numeric vector of length \code{n_users},
#'             variance parameter for each user
#'     }
#' }
#' 
#' @examples
#' simulated_data <- simulate_data() 
#' 
#' @export
simulate_data <- function(n_users = 200, 
                          n_domains = 500, 
                          n_groups = 2, 
                          group_prob = c(0.4, 0.6),
                          user_variance = FALSE,
                          params = list('alpha_mu' = 2, 'alpha_sigma' = 0.5,
                                        'gamma_sigma' = 0.75, 
                                        'theta_mu' = c(-0.75, 0.5),
                                        'theta_sigma' = c(0.25, 0.25),
                                        'omega_user_shape' = 5,
                                        'omega_user_rate' = 5,
                                        'omega_domain_shape' = 5,
                                        'omega_domain_rate' = 5)) {

    default_params <- list('alpha_mu' = 2, 'alpha_sigma' = 0.5,
                           'gamma_sigma' = 0.75, 
                           'theta_mu' = c(-0.75, 0.5),
                           'theta_sigma' = c(0.25, 0.25),
                           'omega_user_shape' = 5,
                           'omega_user_rate' = 5,
                           'omega_domain_shape' = 5,
                           'omega_domain_rate' = 5)

    # If user provides a partial list of parameters, fill
    # remaining with defaults
    if (any(!names(default_params) %in% params)) {
      params <- c(params,
                  default_params[!names(default_params) %in% names(params)])
    }

    # Draw group membership of each user
    group <- sample(1:n_groups, n_users, replace = TRUE, prob = group_prob)
    
    # Draw user-level intercept
    alpha <- stats::rnorm(n_users, mean = params$alpha_mu, sd = params$alpha_sigma)
    
    # Draw domain-level intercept
    gamma <- stats::rnorm(n_domains, mean = 0, sd = params$gamma_sigma)
    
    # Draw ideology of each user conditional on group membership
    theta = stats::rnorm(n = n_users, mean = params$theta_mu[group], 
                  sd = params$theta_sigma[group])
    
    # Draw ideology of each news domain
    zeta <- stats::rnorm(n_domains, mean = 0, sd = 1)
    
    # To define the polarization of the scale, for simulation, we'll define the 
    # anchors as the domains with the lowest and highest theta
    anchors <- c(which.min(zeta), which.max(zeta))
    
    # omega_domain represent the variance of the model (user-level)
    if (user_variance) {
      omega_user <- invgamma::rinvgamma(n_users, params$omega_user_shape, 
                              params$omega_user_rate)
    } else {
      omega_user <- rep(1, n_users)
    }
    
    # omega_domain represent the variance of the model (domain-level)
    omega_domain <- invgamma::rinvgamma(n_domains, params$omega_domain_shape, 
                                        params$omega_domain_rate)

    # Compute the values of the user-domain count matrix: the count of each domain
    # shared by each user
    Y = do.call(rbind, lapply(1:n_users, function(i) {
       MASS::rnegbin(n = n_domains, 
                     mu = exp(alpha[i] + gamma - ((theta[i] - zeta)^2)), 
                     theta = omega_user[i]*omega_domain)
    })) 
    
    return(list("Y" = Y, "group" = group, 
                "anchors" = anchors, 
                'parameters' = list('alpha' = alpha, 'gamma' = gamma, 
                                    'theta' = theta, 'zeta' = zeta,
                                    'omega_domain' = omega_domain,
                                    'omega_user' = omega_user)))
}