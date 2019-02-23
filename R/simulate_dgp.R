#' Simulate data according to the media score model's data generating process
#' 
#' [TODO: EXTENDED HEADER]
#' 
#' [TODO: THIS IS THE DETAILS SECTION: FILL IN INFORMATION ON THE MODEL PARAMERS] 
#' 
#' @param n_user, int, number of users in the data
#' @param n_domains, int, number of domains that users can share
#' @param n_groups, int, number of groups users can belong to
#' @param group_prob, numeric vector of length \code{n_groups} specifying the 
#'     probability of a user belonging to each of the groups
#' @param params, list containing parameters of the model distributions. See 
#'     Details section for more information [FILL IN]
#'     
#' @return
#' Returns a list with elements:
#' \itemize{
#'     \item \code{'shares_data'}: A matrix (n_users x n_domains) of simulated counts of
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
                          params = list('alpha_mu' = 2, 'alpha_sigma' = 0.5,
                                        'gamma_sigma' = 0.75, 
                                        'theta_mu' = c(-0.75, 0.5),
                                        'theta_sigma' = c(0.25, 0.25),
                                        'omega_user_shape' = 5,
                                        'omega_user_rate' = 5,
                                        'omega_domain_shape' = 5,
                                        'omega_domain_rate' = 5)) {
    
    # Draw group membership of each user
    group <- sample(1:n_groups, n_users, replace = TRUE, prob = group_prob)
    
    # Draw user-level intercept
    alpha <- rnorm(n_users, mean = params$alpha_mu, sd = params$alpha_sigma)
    
    # Draw domain-level intercept
    gamma <- rnorm(n_domains, mean = 0, sd = params$gamma_sigma)
    
    # Draw ideology of each user conditional on group membership
    theta = rnorm(n = n_users, mean = params$theta_mu[group], 
                  sd = params$theta_sigma[group])
    
    # Draw ideology of each news domain
    zeta <- rnorm(n_domains, mean = 0, sd = 1)
    
    # To define the polarization of the scale, for simulation, we'll define the 
    # anchors as the domains with the lowest and highest theta
    anchors <- c(which.min(zeta), which.max(zeta))
    
    # omega_user and omega_domain represent the variance of the model
    omega_user <- invgamma::rinvgamma(n_users, params$omega_user_shape, 
                            params$omega_user_rate)
    omega_domain <- invgamma::rinvgamma(n_domains, params$omega_domain_shape, 
                            params$omega_domain_rate)
    
    # Compute the values of the user-domain count matrix: the count of each domain
    # shared by each user
    shares_data = do.call(rbind, lapply(1:n_users, function(i) {
       MASS::rnegbin(n = n_domains, 
                     mu = exp(alpha[i] + gamma - ((theta[i] - zeta)^2)),
                     theta = omega_user[i]*omega_domain)
    })) 
    
    return(list("shares_data" = shares_data, "group" = group, 
                "anchors" = anchors, 
                'parameters' = list('alpha' = alpha, 'gamma' = gamma, 
                                    'theta' = theta, 'zeta' = zeta)))
}

### This will go into vignette:

#library(rstan)
#library(plyr)
#library(ggplot2)
#library(ggridges)
#library(gridExtra)
#library(stargazer)
#library(overlap)
#library(scales)
#library(parallel)
#library(matrixStats)
#library(MASS)
#library(invgamma)
#
###### SET WORKING DIRECTORY (IF ON PERSONAL COMPUTER OR HPC)
#if (grepl("gregoryeady", system("whoami", intern = TRUE))) {
#  setwd("/Users/GregoryEady/GitHub/mediascores")
#} else if (grepl("ge31", system("whoami", intern = TRUE))) {
#  setwd("/scratch/ge31/GitHub/mediascores")
#}
#
#n_cores <- 4
#options(mc.cores = n_cores)
#
#n_users <- 200
#n_domains <- 500
#
## Group that users might belong to (e.g. Republican, Democrat, ordinary user)
## Can input "1" for all users if all belong to same group or no grouping desired
#group <- rbinom(n_users, 1, 0.6) + 1
#
## alpha is the user-level intercept
## Represents the baseline degree to which a given user shares news URLs
## in general
#alpha_mu <- 2
#alpha_sigma <- 0.5
#alpha <- rnorm(n_users, mean = alpha_mu, sd = alpha_sigma)
#
#
## gamma is the domain-level intercept
## Represents the baseline degree to which a users shares news URLs from a given 
## news domain in general. Will be a large value for often-shared domains
## (e.g. nytimes.com) and low for rarely-shared domains (e.g. stripes.com)
## No gamma_mu because is unidentified (alpha_mu acts as the model intercept)
#gamma_sigma <- 0.75
#gamma <- rnorm(n_domains, mean = 0, sd = gamma_sigma)
#
## theta is the ideology of each user
## theta is given a separate hierarchical prior defined by a user's group
## i.e. the "group" variable defined above
#theta_mu <- c(-0.75, 0.5)
#theta_sigma <- c(0.25, 0.25)
#theta <- numeric(length(group))
#
#theta[group == 1] <- rnorm(sum(group == 1),
#                           mean = theta_mu[1], sd = theta_sigma[1])
#
#theta[group == 2] <- rnorm(sum(group == 2),
#                           mean = theta_mu[2], sd = theta_sigma[2])
#
#
## zeta is the ideology of each news domain
## Identification in the model is fixed by setting the prior on these parameters
## to Normal(0, 1)
#zeta_sigma <- 1
#zeta <- rnorm(n_domains, mean = 0, sd = 1)
#
## To define the polarization the scale (whether high values denote liberal or
## conservative), we assume that the research has in mind a well-known liberal
## domain and a well-known conservative domain.
## For simulation, we'll define the anchors as the domains with the lowest and
## highest theta
#anchors <- c(which(zeta == min(zeta)), which(zeta == max(zeta)))
#
## omega_user and omega_domain represent the variance of the model
#a_user <- 5
#b_user <- 5
#omega_user <- rinvgamma(n_users, a_user, b_user)
#
#a_domain <- 5
#b_domain <- 5
#omega_domain <- rinvgamma(n_domains, a_domain, b_domain)
#
## Compute the values of the user-domain count matrix: the count of each domain
## shared by each user
#Y <- matrix(data = 0, nrow = n_users, ncol = n_domains)
#
#for(i in 1:n_users) {
#    Y[i, ] <- rnegbin(n_domains, exp(alpha[i] + gamma - ((theta[i] - zeta)^2)), omega_user[i]*omega_domain)
#    # Y[i, ] <- rpois(n_domains, exp(alpha[i] + gamma - ((theta[i] - zeta)^2)))
#}
#
## Compile the mediascore Stan model
#model_media_scores <- stan_model(file = "src/stan_files/mediascores.stan")
#
## N = number of users; M = number of domains; G = number of groups;
## group = group each user belongs to; Y = user-domain count matrix
#model_data <- list(N = nrow(Y), M = ncol(Y), G = length(unique(group)),
#                   group = group, Y = Y, anchors = anchors)
#
#
#posterior <- mediascores(Y, group, anchors, "meanfield")
#
#
#plot(alpha, sapply(extract(posterior, paste0("alpha[", 1:n_users, "]")), median)); abline(0, 1)
#plot(gamma, sapply(extract(posterior, paste0("gamma[", 1:n_domains, "]")), median)); abline(0, 1)
#plot(theta, sapply(extract(posterior, paste0("theta[", 1:n_users, "]")), median)); abline(0, 1)
#plot(zeta, sapply(extract(posterior, paste0("zeta[", 1:n_domains, "]")), median)); abline(0, 1)
#plot(omega_user, sapply(extract(posterior, paste0("omega_user[", 1:n_users, "]")), median)); abline(0, 1)
#plot(omega_domain, sapply(extract(posterior, paste0("omega_domain[", 1:n_domains, "]")), median)); abline(0, 1)
#
#
## Check whether 90% of CIs contain true value
#summary(posterior, "alpha[1]")
#
#mean(c(eta, theta_mu[1], theta_mu[2], theta_sigma[1], theta_sigma[2], zeta_sigma) > sapply(extract(posterior, c("eta", "theta_mu[1]", "theta_mu[2]", "theta_sigma[1]", "theta_sigma[2]", "zeta_sigma")), function(x) quantile(x, 0.05)) &
#     c(eta, theta_mu[1], theta_mu[2], theta_sigma[1], theta_sigma[2], zeta_sigma) < sapply(extract(posterior, c("eta", "theta_mu[1]", "theta_mu[2]", "theta_sigma[1]", "theta_sigma[2]", "zeta_sigma")), function(x) quantile(x, 0.95)))
#
#mean(alpha > sapply(extract(posterior, paste0("alpha[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
#     alpha < sapply(extract(posterior, paste0("alpha[", 1:n_users, "]")), function(x) quantile(x, 0.95)))
#mean(gamma > sapply(extract(posterior, paste0("gamma[", 1:n_domains, "]")), function(x) quantile(x, 0.05)) &
#     gamma < sapply(extract(posterior, paste0("gamma[", 1:n_domains, "]")), function(x) quantile(x, 0.95)))
#mean(theta > sapply(extract(posterior, paste0("theta[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
#     theta < sapply(extract(posterior, paste0("theta[", 1:n_users, "]")), function(x) quantile(x, 0.95)))
#mean(zeta > sapply(extract(posterior, paste0("zeta[", 1:n_domains, "]")), function(x) quantile(x, 0.05)) &
#     zeta < sapply(extract(posterior, paste0("zeta[", 1:n_domains, "]")), function(x) quantile(x, 0.95)))
#mean(omega > sapply(extract(posterior, paste0("omega[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
#     omega < sapply(extract(posterior, paste0("omega[", 1:n_users, "]")), function(x) quantile(x, 0.95)))