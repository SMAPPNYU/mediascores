library(rstan)
library(plyr)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(stargazer)
library(overlap)
library(scales)
library(parallel)
library(matrixStats)
library(MASS)
library(invgamma)

##### SET WORKING DIRECTORY (IF ON PERSONAL COMPUTER OR HPC)
if (grepl("gregoryeady", system("whoami", intern = TRUE))) {
  setwd("/Users/GregoryEady/GitHub/mediascores")
} else if (grepl("ge31", system("whoami", intern = TRUE))) {
  setwd("/scratch/ge31/GitHub/mediascores")
}

n_cores <- 4
options(mc.cores = n_cores)

n_users <- 200
n_domains <- 500

# Group that users might belong to (e.g. Republican, Democrat, ordinary user)
# Can input "1" for all users if all belong to same group or no grouping desired
group <- rbinom(n_users, 1, 0.6) + 1

# alpha is the user-level intercept
# Represents the baseline degree to which a given user shares news URLs
# in general
alpha_mu <- 2
alpha_sigma <- 0.5
alpha <- rnorm(n_users, mean = alpha_mu, sd = alpha_sigma)


# gamma is the domain-level intercept
# Represents the baseline degree to which a users shares news URLs from a given 
# news domain in general. Will be a large value for often-shared domains
# (e.g. nytimes.com) and low for rarely-shared domains (e.g. stripes.com)
# No gamma_mu because is unidentified (alpha_mu acts as the model intercept)
gamma_sigma <- 0.75
gamma <- rnorm(n_domains, mean = 0, sd = gamma_sigma)

# theta is the ideology of each user
# theta is given a separate hierarchical prior defined by a user's group
# i.e. the "group" variable defined above
theta_mu <- c(-0.75, 0.5)
theta_sigma <- c(0.25, 0.25)
theta <- numeric(length(group))

theta[group == 1] <- rnorm(sum(group == 1),
                           mean = theta_mu[1], sd = theta_sigma[1])

theta[group == 2] <- rnorm(sum(group == 2),
                           mean = theta_mu[2], sd = theta_sigma[2])


# zeta is the ideology of each news domain
# Identification in the model is fixed by setting the prior on these parameters
# to Normal(0, 1)
zeta_sigma <- 1
zeta <- rnorm(n_domains, mean = 0, sd = 1)

# To define the polarization the scale (whether high values denote liberal or
# conservative), we assume that the research has in mind a well-known liberal
# domain and a well-known conservative domain.
# For simulation, we'll define the anchors as the domains with the lowest and
# highest theta
anchors <- c(which(zeta == min(zeta)), which(zeta == max(zeta)))

# omega_user and omega_domain represent the variance of the model
a_user <- 5
b_user <- 5
omega_user <- rinvgamma(n_users, a_user, b_user)

a_domain <- 5
b_domain <- 5
omega_domain <- rinvgamma(n_domains, a_domain, b_domain)

# Compute the values of the user-domain count matrix: the count of each domain
# shared by each user
Y <- matrix(data = 0, nrow = n_users, ncol = n_domains)

for(i in 1:n_users) {
    Y[i, ] <- rnegbin(n_domains, exp(alpha[i] + gamma - ((theta[i] - zeta)^2)), omega_user[i]*omega_domain)
    # Y[i, ] <- rpois(n_domains, exp(alpha[i] + gamma - ((theta[i] - zeta)^2)))
}

# Compile the mediascore Stan model
model_media_scores <- stan_model(file = "src/stan_files/mediascores.stan")


# Fit the model
posterior <- mediascores(Y, group, anchors, "meanfield")


plot(alpha, sapply(extract(posterior, paste0("alpha[", 1:n_users, "]")), median)); abline(0, 1)
plot(gamma, sapply(extract(posterior, paste0("gamma[", 1:n_domains, "]")), median)); abline(0, 1)
plot(theta, sapply(extract(posterior, paste0("theta[", 1:n_users, "]")), median)); abline(0, 1)
plot(zeta, sapply(extract(posterior, paste0("zeta[", 1:n_domains, "]")), median)); abline(0, 1)
plot(omega_user, sapply(extract(posterior, paste0("omega_user[", 1:n_users, "]")), median)); abline(0, 1)
plot(omega_domain, sapply(extract(posterior, paste0("omega_domain[", 1:n_domains, "]")), median)); abline(0, 1)


# Check whether 90% of CIs contain true value
summary(posterior, "alpha[1]")

mean(c(eta, theta_mu[1], theta_mu[2], theta_sigma[1], theta_sigma[2], zeta_sigma) > sapply(extract(posterior, c("eta", "theta_mu[1]", "theta_mu[2]", "theta_sigma[1]", "theta_sigma[2]", "zeta_sigma")), function(x) quantile(x, 0.05)) &
     c(eta, theta_mu[1], theta_mu[2], theta_sigma[1], theta_sigma[2], zeta_sigma) < sapply(extract(posterior, c("eta", "theta_mu[1]", "theta_mu[2]", "theta_sigma[1]", "theta_sigma[2]", "zeta_sigma")), function(x) quantile(x, 0.95)))

mean(alpha > sapply(extract(posterior, paste0("alpha[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
     alpha < sapply(extract(posterior, paste0("alpha[", 1:n_users, "]")), function(x) quantile(x, 0.95)))
mean(gamma > sapply(extract(posterior, paste0("gamma[", 1:n_domains, "]")), function(x) quantile(x, 0.05)) &
     gamma < sapply(extract(posterior, paste0("gamma[", 1:n_domains, "]")), function(x) quantile(x, 0.95)))
mean(theta > sapply(extract(posterior, paste0("theta[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
     theta < sapply(extract(posterior, paste0("theta[", 1:n_users, "]")), function(x) quantile(x, 0.95)))
mean(zeta > sapply(extract(posterior, paste0("zeta[", 1:n_domains, "]")), function(x) quantile(x, 0.05)) &
     zeta < sapply(extract(posterior, paste0("zeta[", 1:n_domains, "]")), function(x) quantile(x, 0.95)))
mean(omega > sapply(extract(posterior, paste0("omega[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
     omega < sapply(extract(posterior, paste0("omega[", 1:n_users, "]")), function(x) quantile(x, 0.95)))


