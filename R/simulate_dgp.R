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

##### SET WORKING DIRECTORY (IF ON PERSONAL COMPUTER OR HPC)
if(grepl("gregoryeady", system("whoami", intern = TRUE))) {
  setwd("/Users/GregoryEady/Dropbox (Personal)/Code/URL_Scaling/")
} else if(grepl("ge31", system("whoami", intern = TRUE))) {
  setwd("/scratch/ge31/URL_Scaling")
}

n_cores <- 4
options(mc.cores = n_cores)

n_users <- 250
n_domains <- 100
affiliation <- rbinom(n_users, 1, 0.6) + 1

lambda <- 1

alpha_mu <- 1.5
alpha_sigma <- 0.5
alpha <- rnorm(n_users, mean = alpha_mu, sd = alpha_sigma)

gamma_sigma <- 0.75
gamma <- rnorm(n_domains, mean = 0, sd = gamma_sigma)

theta_mu <- c(-0.75, 0.5)
theta_sigma <- c(0.25, 0.25)
theta <- numeric(length(affiliation))

theta[affiliation == 1] <- rnorm(sum(affiliation == 1),
                                 mean = theta_mu[1], sd = theta_sigma[1])

theta[affiliation == 2] <- rnorm(sum(affiliation == 2),
                                 mean = theta_mu[2], sd = theta_sigma[2])

zeta_sigma <- 0.5
zeta <- rnorm(n_domains, mean = 0, sd = zeta_sigma)
zeta[1] <- -1
zeta[2] <- +1

# a <- 4
# b <- 3
# omega <- 1/rgamma(n_users, a, b)

Y <- matrix(data = 0, nrow = n_users, ncol = n_domains)

for(i in 1:n_users) {
    # Y[i, ] <- rnegbin(n_domains, exp(alpha[i] + gamma - lambda * ((theta[i] - zeta)^2)), omega[i])
    Y[i, ] <- rpois(n_domains, exp(alpha[i] + gamma - lambda * ((theta[i] - zeta)^2)))
}


model_media_scores <- stan_model(file = "Media_Scores.stan")

model_data <- list(n_users = n_users, n_domains = n_domains,
                   group = affiliation, Y = Y, verbose = 2)

inits <- rep(list(list(theta = as.numeric(affiliation == 2) - as.numeric(affiliation == 1),
                       # theta_mu = c(-1, 1),
                       theta_mu_1 = -1,
                       theta_mu_2 = 1,
                       theta_sigma = c(1, 1),
                       lambda = 1,
                       alpha = as.numeric(scale(log(apply(Y, 1, sum)))),
                       gamma = as.numeric(scale(log(apply(Y, 2, sum)))))),
             n_cores)

model_sim_fitted <- sampling(model_media_scores, data = model_data,
                             warmup = 750, iter = 1500,
                             init = inits, refresh = 50,
                             open_progress = TRUE, chains = n_cores)

# model_sim_fitted <- vb(model_media_scores, data = model_data, init = inits[[1]],
#                        output_samples = 5000, tol_rel_obj = 0.001,
#                        algorithm = "meanfield")


plot(alpha, sapply(extract(model_sim_fitted, paste0("alpha[", 1:n_users, "]")), median)); abline(0, 1)
plot(gamma, sapply(extract(model_sim_fitted, paste0("gamma[", 1:n_domains, "]")), median)); abline(0, 1)
plot(theta, sapply(extract(model_sim_fitted, paste0("theta[", 1:n_users, "]")), median)); abline(0, 1)
plot(zeta, sapply(extract(model_sim_fitted, paste0("zeta[", 1:n_domains, "]")), median)); abline(0, 1)
plot(omega, sapply(extract(model_sim_fitted, paste0("omega[", 1:n_users, "]")), median)); abline(0, 1)


# Check whether 90% of CIs contain true value
summary(model_sim_fitted, "alpha[1]")

mean(c(eta, theta_mu[1], theta_mu[2], theta_sigma[1], theta_sigma[2], zeta_sigma) > sapply(extract(model_sim_fitted, c("eta", "theta_mu[1]", "theta_mu[2]", "theta_sigma[1]", "theta_sigma[2]", "zeta_sigma")), function(x) quantile(x, 0.05)) &
     c(eta, theta_mu[1], theta_mu[2], theta_sigma[1], theta_sigma[2], zeta_sigma) < sapply(extract(model_sim_fitted, c("eta", "theta_mu[1]", "theta_mu[2]", "theta_sigma[1]", "theta_sigma[2]", "zeta_sigma")), function(x) quantile(x, 0.95)))

mean(alpha > sapply(extract(model_sim_fitted, paste0("alpha[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
     alpha < sapply(extract(model_sim_fitted, paste0("alpha[", 1:n_users, "]")), function(x) quantile(x, 0.95)))
mean(gamma > sapply(extract(model_sim_fitted, paste0("gamma[", 1:n_domains, "]")), function(x) quantile(x, 0.05)) &
     gamma < sapply(extract(model_sim_fitted, paste0("gamma[", 1:n_domains, "]")), function(x) quantile(x, 0.95)))
mean(theta > sapply(extract(model_sim_fitted, paste0("theta[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
     theta < sapply(extract(model_sim_fitted, paste0("theta[", 1:n_users, "]")), function(x) quantile(x, 0.95)))
mean(zeta > sapply(extract(model_sim_fitted, paste0("zeta[", 1:n_domains, "]")), function(x) quantile(x, 0.05)) &
     zeta < sapply(extract(model_sim_fitted, paste0("zeta[", 1:n_domains, "]")), function(x) quantile(x, 0.95)))
mean(omega > sapply(extract(model_sim_fitted, paste0("omega[", 1:n_users, "]")), function(x) quantile(x, 0.05)) &
     omega < sapply(extract(model_sim_fitted, paste0("omega[", 1:n_users, "]")), function(x) quantile(x, 0.95)))


