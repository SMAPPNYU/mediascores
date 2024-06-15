data {

  int N;
  int M;
  int G;
  int group[N];
  int Y[N, M];
  int anchors[2];

}

parameters {

  // User and domain intercepts
  vector[N] alpha;             // User intercept
  real alpha_mu;               // User intercept
  real<lower = 0> alpha_sigma; // User intercept

  vector[M] gamma;             // Domain intercept
  real<lower = 0> gamma_sigma; // Domain intercept

  // Variance
  vector<lower = 0>[M] omega_domain; // Dispersion
  real<lower = 0> omega_domain_a;    // Dispersion
  real<lower = 0> omega_domain_b;    // Dispersion

  // Ideology
  vector[N] theta_unconstrained;     // User ideology
  vector[G] theta_mu_unconstrained;  // User ideology
  vector<lower = 0>[G] theta_sigma;  // User ideology

  vector[M] zeta_unconstrained;      // Domain ideology
  real<lower = 0> zeta_sigma;        // Domain scale

}

transformed parameters {
}

model {
  vector[N] lin_pred;
  real C[N, M]; // Count

  alpha_mu ~ normal(0, 2.5);             // User intercept
  alpha_sigma ~ normal(0, 2.5);          // User intercept
  alpha ~ normal(alpha_mu, alpha_sigma); // User intercept

  gamma_sigma ~ normal(0, 2.5);   // Domain intercept
  gamma ~ normal(0, gamma_sigma); // Domain intercept
  
  omega_domain_a ~ normal(0, 5);
  omega_domain_b ~ normal(0, 5);
  omega_domain ~ inv_gamma(omega_domain_a, omega_domain_b); // Dispersion

  theta_sigma ~ normal(0, 1);             // User ideology
  theta_mu_unconstrained ~ normal(0, 1);  // User ideology
  for(g in 1:N) {
    theta_unconstrained[group[g]] ~ normal(theta_mu_unconstrained[group[g]], theta_sigma[group[g]]); // User ideology
  }

  zeta_sigma ~ normal(0, 1);                  // Domain ideology
  zeta_unconstrained ~ normal(0, zeta_sigma); // Domain ideology

  for(m in 1:M) {
    for(j in 1:N) {
      lin_pred[j] = alpha[j] + gamma[m] - square(theta_unconstrained[j] - zeta_unconstrained[m]);
    }
    Y[, m] ~ neg_binomial_2_log(lin_pred, omega_domain[m]);
  }

}

generated quantities {

  // Fix reflection invariance
  vector[M] zeta = zeta_unconstrained[anchors[1]] < zeta_unconstrained[anchors[2]] ? zeta_unconstrained : zeta_unconstrained * -1;
  vector[N] theta = zeta_unconstrained[anchors[1]] < zeta_unconstrained[anchors[2]] ? theta_unconstrained : theta_unconstrained * -1;
  vector[G] theta_mu = zeta_unconstrained[anchors[1]] < zeta_unconstrained[anchors[2]] ? theta_mu_unconstrained : theta_mu_unconstrained * -1;

}
