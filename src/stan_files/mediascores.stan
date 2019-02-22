functions {

  vector neg_bin(vector global_par, vector local_par, real[] real_var, int[] int_var) {
    int map_N = size(int_var);
    vector[map_N] map_alpha = global_par[1:map_N];
    vector[map_N] map_theta = global_par[(map_N+1):(map_N*2)];
    vector[map_N] map_omega_user = global_par[(map_N*2+1):(map_N*3)];
    real map_gamma = local_par[1];
    real map_zeta = local_par[2];
    real map_omega_domain = local_par[3];
    vector[map_N] lin_pred;
    real ll = 0;

    for(i in 1:map_N) {
      lin_pred[i] = map_alpha[i] + map_gamma - square(map_theta[i] - map_zeta);
    }

    ll = neg_binomial_2_log_lpmf(int_var | lin_pred, map_omega_user * map_omega_domain);

    return [ll]';

  }

}

data {

  int N;
  int M;
  int G;
  int group[N];
  int Y[N, M];
  int anchors[2];

}

transformed data {
  int<lower = 0> x_i[M, N];
  real x_r[M, N];

  for (m in 1:M) {
      x_i[m] = to_array_1d(Y[, m]);
      x_r[m] = to_array_1d(rep_row_vector(0, N));
  }

}

parameters {

  // User and domain intercepts
  vector[N] alpha_raw;         // User intercept
  real alpha_mu;               // User intercept
  real<lower = 0> alpha_sigma; // User intercept

  vector[M] gamma_raw;         // Domain intercept
  real<lower = 0> gamma_sigma; // Domain intercept

  // Variance
  vector<lower = 0>[N] omega_user; // Dispersion (N or M)
  real<lower = 0> omega_user_a;    // Dispersion
  real<lower = 0> omega_user_b;    // Dispersion

  vector<lower = 0>[M] omega_domain; // Dispersion (N or M)
  real<lower = 0> omega_domain_a;    // Dispersion
  real<lower = 0> omega_domain_b;    // Dispersion

  // Ideology
  vector[N] theta_unconstrained_raw; // User ideology
  vector[G] theta_mu_unconstrained;  // User ideology
  vector<lower = 0>[G] theta_sigma;  // User ideology

  vector[M] zeta_unconstrained_raw;  // Domain ideology
  real<lower = 0> zeta_sigma;        // Domain scale

}

transformed parameters {
  vector[N] alpha = alpha_mu + alpha_raw * alpha_sigma;
  vector[M] gamma = gamma_raw * gamma_sigma;
  vector[N] theta_unconstrained;
  vector[M] zeta_unconstrained = zeta_unconstrained_raw * zeta_sigma;
  vector[3] gamma_zeta_omega[M];

  for(m in 1:M) {
    gamma_zeta_omega[m, 1] = gamma[m];
    gamma_zeta_omega[m, 2] = zeta_unconstrained[m];
    gamma_zeta_omega[m, 3] = omega_domain[m];
  }
  
  for(i in 1:N) {
    theta_unconstrained[i] = theta_mu_unconstrained[group[i]] + theta_unconstrained_raw[i] * theta_sigma[group[i]];
    // theta_unconstrained[i] = theta_unconstrained_raw[i];
  }

}

model {
  
  real C[N, M]; // Count

  alpha_mu ~ normal(0, 2.5);    // User intercept
  alpha_sigma ~ normal(0, 2.5); // User intercept
  alpha_raw ~ normal(0, 1);     // User intercept

  gamma_sigma ~ normal(0, 2.5); // Domain intercept
  gamma_raw ~ normal(0, 1);     // Domain intercept
  
  omega_user_a ~ normal(0, 5);
  omega_user_b ~ normal(0, 5);
  omega_user ~ inv_gamma(omega_user_a, omega_user_b); // Dispersion

  omega_domain_a ~ normal(0, 5);
  omega_domain_b ~ normal(0, 5);
  omega_domain ~ inv_gamma(omega_domain_a, omega_domain_b); // Dispersion

  theta_sigma ~ normal(0, 1);             // User ideology
  theta_mu_unconstrained ~ normal(0, 1);  // User ideology
  theta_unconstrained_raw ~ normal(0, 1); // User ideology
  // theta_unconstrained_raw ~ normal(0, 2.5); // User ideology

  zeta_sigma ~ normal(0, 1);             // Domain ideology
  zeta_unconstrained_raw ~ normal(0, 1); // Domain ideology

  target += sum(map_rect(neg_bin,
                         append_row(append_row(alpha, theta_unconstrained), omega_user),
                         gamma_zeta_omega, x_r, x_i));

}

generated quantities {

  // Fix reflection invariance
  vector[M] zeta = zeta_unconstrained[anchors[1]] < zeta_unconstrained[anchors[2]] ? zeta_unconstrained : zeta_unconstrained * -1;
  vector[N] theta = zeta_unconstrained[anchors[1]] < zeta_unconstrained[anchors[2]] ? theta_unconstrained : theta_unconstrained * -1;
  vector[G] theta_mu = zeta_unconstrained[anchors[1]] < zeta_unconstrained[anchors[2]] ? theta_mu_unconstrained : theta_mu_unconstrained * -1;


}

