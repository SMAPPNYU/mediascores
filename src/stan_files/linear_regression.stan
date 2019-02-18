data {
  int n; 
  int k; 
  real y[n]; 
  matrix[n,k] X; 
}
parameters {
  vector[k] beta; 
  real sigma; 
}
model {  
  beta[1] ~ cauchy(0,10);
  for(i in 2:k) beta[i] ~ cauchy(0,2.5);
  y ~ normal(X*beta, sigma);
}
