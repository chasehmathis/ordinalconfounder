data {
  int<lower=1> N;          // number of data points
  int<lower=1> C;          // number of categories
  array[N] int<lower=1, upper=C> y; // outcome variable with C ordered categories
  matrix[N, 1] X;          // predictor matrix
  vector[C-1] center;
}
parameters {
  vector[1] beta;          // coefficients for predictors
  ordered[C-1] c;          // thresholds
  real alpha;              // intercept
}
model {
  vector[N] eta;

  // Priors
  beta ~ normal(0, 2);
  alpha ~ normal(0,1);
  for (i in 1:(C-1))
    c[i] ~ normal(center[i], 1);

  // Latent variable model
  for (n in 1:N)
    eta[n] = X[n] * beta + alpha;

  // Likelihood
  for (n in 1:N)
    y[n] ~ ordered_logistic(eta[n], c);
}
