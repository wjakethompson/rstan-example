data {
  int<lower=1> J;                         // number of respondents
  int<lower=1> K;                         // number of items
  int<lower=1> N;                         // number of observations
  int<lower=1,upper=J> jj[N];             // respondent for observation n
  int<lower=1,upper=K> kk[N];             // item for observation n
  int<lower=0,upper=1> y[N];              // score for observation n
}
parameters {
  real theta[J];
  real b[K];
  real<lower=0> a[K];
  real<lower=0,upper=1> c[K];
}
model {
  vector[N] eta;
  
  // priors
  theta ~ normal(0, 1);
  b ~ normal(0, 10);
  a ~ lognormal(0.5, 1);
  c ~ beta(5, 17);
  
  // model
  for (n in 1:N) {
    eta[n] = c[kk[n]] + (1 - c[kk[n]]) * inv_logit(a[kk[n]] * (theta[jj[n]] - b[kk[n]]));
  }
  y ~ bernoulli(eta);
}
