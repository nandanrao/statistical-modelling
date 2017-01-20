data {
  int<lower=0> N;
  real<lower=0> price[N];
  int<lower=0> available[N];
  int<upper=max(available)> purchased[N];
}
parameters {
  real x;
  real b;
}
model {
  for (n in 1:N)
    purchased[n] ~ poisson(price[n] * x + b) T[,available[n]];
}
