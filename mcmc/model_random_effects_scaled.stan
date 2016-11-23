data {
  int<lower=2> N;
  int<lower=1> M;
  real<lower=0> nu;
  int t[N];
  matrix[N,M] X;
}


parameters {
  vector[M] w;
  vector[N] z;
}

model {
  z ~ normal(0, nu^2);
  w ~ cauchy(0, 1);
  for (i in 1:N)
    log(t[i]) ~ poisson(X[i] * w + z[i]);
}
