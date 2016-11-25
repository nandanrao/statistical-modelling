data {
  int<lower=2> N;
  int<lower=1> M;
  vector[N] t;
  matrix[N,M] Phy;
}

parameters {
  real<lower=0> sig_e;
  vector[M] w;
  vector<lower=0>[N] z;
  real<lower=2> nu;
}

model {
  nu ~ cauchy(0, 1);
  w ~ normal(0, sig_e);

  for(i in 1:N)
    target += gamma_lpdf(z[i] | nu/2, nu/2 - 1) +  normal_lpdf(t[i] | Phy * w, z[i]);

}
