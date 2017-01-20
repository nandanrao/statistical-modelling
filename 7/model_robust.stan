data {
  int<lower=2> N;
  int<lower=1> M;
  vector[N] t;
  matrix[N,M] Phy;
}

parameters {
  real<lower=0> sig_e;
  vector[M] w;
  real w0;
  vector<lower=0>[N] z;
  real<lower=2> nu;
}

transformed parameters {
  vector<lower=0>[N] sqrt_z;
  for (n in 1:N)
    sqrt_z[n] = sqrt(z[n]);
}

model {
  nu ~ cauchy(0, 2.5);
  w ~ normal(0, sig_e);
  z ~ inv_gamma(nu/2, nu/2 -1);
  t ~ normal(rep_vector(w0, N) + Phy * w, sig_e * sqrt_z);
}
