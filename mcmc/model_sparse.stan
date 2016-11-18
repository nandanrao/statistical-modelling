data {

  int<lower=2> N;
  int<lower=1> M;
  vector[N] t;
  matrix[N,M] Phy;
}


parameters {

  real<lower=0> sig_e;
  real w0;
  vector[M] w;
  vector<lower=0>[M] z;
  real<lower=0> lam;

}

transformed parameters {
  vector<lower=0>[M] sqrt_z;
  for (m in 1:M)
    sqrt_z[m] = sqrt(z[m]);
}


model {
  lam ~ cauchy(0, 1);
  z ~ gamma(1, 1 / (2 * lam^2));
  w ~ normal(0, sig_e * sqrt_z);
  t ~ normal(rep_vector(w0, N) + Phy * w, sig_e);
}
