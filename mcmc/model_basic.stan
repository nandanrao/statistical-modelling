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
}


model {

    t ~ normal(rep_vector(w0, N) + Phy * w, sig_e);
}
