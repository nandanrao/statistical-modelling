library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


## load data
load("synth_reg.rda")
load("polls.rda")


## fit unpenalized model

model_basic <- stan("model_basic.stan", data=synth_reg)
plot(model_basic, pars="w")
plot(model_basic, pars="sig_e")


## fit ridge model

model_ridge <- stan("model_ridge.stan", data=synth_reg)
plot(model_ridge, pars="w")
plot(model_ridge, pars="sig_e")
plot(model_ridge, pars="lam")



model_ridge_2 <- stan("model_ridge.stan", data=synth_reg)
## fit sparse model

model_sparse <- stan("model_sparse.stan", data=synth_reg, iter=4000)
plot(model_ridge, pars="w")
plot(model_ridge, pars="sig_e")
plot(model_sparse, pars="lam")
plot(model_sparse, pars="z")


## fit polls model

model_polls <- stan("model_polls.stan", data=polls, iter=2000)
plot(model_polls, pars="pollster_bias")
plot(model_polls, pars="election_bias")
plot(model_polls, pars="vote_new")
vote <- extract(model_polls, pars="vote_new")$vote_new
mean(vote[,1] > vote[,2])
