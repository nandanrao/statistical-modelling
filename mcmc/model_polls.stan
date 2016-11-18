data {

    int<lower=1> n_polls;
    int<lower=1> n_new;
    int<lower=1> n_parties;
    int<lower=1> n_pollsters;
    int<lower=1> n_elecs;
    
    int<lower=1> pollster_map[n_polls];
    int<lower=1> elec_map[n_polls];

    vector<lower=0,upper=1>[n_parties] predictions[n_polls];
    vector<lower=0,upper=1>[n_parties] vote[n_elecs - 1];
}


parameters {

    vector[n_parties] std_pollster_bias[n_pollsters];
    vector[n_parties] std_election_bias[n_elecs];
    
    vector<lower=0>[n_parties] sd_pollster_bias;
    vector<lower=0>[n_parties] sd_election_bias;
    vector<lower=0>[n_parties] sd_resid;

    vector<lower=0,upper=1>[n_parties] vote_new;
}


transformed parameters {

    vector[n_parties] pollster_bias[n_pollsters];
    vector[n_parties] election_bias[n_elecs];
    vector[n_parties] trend[n_elecs];
    
    for (i in 1:n_pollsters)
        pollster_bias[i] <- sd_pollster_bias .* std_pollster_bias[i];
    
    for (i in 1:n_elecs)
        election_bias[i] <- sd_election_bias .* std_election_bias[i];
}


model {

    for (i in 1:n_pollsters)
    	std_pollster_bias[i] ~ normal(0, 1);
    
    for (i in 1:n_elecs)
    	std_election_bias[i] ~ normal(0, 1);
    
    for (i in 1:(n_polls - n_new))
        predictions[i] ~ normal(
	    vote[elec_map[i]] + pollster_bias[pollster_map[i]] + election_bias[elec_map[i]],
	    sd_resid);

    for (i in (n_polls - n_new + 1):n_polls)
        predictions[i] ~ normal(
	    vote_new + pollster_bias[pollster_map[i]] + election_bias[elec_map[i]],
	    sd_resid);
}