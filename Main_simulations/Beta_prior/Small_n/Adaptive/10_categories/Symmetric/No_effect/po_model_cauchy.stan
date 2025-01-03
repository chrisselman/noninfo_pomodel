// Cumulative logistic model with proportional odds assumption.
data {
  int<lower=2> n_states;    
  int<lower=0> n_patients;  
  
  int y[n_patients];  
  int x[n_patients];
  
  vector<lower=0>[n_states] p_par; // dirichlet prior hyper-parameters
}

parameters {
  simplex[n_states] p;
  real beta;
}

transformed parameters {
  ordered[n_states-1] alpha;
  alpha = logit(cumulative_sum(p[1:(n_states-1)]));
}

model {
  
  p ~ dirichlet(p_par);

  beta ~  student_t(1,0,2);  
  
  for(i in 1:n_patients)
    y[i] ~ ordered_logistic(x[i] * beta, alpha);
}


generated quantities {
  real odds_ratio;
  odds_ratio = exp(beta);
  
  vector[n_patients] y_rep;
  for (n in 1:n_patients) y_rep[n] = ordered_logistic_rng(x[n]*beta, alpha);
  
}