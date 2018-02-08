
posterior_var = function(prior_var, likelihood_var) {
  prior_var * likelihood_var / (prior_var + likelihood_var)
}

posterior_var_2 = function(prior_var, likelihood_var) {
  1/ (1/prior_var + 1/likelihood_var)
}


posterior_var(0.5,1)
posterior_var_2(0.5,1)


posterior_var(0.5,10)