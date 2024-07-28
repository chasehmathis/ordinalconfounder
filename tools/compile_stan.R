library(cmdstanr)
cmdstan_model("stan_models/ordinal.stan")
options(mc.cores = parallel::detectCores())
