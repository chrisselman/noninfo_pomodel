## Set working directory 
setwd("/group/cebu1/BACKUP/Chris/Project_3/noninfo_pomodel/Main_simulations/Beta_prior/Small_n/Adaptive/4_categories/Uniform/No_effect")

## Main simulation scenario 
rm(list=ls())

options(mc.cores = 8)
## Load in the appropriate packages
library("rstan")
library('posterior')
library('rstanarm')

args = commandArgs(trailingOnly = TRUE)
datnum <- as.numeric(args[1])

# Load the 1000 datasets
load("sim_data.Rdata")
attach(big_data)

# Select a subset of the data
s2 = seq(20,1000,by=20)
s1=s2-19

temp = big_data[(big_data$k >= s1[datnum] & big_data$k <= s2[datnum]),]

# Set seed
set.seed(071023)
seed=sample(1:1e8,50,replace=F)[datnum]

set.seed(seed)
nsim <- 1000

## Define states and other parameters 
# 4 category outcome
states <- c("A", "B", "C","D")

#Generate empty vectors to store performance measures and diagnostic measures 
## Store bias for each analysis model  
bias_normal_largesd <- bias_normal_smallsd <- bias_laplace_largesd <- bias_laplace_smallsd <- bias_cauchy <- bias_rsquare <- length(nsim)  


## Store coverage for each analysis model  
coverage_normal_largesd <- coverage_normal_smallsd <- coverage_laplace_largesd <- coverage_laplace_smallsd <- coverage_cauchy <- coverage_rsquare <- length(nsim)  


## Store MSE for each analysis model  
mse_normal_largesd <- mse_normal_smallsd <- mse_laplace_largesd <- mse_laplace_smallsd <- mse_cauchy <- mse_rsquare <- length(nsim)  


## Store posterior probability 
postprob_normal_largesd <- postprob_normal_smallsd <- postprob_laplace_largesd <- postprob_laplace_smallsd <- postprob_cauchy <- postprob_rsquare <- length(nsim)

## Store R-hat for each parameter of each cumulative logit for each analysis model  
rhat_normal_largesd <- rhat_normal_smallsd <- rhat_laplace_largesd <- rhat_laplace_smallsd <- rhat_cauchy <- rhat_rsquare <- length(nsim)  



## Store bulk ESS for each parameter of each cumulative logit for each analysis model  
bulkess_normal_largesd <- bulkess_normal_smallsd <- bulkess_laplace_largesd <- bulkess_laplace_smallsd <- bulkess_cauchy <- bulkess_rsquare <- length(nsim)  


## Store tail ESS for each parameter of each cumulative logit for each analysis model  
tailess_normal_largesd <- tailess_normal_smallsd <- tailess_laplace_largesd <- tailess_laplace_smallsd <- tailess_cauchy <- tailess_rsquare <- length(nsim)  


## Store MCSE for each parameter of each cumulative logit for each analysis model  
mcse_normal_largesd <- mcse_normal_smallsd <- mcse_laplace_largesd <- mcse_laplace_smallsd <- mcse_cauchy <- mcse_rsquare <- length(nsim)  


## Number of divergent transitions 
numdivergent_normal_largesd <- numdivergent_normal_smallsd <- numdivergent_laplace_largesd <- numdivergent_laplace_smallsd <- numdivergent_cauchy <- length(nsim)  

## Frequentist results 
bias_freq <- coverage_freq <- mse_freq <- length(nsim)

## Stopped or not 
stopping_normal_largesd <- stopping_normal_smallsd <- stopping_laplace_largesd <- stopping_laplace_smallsd <- stopping_cauchy <- stopping_rsquare <- stopping_freq <- length(nsim)

for (k in s1[datnum]:s2[datnum]){
  x <- big_data$x[big_data$k == k]
  y <- as.integer(big_data$y[big_data$k == k])
  
n_states = 4 ## Number of levels in ordinal outcome 
n_patients = as.integer(length(y)) # Sample size 


## METHOD 1 assuming normal prior with SD = 100
sdbeta = 100

example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
  )

example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                   # control = list(adapt_delta = 0.99, max_treedepth = 12)
                   )

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {

# Extract summary for proportional OR 
# Bias for each cumulative logit 
bias_normal_largesd[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)


# Coverage - does the true value fall in the credible interval?
lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

coverage_normal_largesd[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)

# MSE 
mse_normal_largesd[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2

# Posterior probability 
postprob_normal_largesd[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 


# MCSE 
mcse_normal_largesd[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])

# R-hat 
rhat_normal_largesd[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])

# Bulk ESS 
bulkess_normal_largesd[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])

# Tail ESS 
tailess_normal_largesd[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])


# Number of divergent transitions 
numdivergent_normal_largesd[k] <- get_num_divergent(example_fit)

stopping_normal_largesd[k] <- 1
} else 
{
  example_data <- list(
    y = y, ## Outcome 
    n_states = n_states, # Number of categories
    n_patients = n_patients, ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )
  
  example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                      # control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
  

  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_normal_largesd[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_normal_largesd[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_normal_largesd[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_normal_largesd[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_normal_largesd[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_normal_largesd[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_normal_largesd[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_normal_largesd[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_normal_largesd[k] <- get_num_divergent(example_fit)
  
  stopping_normal_largesd[k] <- 0
  
}

## METHOD 2 assuming normal prior with SD = 2.5
sdbeta = 2.5


example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
)

example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                    # control = list(adapt_delta = 0.99, max_treedepth = 12)
)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_normal_smallsd[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_normal_smallsd[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_normal_smallsd[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_normal_smallsd[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_normal_smallsd[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_normal_smallsd[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_normal_smallsd[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_normal_smallsd[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_normal_smallsd[k] <- get_num_divergent(example_fit)
  
  stopping_normal_smallsd[k] <- 1
  
} else 
{
  example_data <- list(
    y = y, ## Outcome 
    n_states = n_states, # Number of categories
    n_patients = n_patients, ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )
  
  example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                      # control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_normal_smallsd[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_normal_smallsd[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_normal_smallsd[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_normal_smallsd[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_normal_smallsd[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_normal_smallsd[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_normal_smallsd[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_normal_smallsd[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_normal_smallsd[k] <- get_num_divergent(example_fit)
  
  stopping_normal_smallsd[k] <- 0
  
}



## METHOD 3 assuming Laplace prior with SD = 100
sdbeta = 100/sqrt(2)

example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
)

example_fit <- stan(file = "po_model_laplace.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                    # control = list(adapt_delta = 0.99, max_treedepth = 12)
)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_laplace_largesd[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_laplace_largesd[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_laplace_largesd[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_laplace_largesd[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_laplace_largesd[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_laplace_largesd[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_laplace_largesd[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_laplace_largesd[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_laplace_largesd[k] <- get_num_divergent(example_fit)
  
  stopping_laplace_largesd[k] <- 1
  
} else 
{
  example_data <- list(
    y = y, ## Outcome 
    n_states = n_states, # Number of categories
    n_patients = n_patients, ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )
  
  example_fit <- stan(file = "po_model_laplace.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                      # control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_laplace_largesd[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_laplace_largesd[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_laplace_largesd[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_laplace_largesd[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_laplace_largesd[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_laplace_largesd[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_laplace_largesd[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_laplace_largesd[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_laplace_largesd[k] <- get_num_divergent(example_fit)
  
  stopping_laplace_largesd[k] <- 0
  
}

## METHOD 4 assuming Laplace prior with SD = 2.5
sdbeta = 2.5/sqrt(2)

example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
)

example_fit <- stan(file = "po_model_laplace.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                    # control = list(adapt_delta = 0.99, max_treedepth = 12)
)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_laplace_smallsd[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_laplace_smallsd[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_laplace_smallsd[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_laplace_smallsd[k] <- get_num_divergent(example_fit)
  
  stopping_laplace_smallsd[k] <- 1
  
} else 
{
  example_data <- list(
    y = y, ## Outcome 
    n_states = n_states, # Number of categories
    n_patients = n_patients, ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )
  
  example_fit <- stan(file = "po_model_laplace.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                      # control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_laplace_smallsd[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_laplace_smallsd[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_laplace_smallsd[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_laplace_smallsd[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_laplace_smallsd[k] <- get_num_divergent(example_fit)
  
  stopping_laplace_smallsd[k] <- 0
  
}


## METHOD 5 assuming Cauchy prior
example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
)

example_fit <- stan(file = "po_model_cauchy.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                    # control = list(adapt_delta = 0.99, max_treedepth = 12)
)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cauchy[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_cauchy[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_cauchy[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_cauchy[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_cauchy[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_cauchy[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_cauchy[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_cauchy[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_cauchy[k] <- get_num_divergent(example_fit)
  
  stopping_cauchy[k] <- 1
  
} else 
{
  example_data <- list(
    y = y, ## Outcome 
    n_states = n_states, # Number of categories
    n_patients = n_patients, ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )
  
  example_fit <- stan(file = "po_model_cauchy.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4#,
                      # control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
  
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_cauchy[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_cauchy[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_cauchy[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_cauchy[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
  
  
  # MCSE 
  mcse_cauchy[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_cauchy[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_cauchy[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_cauchy[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  # Number of divergent transitions 
  numdivergent_cauchy[k] <- get_num_divergent(example_fit)
  
  stopping_cauchy[k] <- 0
  
}


## METHOD 6 assuming prior on the R-squared
example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  x = x[1:(length(x)/2)] ## Treatment variable
)


example_fit <- stan_polr(as.factor(y)~x, prior = R2(0.5, "mean"), data = as.data.frame(example_data), 
                         algorithm = "sampling", iter = 10000, cores = 4, seed = 22052024, chains = 4, prior_counts = dirichlet(1))

warnings(example_fit)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "x"]



postprob <- (dim(df[df$x > 0,])[1])/length(df$x) 

if (postprob > 0.99) {
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_rsquare[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_rsquare[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_rsquare[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_rsquare[k] <- (dim(df[df$x > 0,])[1])/length(df$x)  
  
  
  # MCSE 
  mcse_rsquare[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_rsquare[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_rsquare[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_rsquare[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  
  stopping_rsquare[k] <- 1
  
} else 
{
  example_data <- list(
    y = y[1:(length(y))], ## Outcome 
    x = x[1:(length(x))] ## Treatment variable
  )
  
  
  example_fit <- stan_polr(as.factor(y)~x, prior = R2(0.5, "mean"), data = as.data.frame(example_data), 
                           algorithm = "sampling", iter = 10000, cores = 4, seed = 22052024, chains = 4, prior_counts = dirichlet(1))
  
  warnings(example_fit)
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "x"]
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  bias_rsquare[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
  
  
  # Coverage - does the true value fall in the credible interval?
  lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  coverage_rsquare[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
  
  # MSE 
  mse_rsquare[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
  
  # Posterior probability 
  postprob_rsquare[k] <- (dim(df[df$x > 0,])[1])/length(df$x) 
  
  # MCSE 
  mcse_rsquare[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
  
  # R-hat 
  rhat_rsquare[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
  
  # Bulk ESS 
  bulkess_rsquare[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
  
  # Tail ESS 
  tailess_rsquare[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
  
  stopping_rsquare[k] <- 0
}




# Frequentist analysis 
library(MASS)

example_data <- list(
  y = y[1:(length(y)/2)], 
  x = x[1:(length(x)/2)]
)

freqmod <- polr(as.factor(y)~x, data = as.data.frame(example_data), 
     Hess = TRUE)

mod <- summary(freqmod)

pval <- as.numeric(pt(freqmod$coefficients/mod$coefficients[1,2], df = freqmod$df.residual, lower.tail=F))

if (pval < 0.01) {

## Store bias and corresponding coverage
bias_freq[k] <- as.numeric(freqmod$coefficients) - log(1)

coverage_freq[k] <- ifelse(as.numeric(confint(freqmod)[1]) <= log(1) & as.numeric(confint(freqmod)[2]) >= log(1), 1, 0)

mse_freq[k] <-  (as.numeric(freqmod$coefficients) - log(1))^2

stopping_freq[k] <- 1

}
else {
  example_data <- list(
    y = y[1:(length(y))], 
    x = x[1:(length(x))]
  )
  
  freqmod <- polr(as.factor(y)~x, data = as.data.frame(example_data), 
                  Hess = TRUE)
  
  mod <- summary(freqmod)
  
  
  ## Store bias and corresponding coverage
  bias_freq[k] <- as.numeric(freqmod$coefficients) - log(1)
  
  coverage_freq[k] <- ifelse(as.numeric(confint(freqmod)[1]) <= log(1) & as.numeric(confint(freqmod)[2]) >= log(1), 1, 0)
  
  mse_freq[k] <-  (as.numeric(freqmod$coefficients) - log(1))^2  
  
  stopping_freq[k] <- 0
  
}}



## Format data for analysis 
data <- data.frame(bias_rsquare = bias_rsquare, coverage_rsquare = coverage_rsquare, mse_rsquare = mse_rsquare, 
                   mcse_rsquare = mcse_rsquare, rhat_rsquare = rhat_rsquare, bulkess_rsquare = bulkess_rsquare,
                   tailess_rsquare = tailess_rsquare, postprob_rsquare = postprob_rsquare, stopping_rsquare = stopping_rsquare,
                   bias_normal_largesd = bias_normal_largesd, coverage_normal_largesd = coverage_normal_largesd, mse_normal_largesd = mse_normal_largesd, 
                   mcse_normal_largesd = mcse_normal_largesd, rhat_normal_largesd = rhat_normal_largesd, bulkess_normal_largesd = bulkess_normal_largesd,
                   tailess_normal_largesd = tailess_normal_largesd, postprob_normal_largesd = postprob_normal_largesd, numdivergent_normal_largesd = numdivergent_normal_largesd, stopping_normal_largesd = stopping_normal_largesd,
                   bias_normal_smallsd = bias_normal_smallsd, coverage_normal_smallsd = coverage_normal_smallsd, mse_normal_smallsd = mse_normal_smallsd, 
                   mcse_normal_smallsd = mcse_normal_smallsd, rhat_normal_smallsd = rhat_normal_smallsd, bulkess_normal_smallsd = bulkess_normal_smallsd,
                   tailess_normal_smallsd = tailess_normal_smallsd, postprob_normal_smallsd = postprob_normal_smallsd, numdivergent_normal_smallsd = numdivergent_normal_smallsd, stopping_normal_smallsd = stopping_normal_smallsd,
                   bias_laplace_largesd = bias_laplace_largesd, coverage_laplace_largesd = coverage_laplace_largesd, mse_laplace_largesd = mse_laplace_largesd, 
                   mcse_laplace_largesd = mcse_laplace_largesd, rhat_laplace_largesd = rhat_laplace_largesd, bulkess_laplace_largesd = bulkess_laplace_largesd,
                   tailess_laplace_largesd = tailess_laplace_largesd, postprob_laplace_largesd = postprob_laplace_largesd, numdivergent_laplace_largesd = numdivergent_laplace_largesd, stopping_laplace_largesd = stopping_laplace_largesd,
                   bias_laplace_smallsd = bias_laplace_smallsd, coverage_laplace_smallsd = coverage_laplace_smallsd, mse_laplace_smallsd = mse_laplace_smallsd, 
                   mcse_laplace_smallsd = mcse_laplace_smallsd, rhat_laplace_smallsd = rhat_laplace_smallsd, bulkess_laplace_smallsd = bulkess_laplace_smallsd,
                   tailess_laplace_smallsd = tailess_laplace_smallsd, postprob_laplace_smallsd = postprob_laplace_smallsd, numdivergent_laplace_smallsd = numdivergent_laplace_smallsd, stopping_laplace_smallsd = stopping_laplace_smallsd,
                   bias_cauchy = bias_cauchy, coverage_cauchy = coverage_cauchy, mse_cauchy = mse_cauchy, 
                   mcse_cauchy = mcse_cauchy, rhat_cauchy = rhat_cauchy, bulkess_cauchy = bulkess_cauchy,
                   tailess_cauchy = tailess_cauchy, postprob_cauchy = postprob_cauchy, numdivergent_cauchy = numdivergent_cauchy, stopping_cauchy = stopping_cauchy,
                   bias_freq = bias_freq, coverage_freq = coverage_freq, mse_freq = mse_freq, stopping_freq = stopping_freq
)  


data <- data[s1[datnum]:s2[datnum],]

save(data, file = paste0("sim_performance",datnum,".Rdata"))

end <- Sys.time()


