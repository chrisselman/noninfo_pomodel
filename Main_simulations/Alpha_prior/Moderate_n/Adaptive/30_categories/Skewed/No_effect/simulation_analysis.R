## Set working directory 
setwd("/group/cebu1/BACKUP/Chris/Project_3/Main_simulations/Alpha_prior/Moderate_n/Adaptive/30_categories/Skewed/No_effect")

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
states <- c("A", "B", "C","D","E","F","G","H","I","J",
            "K", "L", "M", "N", "O", "P","Q","R","S","T",
            "U","V","W","X","Y","Z","AA","BB","CC","DD")

#Generate empty vectors to store performance measures and diagnostic measures 
## Store bias for each analysis model  
bias_dirones <- bias_dirzero <- bias_dirjef <- bias_dirrec <- bias_normal <- length(nsim)  


## Store coverage for each analysis model  
coverage_dirones <- coverage_dirzero <- coverage_dirjef <- coverage_dirrec <- coverage_normal <- length(nsim)  


## Store MSE for each analysis model  
mse_dirones <- mse_dirzero <- mse_dirjef <- mse_dirrec <- mse_normal <- length(nsim)  


## Store posterior probability 
postprob_dirones <- postprob_dirzero <- postprob_dirjef <- postprob_dirrec <- postprob_normal <- length(nsim)

## Store R-hat for each parameter of each cumulative logit for each analysis model  
rhat_dirones <- rhat_dirzero <- rhat_dirjef <- rhat_dirrec <- rhat_normal <- length(nsim)  



## Store bulk ESS for each parameter of each cumulative logit for each analysis model  
bulkess_dirones <- bulkess_dirzero <- bulkess_dirjef <- bulkess_dirrec <- bulkess_normal <- length(nsim)  


## Store tail ESS for each parameter of each cumulative logit for each analysis model  
tailess_dirones <- tailess_dirzero <- tailess_dirjef <- tailess_dirrec <- tailess_normal <- length(nsim)  


## Store MCSE for each parameter of each cumulative logit for each analysis model  
mcse_dirones <- mcse_dirzero <- mcse_dirjef <- mcse_dirrec <- mcse_normal <- length(nsim)  


## Number of divergent transitions 
numdivergent_dirones <- numdivergent_dirzero <- numdivergent_dirjef <- numdivergent_dirrec <- numdivergent_normal <- length(nsim)  

## Frequentist results 
bias_freq <- coverage_freq <- mse_freq <- length(nsim)


## Stopped or not 
stopping_dirones <- stopping_dirzero <- stopping_dirjef <- stopping_dirrec <- stopping_normal <- stopping_freq <- length(nsim)


for (k in s1[datnum]:s2[datnum]){
  x <- big_data$x[big_data$k == k]
  y <- as.integer(big_data$y[big_data$k == k])
  
n_states = 30 ## Number of levels in ordinal outcome 
n_patients = as.integer(length(y)) # Sample size 


## METHOD 1 assuming normal prior with SD = 100 and alpha = 1
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
                    iter = 10000, chains = 4, seed = 22052024, cores = 4,
                   control = list(adapt_delta = 0.99, max_treedepth = 12)
                   )

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {
  

# Extract summary for proportional OR 
# Bias for each cumulative logit 
bias_dirones[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)


# Coverage - does the true value fall in the credible interval?
lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

coverage_dirones[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)

# MSE 
mse_dirones[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2

# Posterior probability 
postprob_dirones[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 


# MCSE 
mcse_dirones[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])

# R-hat 
rhat_dirones[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])

# Bulk ESS 
bulkess_dirones[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])

# Tail ESS 
tailess_dirones[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])


# Number of divergent transitions 
numdivergent_dirones[k] <- get_num_divergent(example_fit)

stopping_dirones[k] <- 1
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
                      iter = 10000, chains = 4, seed = 22052024, cores = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
  
    # Extract summary for proportional OR 
    # Bias for each cumulative logit 
    bias_dirones[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
    
    
    # Coverage - does the true value fall in the credible interval?
    lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
    uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
    
    coverage_dirones[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
    
    # MSE 
    mse_dirones[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
    
    # Posterior probability 
    postprob_dirones[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
    
    
    # MCSE 
    mcse_dirones[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
    
    # R-hat 
    rhat_dirones[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
    
    # Bulk ESS 
    bulkess_dirones[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
    
    # Tail ESS 
    tailess_dirones[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
    
    
    # Number of divergent transitions 
    numdivergent_dirones[k] <- get_num_divergent(example_fit)
    
    stopping_dirones[k] <- 0
    
}
  
  

## METHOD 2 assuming alpha = 0

example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(0.001, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
)

example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 12)
)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {

# Extract summary for proportional OR 
# Bias for each cumulative logit 
bias_dirzero[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)


# Coverage - does the true value fall in the credible interval?
lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

coverage_dirzero[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)

# MSE 
mse_dirzero[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2

# Posterior probability 
postprob_dirzero[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 

# MCSE 
mcse_dirzero[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])

# R-hat 
rhat_dirzero[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])

# Bulk ESS 
bulkess_dirzero[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])

# Tail ESS 
tailess_dirzero[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])


# Number of divergent transitions 
numdivergent_dirzero[k] <- get_num_divergent(example_fit)

stopping_dirzero[k] <- 1
} else 
{
  example_data <- list(
    y = y, ## Outcome 
    n_states = n_states, # Number of categories
    n_patients = n_patients, ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(0.001, n_states), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )
  
  example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
    
    # Extract summary for proportional OR 
    # Bias for each cumulative logit 
    bias_dirzero[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
    
    
    # Coverage - does the true value fall in the credible interval?
    lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
    uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
    
    coverage_dirzero[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
    
    # MSE 
    mse_dirzero[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
    
    # Posterior probability 
    postprob_dirzero[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
    
    # MCSE 
    mcse_dirzero[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
    
    # R-hat 
    rhat_dirzero[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
    
    # Bulk ESS 
    bulkess_dirzero[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
    
    # Tail ESS 
    tailess_dirzero[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
    
    
    # Number of divergent transitions 
    numdivergent_dirzero[k] <- get_num_divergent(example_fit)
    
    stopping_dirzero[k] <- 0
}


## METHOD 3 assuming alpha = 0.5 
example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(0.5, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
)

example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 12)
)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {

# Extract summary for proportional OR 
# Bias for each cumulative logit 
bias_dirjef[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)


# Coverage - does the true value fall in the credible interval?
lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

coverage_dirjef[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)

# MSE 
mse_dirjef[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2

# Posterior probability 
postprob_dirjef[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 

# MCSE 
mcse_dirjef[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])

# R-hat 
rhat_dirjef[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])

# Bulk ESS 
bulkess_dirjef[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])

# Tail ESS 
tailess_dirjef[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])


# Number of divergent transitions 
numdivergent_dirjef[k] <- get_num_divergent(example_fit)

stopping_dirjef[k] <- 1
} else 
{
  example_data <- list(
    y = y, ## Outcome 
    n_states = n_states, # Number of categories
    n_patients = n_patients, ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(0.5, n_states), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )
  
  example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
  
    
    # Extract summary for proportional OR 
    # Bias for each cumulative logit 
    bias_dirjef[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
    
    
    # Coverage - does the true value fall in the credible interval?
    lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
    uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
    
    coverage_dirjef[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
    
    # MSE 
    mse_dirjef[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
    
    # Posterior probability 
    postprob_dirjef[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
    
    # MCSE 
    mcse_dirjef[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
    
    # R-hat 
    rhat_dirjef[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
    
    # Bulk ESS 
    bulkess_dirjef[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
    
    # Tail ESS 
    tailess_dirjef[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
    
    
    # Number of divergent transitions 
    numdivergent_dirjef[k] <- get_num_divergent(example_fit)
    
    stopping_dirjef[k] <- 0
}
  


## METHOD 4 assuming alpha = 1/J
example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(1/n_states, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
)

example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 12)
)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {

# Extract summary for proportional OR 
# Bias for each cumulative logit 
bias_dirrec[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)


# Coverage - does the true value fall in the credible interval?
lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

coverage_dirrec[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)

# MSE 
mse_dirrec[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2

# Posterior probability 
postprob_dirrec[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 

# MCSE 
mcse_dirrec[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])

# R-hat 
rhat_dirrec[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])

# Bulk ESS 
bulkess_dirrec[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])

# Tail ESS 
tailess_dirrec[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])


# Number of divergent transitions 
numdivergent_dirrec[k] <- get_num_divergent(example_fit)

stopping_dirrec[k] <- 1
} else 
{
  example_data <- list(
    y = y, ## Outcome 
    n_states = n_states, # Number of categories
    n_patients = n_patients, ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1/n_states, n_states), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )

  example_fit <- stan(file = "po_model_normal.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]

    
    # Extract summary for proportional OR 
    # Bias for each cumulative logit 
    bias_dirrec[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
    
    
    # Coverage - does the true value fall in the credible interval?
    lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
    uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
    
    coverage_dirrec[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
    
    # MSE 
    mse_dirrec[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
    
    # Posterior probability 
    postprob_dirrec[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
    
    # MCSE 
    mcse_dirrec[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
    
    # R-hat 
    rhat_dirrec[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
    
    # Bulk ESS 
    bulkess_dirrec[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
    
    # Tail ESS 
    tailess_dirrec[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
    
    
    # Number of divergent transitions 
    numdivergent_dirrec[k] <- get_num_divergent(example_fit)
    
    stopping_dirrec[k] <- 0
}


## METHOD 5 assuming alpha is normal 
example_data <- list(
  y = y[1:(length(y)/2)], ## Outcome 
  n_states = n_states, # Number of categories
  n_patients = as.integer(n_patients/2), ## Sample size 
  x = x[1:(length(x)/2)], ## Treatment variable
  p_par = rep(1, n_states), ## Concentration parameter for prior on alpha 
  sdbeta = sdbeta ## Specify prior SD for log-OR 
)

example_fit <- stan(file = "po_model_alphnorm.stan", data = example_data,
                    iter = 10000, chains = 4, seed = 22052024, cores = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 12)
)

df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "beta"]

postprob <- (length(df$beta[df$beta > 0]))/length(df$beta)

if (postprob > 0.99) {

# Extract summary for proportional OR 
# Bias for each cumulative logit 
bias_normal[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)


# Coverage - does the true value fall in the credible interval?
lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

coverage_normal[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)

# MSE 
mse_normal[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2

# Posterior probability 
postprob_normal[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 

# MCSE 
mcse_normal[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])

# R-hat 
rhat_normal[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])

# Bulk ESS 
bulkess_normal[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])

# Tail ESS 
tailess_normal[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])


# Number of divergent transitions 
numdivergent_normal[k] <- get_num_divergent(example_fit)

stopping_normal[k] <- 1
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

  
  example_fit <- stan(file = "po_model_alphnorm.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]

    
    # Extract summary for proportional OR 
    # Bias for each cumulative logit 
    bias_normal[k] <- as.numeric(summarise_draws(df, "median")[1,2]) - log(1)
    
    
    # Coverage - does the true value fall in the credible interval?
    lci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
    uci <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
    
    coverage_normal[k] <- ifelse(lci <= log(1) & uci >= log(1), 1, 0)
    
    # MSE 
    mse_normal[k] <- (as.numeric(summarise_draws(df, "median")[1,2]) - log(1))^2
    
    # Posterior probability 
    postprob_normal[k] <- (length(df$beta[df$beta > 0]))/length(df$beta) 
    
    # MCSE 
    mcse_normal[k] <- as.numeric(summarise_draws(df, "mcse_median")[1,2])
    
    # R-hat 
    rhat_normal[k] <- as.numeric(summarise_draws(df, "rhat")[1,2])
    
    # Bulk ESS 
    bulkess_normal[k] <- as.numeric(summarise_draws(df, "ess_bulk")[1,2])
    
    # Tail ESS 
    tailess_normal[k] <- as.numeric(summarise_draws(df, "ess_tail")[1,2])
    
    
    # Number of divergent transitions 
    numdivergent_normal[k] <- get_num_divergent(example_fit)
    
    stopping_normal[k] <- 0
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
data <- data.frame(
                    bias_dirones = bias_dirones, coverage_dirones = coverage_dirones, mse_dirones = mse_dirones, 
                   mcse_dirones = mcse_dirones, rhat_dirones = rhat_dirones, bulkess_dirones = bulkess_dirones,
                   tailess_dirones = tailess_dirones, postprob_dirones = postprob_dirones, numdivergent_dirones = numdivergent_dirones, stopping_dirones = stopping_dirones,
                   bias_dirzero = bias_dirzero, coverage_dirzero = coverage_dirzero, mse_dirzero = mse_dirzero, 
                   mcse_dirzero = mcse_dirzero, rhat_dirzero = rhat_dirzero, bulkess_dirzero = bulkess_dirzero,
                   tailess_dirzero = tailess_dirzero, postprob_dirzero = postprob_dirzero, numdivergent_dirzero = numdivergent_dirzero, stopping_dirzero = stopping_dirzero,
                   bias_dirjef = bias_dirjef, coverage_dirjef = coverage_dirjef, mse_dirjef = mse_dirjef, 
                   mcse_dirjef = mcse_dirjef, rhat_dirjef = rhat_dirjef, bulkess_dirjef = bulkess_dirjef,
                   tailess_dirjef = tailess_dirjef, postprob_dirjef = postprob_dirjef, numdivergent_dirjef = numdivergent_dirjef, stopping_dirjef = stopping_dirjef,
                   bias_dirrec = bias_dirrec, coverage_dirrec = coverage_dirrec, mse_dirrec = mse_dirrec, 
                   mcse_dirrec = mcse_dirrec, rhat_dirrec = rhat_dirrec, bulkess_dirrec = bulkess_dirrec,
                   tailess_dirrec = tailess_dirrec, postprob_dirrec = postprob_dirrec, numdivergent_dirrec = numdivergent_dirrec, stopping_dirrec = stopping_dirrec,
                   bias_normal = bias_normal, coverage_normal = coverage_normal, mse_normal = mse_normal, 
                   mcse_normal = mcse_normal, rhat_normal = rhat_normal, bulkess_normal = bulkess_normal,
                   tailess_normal = tailess_normal, postprob_normal = postprob_normal, numdivergent_normal = numdivergent_normal, stopping_normal = stopping_normal,
                   bias_freq = bias_freq, coverage_freq = coverage_freq, mse_freq = mse_freq, stopping_freq = stopping_freq
)  

data <- data[s1[datnum]:s2[datnum],]

save(data, file = paste0("sim_performance",datnum,".Rdata"))

end <- Sys.time()


