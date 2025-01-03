## Set working directory 
setwd("/group/cebu1/BACKUP/Chris/Project_3/ASCOT")

## Load in the appropriate packages
library("rstan")
library('posterior')
library("rstanarm")
library(readxl)

# Load the analysis set 
d <- read_xlsx("analysis.xlsx")

d <- d[,c(2,6)]
d <- na.omit(d)
attach(d)

## Breathlessness scale 
x1 = as.factor(d$trt)
x = d$trt
  
  ##################### Proportional odds model ##################
sdbeta = 100

example_data <- list(
  y = as.integer(d$daysfreeofvent)+1, ## Outcome 
  n_states = 29, # Number of categories
  n_patients = length(d$daysfreeofvent), ## Sample size 
  x = x, ## Treatment variable
  p_par = rep(1, 29), ## Concentration parameter for prior on alpha 
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
est_normal_largesd <- as.numeric(summarise_draws(df, "median")[1,2]) 


# Coverage - does the true value fall in the credible interval?
lci_normal_largesd <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci_normal_largesd <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])


postprob_normal_largesd <- (length(df$beta[df$beta > 0]))/length(df$beta)

## SD = 2.5
sdbeta = 2.5

example_data <- list(
  y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
  n_states = 29, # Number of categories
  n_patients = length(d$daysfreeofvent), ## Sample size 
  x = x, ## Treatment variable
  p_par = rep(1, 29), ## Concentration parameter for prior on alpha 
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
est_normal_smallsd <- as.numeric(summarise_draws(df, "median")[1,2]) 


# Coverage - does the true value fall in the credible interval?
lci_normal_smallsd <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci_normal_smallsd <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

postprob_normal_smallsd <- (length(df$beta[df$beta > 0]))/length(df$beta)


## Laplace 
sdbeta = 100/sqrt(2)

example_data <- list(
  y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
  n_states = 29, # Number of categories
  n_patients = length(d$daysfreeofvent), ## Sample size 
  x = x, ## Treatment variable
  p_par = rep(1, 29), ## Concentration parameter for prior on alpha 
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
est_laplace_largesd <- as.numeric(summarise_draws(df, "median")[1,2]) 


# Coverage - does the true value fall in the credible interval?
lci_laplace_largesd <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci_laplace_largesd <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])


postprob_laplace_largesd <- (length(df$beta[df$beta > 0]))/length(df$beta)


## Small SD laplace 
sdbeta = 2.5/sqrt(2)

example_data <- list(
  y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
  n_states = 29, # Number of categories
  n_patients = length(d$daysfreeofvent), ## Sample size 
  x = x, ## Treatment variable
  p_par = rep(1, 29), ## Concentration parameter for prior on alpha 
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
est_laplace_smallsd <- as.numeric(summarise_draws(df, "median")[1,2]) 


# Coverage - does the true value fall in the credible interval?
lci_laplace_smallsd <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci_laplace_smallsd <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])


postprob_laplace_smallsd <- (length(df$beta[df$beta > 0]))/length(df$beta)


## Cauchy 
sdbeta = 2.5/sqrt(2)

example_data <- list(
  y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
  n_states = 29, # Number of categories
  n_patients = length(d$daysfreeofvent), ## Sample size 
  x = x, ## Treatment variable
  p_par = rep(1, 29), ## Concentration parameter for prior on alpha 
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
est_cauchy <- as.numeric(summarise_draws(df, "median")[1,2]) 


# Coverage - does the true value fall in the credible interval?
lci_cauchy <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci_cauchy <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])


postprob_cauchy <- (length(df$beta[df$beta > 0]))/length(df$beta)


## R-square 
example_data <- list(
  y = as.factor(d$daysfreeofvent), ## Outcome 
  x = x) ## Treatment variable


example_fit <- stan_polr(as.factor(y)~x, prior = R2(0.5, "mean"), data = as.data.frame(example_data), 
                         algorithm = "sampling", iter = 10000, cores = 4, seed = 22052024, chains = 4, prior_counts = dirichlet(1))


df <- as_draws_df(example_fit)

# Just care about log-OR 
df <- df[, "x"]

# Extract summary for proportional OR 
# Bias for each cumulative logit 
est_rsquare <- as.numeric(summarise_draws(df, "median")[1,2]) 


# Coverage - does the true value fall in the credible interval?
lci_rsquare <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
uci_rsquare <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])

postprob_rsquare <- (dim(df[df$x > 0,])[1])/length(df$x) 




  
  ## Create data frame 
  est <- c(est_normal_largesd,est_laplace_largesd,est_normal_smallsd,est_laplace_smallsd,est_cauchy,est_rsquare)
  
  lci <- c(lci_normal_largesd,lci_laplace_largesd,lci_normal_smallsd,lci_laplace_smallsd,lci_cauchy,lci_rsquare)
  
  uci <- c(uci_normal_largesd,uci_laplace_largesd,uci_normal_smallsd,uci_laplace_smallsd,uci_cauchy,uci_rsquare)
  
  postprob <- c(postprob_normal_largesd,postprob_laplace_largesd,postprob_normal_smallsd,postprob_laplace_smallsd,postprob_cauchy,postprob_rsquare)
  
  Prior <- c("Normal (large K)", "Laplace (large K)", "Normal (small K)", "Laplace (small K)", "Cauchy", "R-square")
  
  data <- data.frame(est = est, lci = lci, uci = uci, Prior = Prior, postprob = postprob)
  save(data, file = "vent_beta_data.Rdata")
  write.csv(data,file="vent_beta_data.csv")
  library(ggplot2)
  pd <- position_dodge(0.68)
  
  ggplot(data, aes(x=Prior, y = est, color = Prior)) +
    #draws the means
    geom_point(position=pd, size = 2) +
    #draws the CI error bars
    geom_errorbar(data=data, aes(ymin=lci, ymax=uci, 
                                  color=Prior), width=0.5, size = 0.8, position=pd)  +
    labs(x = "Prior distribution", y = "Log-Odds Ratio", fill = "Prior") +
    geom_hline(yintercept = 0, linetype = "twodash") +
    theme(axis.text=element_text(size=12),
    axis.title=element_text(size=14,face="bold"))
  ggsave("casestudy_vent_beta.png",width = 12, height = 12)
  
  
## Do the exact same but changing the alpha priors 
  ##################### Proportional odds model ##################
  sdbeta = 100
  
  example_data <- list(
    y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
    n_states = 29, # Number of categories
    n_patients = length(d$daysfreeofvent), ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1, 29), ## Concentration parameter for prior on alpha 
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
  est_dirones <- as.numeric(summarise_draws(df, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  lci_dirones <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci_dirones <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  postprob_dirones <- (length(df$beta[df$beta > 0]))/length(df$beta)
  
  ## Conc = 0.5 

  example_data <- list(
    y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
    n_states = 29, # Number of categories
    n_patients = length(d$daysfreeofvent), ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(0.5, 29), ## Concentration parameter for prior on alpha 
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
  est_dirjef <- as.numeric(summarise_draws(df, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  lci_dirjef <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci_dirjef <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  postprob_dirjef <- (length(df$beta[df$beta > 0]))/length(df$beta)
  
  
  ## 1/J
  
  example_data <- list(
    y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
    n_states = 29, # Number of categories
    n_patients = length(d$daysfreeofvent), ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1/29, 29), ## Concentration parameter for prior on alpha 
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
  est_dirrec <- as.numeric(summarise_draws(df, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  lci_dirrec <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci_dirrec <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  postprob_dirrec <- (length(df$beta[df$beta > 0]))/length(df$beta)
  
  
  ## Small J = 0.001
  
  example_data <- list(
    y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
    n_states = 29, # Number of categories
    n_patients = length(d$daysfreeofvent), ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(0.001, 29), ## Concentration parameter for prior on alpha 
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
  est_dirzero <- as.numeric(summarise_draws(df, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  lci_dirzero <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci_dirzero <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  postprob_dirzero <- (length(df$beta[df$beta > 0]))/length(df$beta)
  
  
  ## Normal
  
  example_data <- list(
    y = as.integer(d$daysfreeofvent) + 1, ## Outcome 
    n_states = 29, # Number of categories
    n_patients = length(d$daysfreeofvent), ## Sample size 
    x = x, ## Treatment variable
    p_par = rep(1, 29), ## Concentration parameter for prior on alpha 
    sdbeta = sdbeta ## Specify prior SD for log-OR 
  )
  
  example_fit <- stan(file = "po_model_alphnorm.stan", data = example_data,
                      iter = 10000, chains = 4, seed = 22052024, cores = 4,
                       control = list(adapt_delta = 0.9, max_treedepth = 12)
  )
  
  df <- as_draws_df(example_fit)
  
  # Just care about log-OR 
  df <- df[, "beta"]
  
  # Extract summary for proportional OR 
  # Bias for each cumulative logit 
  est_normal <- as.numeric(summarise_draws(df, "median")[1,2]) 
  
  
  # Coverage - does the true value fall in the credible interval?
  lci_normal <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,2])
  uci_normal <- as.numeric(summarise_draws(df, function(x) quantile(x, probs = c(0.025, 0.975)))[1,3])
  
  
  postprob_normal <- (length(df$beta[df$beta > 0]))/length(df$beta)
  

  
  ## Create data frame 
  est <- c(est_dirones,est_dirjef,est_dirrec,est_dirzero,est_normal)
  
  lci <- c(lci_dirones,lci_dirjef,lci_dirrec,lci_dirzero,lci_normal)
  
  uci <- c(uci_dirones,uci_dirjef,uci_dirrec,uci_dirzero,uci_normal)
  
  postprob <- c(postprob_dirones,postprob_dirjef,postprob_dirrec,postprob_dirzero,postprob_normal)
  
  Prior <- c("Dirichlet(1)", "Dirichlet(0.5)", "Dirichlet(1/5)", "Dirichlet(0.001)", "Normal (large K)")
  
  data <- data.frame(est = est, lci = lci, uci = uci, Prior = Prior, postprob = postprob)
  save(data, file = "vent_alpha_data.Rdata")
  write.csv(data,file="vent_alpha_data.csv")
  library(ggplot2)
  pd <- position_dodge(0.68)
  
  ggplot(data, aes(x=Prior, y = est, color = Prior)) +
    #draws the means
    geom_point(position=pd, size = 2) +
    #draws the CI error bars
    geom_errorbar(data=data, aes(ymin=lci, ymax=uci, 
                                 color=Prior), width=0.5, size = 0.8, position=pd)  +
    labs(x = "Prior distribution", y = "Log-Odds Ratio", fill = "Prior") +
    geom_hline(yintercept = 0, linetype = "twodash") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
  ggsave("casestudy_vent_alpha.png",width = 12, height = 12)
  