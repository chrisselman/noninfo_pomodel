## Set working directory 
setwd("G:/BACKUP/Chris/Project_3/Main_simulations/Beta_prior/Moderate_n/Adaptive/30_categories/Symmetric/Small_effect")

# For simplicity assume fixed design, n = 50, OR = 1.50, J = 4, skewed probs
## Main simulation scenario 

set.seed(300524)
n = 500
nsim = 1000

## Generate simulation inputs 
OR      <- c(1/1.1) # Assume these PORs
logOR   <- log(OR)                     # Take their log.
sdlogOR <- c(0) # No departure from PO

# Munge simulation scenarios
scen <- expand.grid("logOR" = logOR)

## Define states and other parameters 
# 4 category outcome
states <- c("A", "B", "C","D","E","F","G","H","I","J",
            "K", "L", "M", "N", "O", "P","Q","R","S","T",
            "U","V","W","X","Y","Z","AA","BB","CC","DD")
iStates <- seq(states)

# Control probs in higher categories 
quant <- pbeta(c(1/length(states) * seq(1,length(states)-1,1)),0.5,0.5)
p0 <- c(diff(c(0,quant)),1-quant[length(states)-1])


## Load in the appropriate packages
library("truncnorm")



## Define functions
getProbs <- function(p0 = c(10, 40, 10,40)/100,
                     logOddsRatio, sdLogOR)
{
  cumProbs0   <- cumsum(p0)
  cumOdds0    <- cumProbs0/(1 - cumProbs0) 
  cumlogOdds0 <- log(cumOdds0)
  cumlogOdds1 <- rep(NA,length(p0))
  cumlogOdds1[length(p0)] <- Inf
  for(i in (length(p0) - 1):1){ 
    cumlogOdds1[i] <- cumlogOdds0[i] + rtruncnorm(n=1,b=(cumlogOdds1[i+1]-cumlogOdds0[i]),mean=logOddsRatio,
                                                  sd=sdLogOR)
  }
  cumOdds1    <- exp(cumlogOdds1)
  cumProbs1   <- cumOdds1/(1 + cumOdds1)
  cumProbs1[length(cumProbs1)] <- 1
  p1          <- diff(c(0,cumProbs1))
  p1
  
  
  ## Any negative probabilities?
  for(i in 1:length(p1)){
    if(p1[i] < 0)
      stop("Probabilities are negative")
  }
  
  p1
}


## Simulate a trial function 
simulateTrial <- function(logOddsRatio, 
                          nSamples0,
                          nSamples1,
                          sdLogOR,
                          p0     = c(20, 40, 30,10)/100,
                          states = c("A", "B", "C","D"))
{
  ## Define outcome probabilities for the treatment group
  p1   <- getProbs(p0, logOddsRatio, sdLogOR)
  p1
  
  # Simulate two arm trial data 
  dMulti0  <- rmultinom(1, size = nSamples0, prob = p0)
  dMulti1  <- rmultinom(1, size = nSamples1, prob = p1)
  sample0  <- rep(states, dMulti0) 
  sample1  <- rep(states, dMulti1) 
  sample0  <- factor(sample0, levels = states, ordered = T)
  sample1  <- factor(sample1, levels = states, ordered = T)
  
  # Munge simulated data.
  data           <- rbind(data.frame("a" = 0, "y" = sample0),
                          data.frame("a" = 1, "y" = sample1))
  data
}


## Function to make stan inputs.
makeStanData <- function(dat)
{
  
  z <- data.frame("y" = as.numeric(dat[["y"]]), 
                  "X" = as.numeric(dat[["a"]]))
  z <- z[["X"]] == 1 & z[["y"]] > 2
  stanData <- list(N = nrow(dat),
                   y = as.numeric(dat[["y"]]),
                   X = as.matrix((dat[["a"]])),
                   Z = as.matrix(as.numeric(z)),
                   p = 1,
                   q = 1,
                   k = length(states))
}


# Main simulation routine
random_number_generator_seeds <- matrix(0,nrow=nsim,ncol=626) # stores the random number generator seeds

# Store full dataset
df <- list()

for(k in 1:nsim){ 
  # Save the random number generator seed
  random_number_generator_seeds[k,] <- .Random.seed
  
  
  ## Simulate the data 
  # Sample size in each group 
  randtrt <- rbinom(n,1,c(0.5,0.5))  ## Ber(0.5)
  sampsize0 <- table(randtrt)[1]
  sampsize1 <- table(randtrt)[2]
  
  simData <- list()
  
  for(i in seq(nrow(scen)))
  { 
    dat <- list()
    for(j in seq(1))
    {
      print(c(i,j))
      dat[[j]] <- simulateTrial(nSamples0 = sampsize0, 
                                nSamples1 = sampsize1,
                                p0     = p0,
                                states = states,
                                logOddsRatio = with(scen,logOR[i]),
                                sdLogOR = with(scen,sdlogOR[i]))
    }
    simData[[i]] <- dat
  }
  
  
  # Make a single dataset  
  singleDat <- makeStanData(simData[[1]][[1]])
  dat <- data.frame(x=singleDat$X[,], y = singleDat$y, x1 = as.factor(singleDat$X[,]))
  dat$k <- k
  dat <- dat[sample(nrow(dat)),]
  df[[k]] <- dat 
}


big_data = do.call(rbind,df)

save(big_data, file = "sim_data.Rdata")

save(random_number_generator_seeds, file = "seeds.Rdata")
