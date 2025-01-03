## Simulate ASCOT data used in case study 


## Breathlessness scale 
states <- 0:4

## Define probabilities in control/treatment groups
breathprobs0 <- c(0.35,0.41,0.10,0.12,0.02)
breathprobs1 <- c(0.46,0.35,0.10,0.07,0.02)

## Simulate from a multinomial distribution 
set.seed(1234)
breath0 <- rmultinom(1, size = 115, prob = breathprobs0)
sampbreath0 <- rep(states, breath0) 
outbreath0 <- factor(sampbreath0, levels = states, ordered = T)

breath1 <- rmultinom(1, size = 110, prob = breathprobs1)
sampbreath1 <- rep(states, breath1) 
outbreath1 <- factor(sampbreath1, levels = states, ordered = T)

data           <- rbind(data.frame("a" = "Low Dose", "y" = outbreath0),
                        data.frame("a" = "Intermediate Dose", "y" = outbreath1))

save(data, file = "ascot_breathdata.Rdata")


## WHO scale 
states <- 1:8

## Define probabilities in control/treatment groups
whoprobs0 <- c(71,24.6,0.3,0.5,0.3,0,0,3.2)/100
whoprobs1 <- c(73.3,23.4,0,0.2,0,0.3,0.3,2.5)/100

## Simulate from a multinomial distribution 
set.seed(1234)
who0 <- rmultinom(1, size = 597, prob = whoprobs0)
sampwho0 <- rep(states, who0) 
outwho0 <- factor(sampwho0, levels = states, ordered = T)

who1 <- rmultinom(1, size = 607, prob = whoprobs1)
sampwho1 <- rep(states, who1) 
outwho1 <- factor(sampwho1, levels = states, ordered = T)

data           <- rbind(data.frame("a" = "Low Dose", "y" = outwho0),
                        data.frame("a" = "Intermediate Dose", "y" = outwho1))

save(data, file = "ascot_whodata.Rdata")


## Days Alive and Free of Ventilation 
states <- 0:28

## Define probabilities in control/treatment groups
ventprobs0 <- c(3.2,rep(0,8),0.2,0.2,0,0.3,rep(0,6),0.2,0.2,0,0,0.2,0.3,0.2,0.5,0.3,94.3)/100
ventprobs1 <- c(2.5,0.2,0.2,0,0,0.2,rep(0,14),0.2,0.2,0,0.2,0,0.2,0.2,0.3,95.6)/100

## Simulate from a multinomial distribution 
set.seed(1234)
vent0 <- rmultinom(1, size = 599, prob = ventprobs0)
sampvent0 <- rep(states, vent0) 
outvent0 <- factor(sampvent0, levels = states, ordered = T)

vent1 <- rmultinom(1, size = 607, prob = ventprobs1)
sampvent1 <- rep(states, vent1) 
outvent1 <- factor(sampvent1, levels = states, ordered = T)

data           <- rbind(data.frame("a" = "Low Dose", "y" = outvent0),
                        data.frame("a" = "Intermediate Dose", "y" = outvent1))

save(data, file = "ascot_ventfreedaysdata.Rdata")


## Days Alive and Free of Hospitalisation 
states <- 0:28

## Define probabilities in control/treatment groups
hospprobs0 <- c(4.2,0,0,0.2,0,0,0.2,0,0.2,0.2,0.7,0.2,1.3,0.5,0.2,0.5,1.3,1.7,1.8,3.7,5.5,8.8,13.7,22.3,16.3,13.2,2.7,0.8,0)/100
hospprobs1 <- c(3.3,0,0,0,0.2,0,0.2,0.2,0.2,0.2,0.2,0,0,0.2,0.2,0.2,1.3,0.8,1.6,2.3,6.2,9.7,15.4,23.5,15.8,13.8,4.6,0.2,0)/100

## Simulate from a multinomial distribution 
set.seed(1234)
hosp0 <- rmultinom(1, size = 600, prob = hospprobs0)
samphosp0 <- rep(states, hosp0) 
outhosp0 <- factor(samphosp0, levels = states, ordered = T)

hosp1 <- rmultinom(1, size = 609, prob = hospprobs1)
samphosp1 <- rep(states, hosp1) 
outhosp1 <- factor(samphosp1, levels = states, ordered = T)

data           <- rbind(data.frame("a" = "Low Dose", "y" = outhosp0),
                        data.frame("a" = "Intermediate Dose", "y" = outhosp1))

save(data, file = "ascot_hospfreedaysdata.Rdata")
