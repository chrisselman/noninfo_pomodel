library(readxl)

data <- read_excel('dataforgraph_alpha_sens.xlsx')

library(tidyverse)

attach(data)
library(ggplot2)
library(ggpubr)
library(ggh4x)

myTheme <- theme(legend.text = element_text(size = 10), 
                 legend.title = element_text(size = 12), 
                 legend.key.size = unit(1, 'cm'),
                 plot.title = element_text (size = 14, face="bold"),
                 axis.title=element_text(size=16),
                 axis.text=element_text(size=16),
                 strip.text.x = element_text(size = 14),
                 strip.text.y = element_text(size = 14))

## Data generating mechanism: PO for fixed design 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[8] = "Bias"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$Bias <- round(df$Bias,3)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                       levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                       labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                         levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                         labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))

category <- c("4","10","30")
ggplot(df, aes(x = category, y = `Prior Distribution`, fill = Bias)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Bias), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-0.75, 0.75)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme
ggsave("table_alpha_bias_fixed_sens.png",width = 20, height = 15)


## Data generating mechanism: PO for adaptive design 
## BIAS 
## Rename some variables 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[8] = "Bias"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$Bias <- round(df$Bias,3)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))

category <- c("4","10","30")
ggplot(df, aes(x = category, y = `Prior Distribution`, fill = Bias)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Bias), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-0.75, 0.75)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme
ggsave("table_alpha_bias_adaptive_sens.png",width = 20, height = 15)



## Data generating mechanism: fixed
## COVERAGE  
## Rename some variables 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[10] = "Coverage"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"


df$Coverage <- round(df$Coverage,1)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))

ggplot(df, aes(x = category, y = `Prior Distribution`, fill = Coverage)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Coverage), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral3","white","lightblue"),limits=c(0, 100), values=c(0, 0.95, 1)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme
ggsave("table_alpha_coverage_fixed_sens.png",width = 20, height = 15)


## Data generating mechanism: adaptive
## COVERAGE  
## Rename some variables 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[10] = "Coverage"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"


df$Coverage <- round(df$Coverage,1)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))

ggplot(df, aes(x = category, y = `Prior Distribution`, fill = Coverage)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = Coverage), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral3","white","lightblue"),limits=c(0, 100), values=c(0, 0.95, 1)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme
ggsave("table_alpha_coverage_adaptive_sens.png",width = 20, height = 15)


## MSE - PO using fixed design 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[14] = "Mean Squared Error"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Mean Squared Error` <- round(df$`Mean Squared Error`,3)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Mean Squared Error`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Mean Squared Error`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.8)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_mse_fixed_sens.png",width = 20, height = 15)


## MSE - PO using adaptive design 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[14] = "Mean Squared Error"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Mean Squared Error` <- round(df$`Mean Squared Error`,3)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Mean Squared Error`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Mean Squared Error`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.8)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_mse_adaptive_sens.png",width = 20, height = 15)



## Data generating mechanism: PO for fixed design 
## RELATIVE BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[11] = "Relative Bias (%)"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Relative Bias (%)` <- round(df$`Relative Bias (%)`,1)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))

category <- c("4","10","30")
ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Relative Bias (%)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias (%)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-75, 75)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme
ggsave("table_alpha_relbias_fixed_sens.png",width = 20, height = 15)


## Data generating mechanism: PO for adaptive design 
## RELATIVE BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[11] = "Relative Bias (%)"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Relative Bias (%)` <- round(df$`Relative Bias (%)`,1)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))

category <- c("4","10","30")
ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Relative Bias (%)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias (%)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("coral2","white","cornflowerblue"),limits=c(-75, 75)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme
ggsave("table_alpha_relbias_adaptive_sens.png",width = 20, height = 15)



## Posterior probabilities - PO using fixed design 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[16] = "Pr(OR > 1 | Data)"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Pr(OR > 1 | Data)` <- round(df$`Pr(OR > 1 | Data)`,3)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Pr(OR > 1 | Data)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Pr(OR > 1 | Data)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","forestgreen"),limits=c(0, 1), values=c(0, 1)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_postprob_fixed_sens.png",width = 20, height = 15)


## Posterior probabilities - PO using adaptive design 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[16] = "Pr(OR > 1 | Data)"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Pr(OR > 1 | Data)` <- round(df$`Pr(OR > 1 | Data)`,3)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Pr(OR > 1 | Data)`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Pr(OR > 1 | Data)`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","forestgreen"),limits=c(0, 1), values=c(0, 1)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_postprob_adaptive_sens.png",width = 20, height = 15)




## Proportion declaring superiority - PO using fixed design 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[18] = "% declaring superiority"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`% declaring superiority` <- round(df$`% declaring superiority`,1)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `% declaring superiority`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `% declaring superiority`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","cornflowerblue"),limits=c(0, 100), values=c(0, 1)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_propsuperiority_fixed_sens.png",width = 20, height = 15)





## Proportion declaring superiority - PO using fixed design 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[18] = "% declaring superiority"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`% declaring superiority` <- round(df$`% declaring superiority`,1)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `% declaring superiority`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `% declaring superiority`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","cornflowerblue"),limits=c(0, 100), values=c(0, 1)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_propsuperiority_adaptive_sens.png",width = 20, height = 15)




## Proportion of adaptive designs stopping early 
data <- read_excel('dataforgraph_alpha_sens.xlsx')

attach(data)
names(data)
names(data)[20] = "% stopping early"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`% stopping early` <- round(df$`% stopping early`,1)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `% stopping early`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `% stopping early`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","cornflowerblue"),limits=c(0, 100), values=c(0, 1)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_propstopearly_adaptive_sens.png",width = 20, height = 15)




## Summarise MCSEs 
## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[9] = "Bias MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Bias MCSE` <- round(df$`Bias MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Bias MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_bias_mcse_fixed_sens.png",width = 20, height = 15)


## BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[9] = "Bias MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Bias MCSE` <- round(df$`Bias MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Bias MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_bias_mcse_adaptive_sens.png",width = 20, height = 15)




## Summarise MCSEs 
## RELATIVE BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[12] = "Relative Bias MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Relative Bias MCSE` <- round(df$`Relative Bias MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Relative Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_relbias_mcse_fixed_sens.png",width = 20, height = 15)


## Summarise MCSEs - adaptive
## RELATIVE BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[12] = "Relative Bias MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Relative Bias MCSE` <- round(df$`Relative Bias MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Relative Bias MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Relative Bias MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_relbias_mcse_adaptive_sens.png",width = 20, height = 15)




## Summarise MCSEs 
## RELATIVE BIAS 
## Rename some variables 
attach(data)
names(data)
names(data)[13] = "Coverage MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Coverage MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Coverage MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_coverage_mcse_fixed_sens.png",width = 20, height = 15)


## Summarise MCSEs - adaptive
## COVERAGE 
## Rename some variables 
attach(data)
names(data)
names(data)[13] = "Coverage MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Coverage MCSE` <- round(df$`Coverage MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Coverage MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Coverage MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_coverage_mcse_adaptive_sens.png",width = 20, height = 15)




## Summarise MCSEs 
## MSE
## Rename some variables 
attach(data)
names(data)
names(data)[15] = "Mean-Squared Error MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Mean-Squared Error MCSE` <- round(df$`Mean-Squared Error MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Mean-Squared Error MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Mean-Squared Error MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_mse_mcse_fixed_sens.png",width = 20, height = 15)


## Summarise MCSEs - adaptive
## MSE 
## Rename some variables 
attach(data)
names(data)
names(data)[15] = "Mean-Squared Error MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Mean-Squared Error MCSE` <- round(df$`Mean-Squared Error MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Mean-Squared Error MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Mean-Squared Error MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_mse_mcse_adaptive_sens.png",width = 20, height = 15)



## Summarise MCSEs 
## Posterior probs 
## Rename some variables 
attach(data)
names(data)
names(data)[17] = "Pr(OR > 1 | Data) MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Pr(OR > 1 | Data) MCSE` <- round(df$`Pr(OR > 1 | Data) MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Pr(OR > 1 | Data) MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Pr(OR > 1 | Data) MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_postprob_mcse_fixed_sens.png",width = 20, height = 15)


## Summarise MCSEs - adaptive
## Posterior prob  
## Rename some variables 
attach(data)
names(data)
names(data)[17] = "Pr(OR > 1 | Data) MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Pr(OR > 1 | Data) MCSE` <- round(df$`Pr(OR > 1 | Data) MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Pr(OR > 1 | Data) MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Pr(OR > 1 | Data) MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_postprob_mcse_adaptive_sens.png",width = 20, height = 15)



## Summarise MCSEs 
## Proportion declaring superiority 
## Rename some variables 
attach(data)
names(data)
names(data)[19] = "Proportion declaring superiority MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Fixed",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Fixed Design"

df$`Proportion declaring superiority MCSE` <- round(df$`Proportion declaring superiority MCSE`,5)


df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Proportion declaring superiority MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Proportion declaring superiority MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_postsup_mcse_fixed_sens.png",width = 20, height = 15)


## Summarise MCSEs - adaptive
## Posterior prob  
## Rename some variables 
attach(data)
names(data)
names(data)[19] = "Proportion declaring superiority MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Proportion declaring superiority MCSE` <- round(df$`Proportion declaring superiority MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Proportion declaring superiority MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Proportion declaring superiority MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_postsup_mcse_adaptive_sens.png",width = 20, height = 15)


## Summarise MCSEs - adaptive
## Posterior prob  
## Rename some variables 
attach(data)
names(data)
names(data)[21] = "Proportion of trials stopping early MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Proportion declaring superiority MCSE` <- round(df$`Proportion declaring superiority MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Proportion declaring superiority MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Proportion declaring superiority MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.20)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_postsup_mcse_adaptive_sens.png",width = 20, height = 15)

## Summarise MCSEs - adaptive
## 
## Rename some variables 
attach(data)
names(data)
names(data)[21] = "Proportion of trials stopping MCSE"
names(data)[3] = "Prior Distribution"
attach(data)
df <- data[data$design == "Adaptive",]
df <- df[df$`Prior Distribution` != "Frequentist",] # Don't want frequentist

names(df)[5] = "Adaptive Design"

df$`Proportion of trials stopping MCSE` <- round(df$`Proportion of trials stopping MCSE`,5)

df$sampsize[df$sampsize == 100] <- "n = 100"
df$sampsize[df$sampsize == 500] <- "n = 500"
df$controlprob[df$controlprob == "Skewed"] <- "Skewed control probabilities"
df$controlprob[df$controlprob == "Symmetric"] <- "U-shaped control probabilities"
df$controlprob[df$controlprob == "Uniform"] <- "Uniform control probabilities"
df$category[df$category == 4] <- "4 categories"
df$category[df$category == 10] <- "10 categories"
df$category[df$category == 30] <- "30 categories"
df$category <- ordered(df$category,
                       levels = c("4 categories", "10 categories", "30 categories"),
                       labels = c("4 categories", "10 categories", "30 categories"))  
df$effectsize <- ordered(df$effectsize,
                         levels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"),
                         labels = c("None [OR = 1.00]", "Small [OR = 1.10]", "Moderate [OR = 1.50]"))
df$`Prior Distribution` <- ordered(df$`Prior Distribution`,
                                   levels = c("Dirichlet (0)", "Dirichlet (1/J)", "Dirichlet (0.5)", "Dirichlet (1)","Normal"),
                                   labels = c("Dirichlet (α = 0.001)", "Dirichlet (α = 1/J)", "Dirichlet (α = 0.5)", "Dirichlet (α = 1)","Normal - large SD"))


ggplot(df, aes(x = category, y = `Prior Distribution`, fill = `Proportion of trials stopping MCSE`)) +
  geom_tile(color = "black") + 
  geom_text(aes(label = `Proportion of trials stopping MCSE`), color = "black", size = 4) +
  coord_fixed()+
  scale_fill_gradientn(colours=c("white","darksalmon"),limits=c(0, 0.2)) +
  facet_grid2(controlprob~effectsize+sampsize) +
  labs(x = "Number of categories", y = "Prior Distribution for α") +
  scale_x_discrete(labels = category) +
  myTheme 
ggsave("table_alpha_propstop_mcse_adaptive_sens.png",width = 20, height = 15)