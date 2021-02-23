#MCMC Modeling for Intercross Courtship Experiment
#Nathan Phipps
#12/5/2019
#Last updated 2/18/2020

#Setting WD and reading in file
setwd("C:/Users/phipp/OneDrive/Documents/Grad School/Courtship Analysis")
mydata<-read.csv("FINAL_court.csv")

#Calling packages
library(tidyverse)
library(lmerTest)
library(plyr)
library(dplyr)
library(car)
require(MASS)
library(mlmRev)
library(agridat)
library(MCMCglmm)
library(ggplot2)

#Parsed with column specifications
cols(ID = col_integer(),
     Pop = col_character(),
     Family = col_integer(),
     DateBorn = col_character(),
     Sex = col_character(),
     Length = col_double(),
     Stage = col_character(),
     Date = col_character(),
     Coder = col_character(),
     Chase = col_integer(),
     Forced_Cop_Attempt = col_integer(),
     G_Swing = col_integer(),
     Sigmoid = col_integer(),
     Suc_Sigmoid_Cop = col_integer(),
     Ignore = col_integer(),
     Lunge = col_integer(),
     Nip = col_integer(),
     Suc_Forced_Cop = col_integer(),
     Copulated = col_character(),
     TTSigmoid = col_integer(),
     TTAvgSigmoid = col_integer())

#Turning the necessary columns into the factors to be used in analysis
mydata %>%
  mutate(Pop = as.factor(Pop)) %>%
  mutate(Stage = as.factor(Stage)) %>%
  mutate(Family = as.factor(Family)) %>%
  mutate(ID = as.factor(ID)) -> mydata

#Change column name to "famil" to avoid MCMC function confusion, and check that it worked #
colnames(mydata)[colnames(mydata) == "Family"] <- "famil"
head(mydata)

#Fitting Gaussian regression model using Pop*Stage interaction
fit_gaussian_Sig <- MCMCglmm(Sigmoid ~ Pop*Stage,
                         random = ~idv(1):famil + idv(1):ID,
                         data = mydata,
                         nitt=20000)
fit_gaussian_Cop <- MCMCglmm(Forced_Cop_Attempt ~ Pop*Stage,
                             random = ~idv(1):famil + idv(1):ID,
                             data = mydata,
                             nitt=20000)
fit_gaussian_Swing <- MCMCglmm(G_Swing ~ Pop*Stage,
                               random = ~idv(1):famil + idv(1):ID,
                               data = mydata,
                               nitt=20000)


#Fitting Poisson regression model using Pop*Stage interaction
fit_poisson_Sig <- MCMCglmm(Sigmoid ~ Pop*Stage,
                            random = ~idv(1):famil + idv(1):ID,
                            family = "poisson", data = mydata,
                            nitt=20000)
fit_poisson_Cop <- MCMCglmm(Forced_Cop_Attempt ~ Pop*Stage,
                            random = ~idv(1):famil + idv(1):ID,
                            family = "poisson", data = mydata,
                            nitt=20000)
fit_poisson_Swing <- MCMCglmm(G_Swing ~ Pop*Stage,
                            random = ~idv(1):famil + idv(1):ID,
                            family = "poisson", data = mydata,
                            nitt=20000)

#Using glmer for regression as comparison
fit_glmer <- glmer(Sigmoid ~ Pop*Stage +
                     (1|famil/ID),
                   family = "poisson", data = mydata)
    #Bad things? I don't understand

#Examining results
summary(fit_gaussian_Sig)
summary(fit_gaussian_Cop)
summary(fit_gaussian_Swing)

str(fit_gaussian_Sig)

summary(fit_poisson_Sig)
summary(fit_poisson_Cop)
summary(fit_poisson_Swing)

summary(fit_glmer)

summary.MCMCglmm(fit_gaussian_Swing) #This returns the same results as the regular summary() function

?MCMCglmm

#Trying emmeans to find pairwise comparisons
library(emmeans)
emmeans(fit_gaussian_Cop)



######### David Brown's Example Model ######

# Loading the packages
library(MCMCglmm)
library(lme4)

# Creating the data labels
variety_vals <- c("AA","AB","BA","BB")
family_vals <- 1:20
fish <- rep(1:200, each = 2)
famil <- rep(family_vals, each = 20)
variety <- rep(variety_vals, each = 100)
experience <- rep(0:1, times = 200)

table(famil,variety)

# Generating random effects 
family_eff <- rnorm(20, 1)
fish_eff <- rnorm(200, .3)
# Generating response from Poisson regression model with random effects
cop_attempt <- rpois(400,
                     exp(2+.5*(variety =='AA') + .2*(variety == 'BB')-
                           1*(variety == 'AB') - .5*(variety == 'BA') -
                           .5*experience + family_eff[famil] +
                           fish_eff[fish]))
# Saving the dataset
dataset <- data.frame(fish, famil, variety, experience, cop_attempt)


# Fitting Gaussian regression model
fit_gaussian <- MCMCglmm(cop_attempt ~ variety*experience,
                         random = ~idv(1):famil + idv(1):fish,
                         data = dataset)
# Fitting Poisson regression model
fit_poisson <- MCMCglmm(cop_attempt ~ variety + experience,
                        random = ~idv(1):famil + idv(1):fish,
                        family = "poisson",
                        data = dataset)

# Using "glmer" for Poisson regression as a comparison
fit_glmer <- glmer(cop_attempt ~ variety + experience +
                     (1|famil/fish),
                   family = "poisson", data = dataset)

# Examining results
summary(fit_gaussian)
summary(fit_poisson)
summary(fit_glmer)


summary.MCMCglmm(fit_gaussian)


