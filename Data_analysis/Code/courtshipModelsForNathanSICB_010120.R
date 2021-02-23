## Courtship data from Intercross Study
## Do males differ in behavior before vs after experience?
## Testing models
## 1 January 2020
## Laura R Stein

###Bringing in relevant packages and data

rm(list=ls())

#Packages
library(tidyverse)
library(MASS)
library(lsmeans)
library(phia)

#Working directory and data
setwd("C:/Users/User/Desktop/Nathan/")
data <- read_csv("FINAL_court.csv")

#Checking data
head(data)
str(data)

#Change everything to factors that need to be for proper analysis

#First, create the character set
data_char <- data %>%
  mutate(Pop=as.character(Pop),
         char_column=sample(letters[1:22], nrow(data), replace=TRUE))
sum(sapply(data_char, is.character)) 

#Then, apply factor mutation
data_factor <- data_char %>%
  mutate_if(sapply(data_char, is.character), as.factor)

#check
sapply(data_factor, class)


##Sigmoids
#Negative binomial distribution

PQL.Sig <- glmmPQL(Sigmoid ~ Stage*Pop,
        random = ~ 1|Family/ID,
        family = negative.binomial(theta = 1),
        data = data_factor)

summary(PQL.Sig)
#Nothing significant
#Run again without interaction term
PQL.Sig <- glmmPQL(Sigmoid ~ Stage + Pop,
                   random = ~ 1|Family/ID,
                   family = negative.binomial(theta = 1),
                   data = data_factor)

#If you want to do more post-hoc tests for Pop, do it here. Note we already have contrasts for Before and After from your summary:
marginal <- lsmeans(PQL.Sig, ~Pop)
CLD.Sig <- cld(marginal,
               alpha=0.05,
               Letters=letters,
               adjust="tukey")
CLD.Sig

#And formally testing all levels of interactions
testInteractions(PQL.Sig)

##Forced Copulations
#Negative binomial distribution

PQL.For <- glmmPQL(Forced_Cop_Attempt ~ Stage*Pop,
                   random = ~ 1|Family/ID,
                   family = negative.binomial(theta = 1),
                   data = data_factor)

summary(PQL.For)
#Marginal effect of Population
#Run again without interaction
PQL.For <- glmmPQL(Forced_Cop_Attempt ~ Stage + Pop,
                   random = ~ 1|Family/ID,
                   family = negative.binomial(theta = 1),
                   data = data_factor)
# Significant differences between CM and all 3 populations. Marginal effect between Before and After (p = 5.17)

#Post-hoc
marginal <- lsmeans(PQL.For, ~Pop)
CLD.For <- cld(marginal,
               alpha=0.05,
               Letters=letters,
               adjust="tukey")
CLD.For

#And formally testing all levels of interactions
testInteractions(PQL.For)

#CQ and QC differ both from each other and before vs after. Marginal difference in QC vs QH Before and After as well.


##Gonopodium swings
#Negative binomial distribution

PQL.G_Swing <- glmmPQL(G_Swing ~ Stage*Pop,
                   random = ~ 1|Family/ID,
                   family = negative.binomial(theta = 1),
                   data = data_factor)

summary(PQL.G_Swing)
#Significant effect of Population and Stage, but not their interaction
#Run again without interaction effect
PQL.G_Swing <- glmmPQL(G_Swing ~ Stage + Pop,
                       random = ~ 1|Family/ID,
                       family = negative.binomial(theta = 1),
                       data = data_factor)
#Significant difference between CM and all 3 other populations

#Post-hoc for Pop:
marginal <- lsmeans(PQL.G_Swing, ~Pop)
CLD.G_Swing <- cld(marginal,
               alpha=0.05,
               Letters=letters,
               adjust="tukey")
CLD.G_Swing

#CM is different from all other populations. Other populations do not differ from each other.
#Note that we already know CM is different from the others from the summary contrast.

#And formally testing all levels of interactions
testInteractions(PQL.G_Swing)






