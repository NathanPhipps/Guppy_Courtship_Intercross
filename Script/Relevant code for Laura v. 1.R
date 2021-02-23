#Relevant code for Laura v. 1
#Nathan Phipps
#12/5/2019
#Last updated 12/5/2019

#Setting WD and reading in file
setwd("C:/Users/phipp/OneDrive/Documents/Grad School/Courtship Analysis")
mydata<-read.csv("FINAL_court.csv")
court<-read.csv("FINAL_court.csv")

#Calling packages

library(tidyverse)
library(lmerTest)
library(plyr)
library(car)
require(MASS)
library(mlmRev)
library(agridat)
library(MCMCglmm)
library(ggplot2)

data <- read_csv("FINAL_court.csv")

data_char <- data %>%
  mutate(Pop=as.character(Pop),
         char_column=sample(letters[1:5], nrow(data), replace=TRUE))

sum(sapply(data_char, is.character)) 

#Parsed with column specification:
  cols(
  .default = col_double(),
 Pop = col_character(),
     DateBorn = col_character(),
     Sex = col_character(),
     Stage = col_character(),
     Date = col_character(),
     Coder = col_character(),
     Copulated = col_character()
 )
  
#Apply factor mutation to characters
  data_factor <- data_char %>%
    mutate_if(sapply(data_char, is.character), as.factor)
  
        #Check that it worked
 sapply(data_factor, class)

 #Visualize data in histogram
 hist(data_factor$Forced_Cop_Attempt)
 hist(data_factor$Sigmoid)
 hist(data_factor$G_Swing)
 


#Statistical tests for best distribution#
   #Forced copulation attempts #####
data_factor$Forced_Cop_Attempt.t <- data_factor$Forced_Cop_Attempt + 1 

 #normal test
 qqp(data_factor$Forced_Cop_Attempt.t, "norm")
 
 #lognormal
 qqp(data_factor$Forced_Cop_Attempt.t, "lnorm")
 
 nbinom <- fitdistr(data_factor$Forced_Cop_Attempt.t, "Negative Binomial")
 qqp(data_factor$Forced_Cop_Attempt.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
  
 #poisson
 poisson <- fitdistr(data_factor$Forced_Cop_Attempt.t, "Poisson")
qqp(data_factor$Forced_Cop_Attempt.t, "pois", lambda = poisson$estimate)

 #gamma
gamma <- fitdistr(data_factor$Forced_Cop_Attempt.t, "gamma")
qqp(data_factor$Forced_Cop_Attempt.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]]) 
 
 
   #Sigmoids####
data_factor$Sigmoid.t <- data_factor$Sigmoid + 1
qqp(data_factor$Sigmoid.t, "norm")

   #lognormal
qqp(data_factor$Sigmoid.t, "lnorm")

   #negative binomial
nbinom <- fitdistr(data_factor$Sigmoid.t, "Negative Binomial")
qqp(data_factor$Sigmoid.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

   #poisson
poisson <- fitdistr(data_factor$Sigmoid.t, "Poisson")
qqp(data_factor$Sigmoid.t, "pois", lambda = poisson$estimate)

   #gamma
gamma <- fitdistr(data_factor$Sigmoid.t, "gamma")
qqp(data_factor$Sigmoid.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

   #Gonopoodium Swings ####
data_factor$G_Swing.t <- data_factor$G_Swing + 1   

  #normal
qqp(data_factor$G_Swing.t, "norm")

  #lognorm
qqp(data_factor$G_Swing.t, "lnorm")

  #Negative Binomial
nbinom <- fitdistr(data_factor$G_Swing.t, "Negative Binomial")
qqp(data_factor$G_Swing.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

  #poisson
poisson <- fitdistr(data_factor$G_Swing.t, "Poisson")
qqp(data_factor$G_Swing.t, "pois", lambda = poisson$estimate)

  #gamma
gamma <- fitdistr(data_factor$G_Swing.t, "gamma")
qqp(data_factor$G_Swing.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

#Model comparisons and residual tests
#Gonopodium Swings ####
try(glmm.normal <- lmer(G_Swing~Pop*Stage + (1|Family/ID), data = data_factor))
try(glmm.a <- glmer.nb(G_Swing~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom"))
try(glmm.b <- glmer.nb(G_Swing~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom1"))
try(glmm.c <- glmer(G_Swing~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link = "log")))
try(glmm.d <- glmer(G_Swing~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link="identity")))
try(glmm.e<- glmer(G_Swing~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link="sqrt")))
try(glmm.f <- glmer.nb(G_Swing~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom2"))

plot(glmm.a, which = "cook")


#Compare the AIC
AIC(glmm.normal, glmm.a, glmm.b, glmm.c, glmm.e)
      #The lower the better. Negative binomial parameters work best, and are equivalent

#Boxplots to find outliers and check if residuals are centered around zero for each factor
augDat <- data.frame(data_factor, resid = residuals(glmm.a, type = "pearson"))

ggplot(augDat, aes(x = Pop, y =  resid, col = Stage)) +
  geom_point() +
  geom_boxplot(aes(group=Pop), alpha = 0.1) +
  coord_flip()

#Check for zero inflation

try(glmm.zero<-glmer.nb(G_Swing~Pop*Stage+ (1|Family/ID), data=data_factor,family="nbinom", zeroInflation = T))
try(glmm.zero<-glmer.nb(G_Swing~Pop*Stage+ (1|Family/ID), data=data_factor,family="nbinom", zeroInflation = T))
AIC(glmm.zero,glmm.a)

#Forced Copulation Attempts####

try(glmm.normal <- lmer(Forced_Cop_Attempt~Pop*Stage + (1|Family/ID), data = data_factor))
try(glmm.a <- glmer.nb(Forced_Cop_Attempt~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom"))
try(glmm.b <- glmer.nb(Forced_Cop_Attempt~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom1"))
try(glmm.c <- glmer(Forced_Cop_Attempt~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link = "log")))
try(glmm.d <- glmer(Forced_Cop_Attempt~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link="identity")))
try(glmm.e<- glmer(Forced_Cop_Attempt~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link="sqrt")))
try(glmm.f <- glmer.nb(Forced_Cop_Attempt~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom2"))

      #I get  lot of singular fits

plot(glmm.a, which = "cook")
#This doesn't seem to be working. I don't think I was able to successfully run glmm.a for Forced Copulations

#Compare the AIC
AIC(glmm.normal, glmm.a, glmm.b, glmm.c, glmm.e)
#The lower the better. Negative binomial parameter 1 (glmm.b) works best.

#Boxplots to find outliers and check if residuals are centered around zero for each factor
augDat <- data.frame(data_factor, resid = residuals(glmm.a, type = "pearson"))

ggplot(augDat, aes(x = Pop, y =  resid, col = Stage)) +
  geom_point() +
  geom_boxplot(aes(group=Pop), alpha = 0.1) +
  coord_flip()

#Check for zero inflation

try(glmm.zero<-glmer.nb(Forced_Cop_Attempt~Pop*Stage+ (1|Family/ID), data=data_factor,family="nbinom", zeroInflation = T))
try(glmm.zero<-glmer.nb(Forced_Cop_Attempt~Pop*Stage+ (1|Family/ID), data=data_factor,family="nbinom", zeroInflation = T))
AIC(glmm.zero,glmm.a)

#AIC results equal, therefore no zero inflation

#Sigmoids
try(glmm.normal <- lmer(Sigmoid~Pop*Stage + (1|Family/ID), data = data_factor))
try(glmm.a <- glmer.nb(Sigmoid~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom"))
try(glmm.b <- glmer.nb(Sigmoid~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom1"))
try(glmm.c <- glmer(Sigmoid~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link = "log")))
try(glmm.d <- glmer(Sigmoid~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link="identity")))
try(glmm.e<- glmer(Sigmoid~Pop*Stage + (1|Family/ID), data = data_factor, family = poisson(link="sqrt")))
try(glmm.f <- glmer.nb(Sigmoid~Pop*Stage + (1|Family/ID), data = data_factor, family = "nbinom2"))

#I get  lot of singular fits

plot(glmm.a, which = "cook")
#This doesn't seem to be working. I don't think I was able to successfully run glmm.a for Forced Copulations

#Compare the AIC
AIC(glmm.normal, glmm.a, glmm.b, glmm.c, glmm.e)
#The lower the better. Negative binomials work best, both are equal

#Boxplots to find outliers and check if residuals are centered around zero for each factor
augDat <- data.frame(data_factor, resid = residuals(glmm.a, type = "pearson"))

ggplot(augDat, aes(x = Pop, y =  resid, col = Stage)) +
  geom_point() +
  geom_boxplot(aes(group=Pop), alpha = 0.1) +
  coord_flip()

#Check for zero inflation

try(glmm.zero<-glmer.nb(Sigmoid~Pop*Stage+ (1|Family/ID), data=data_factor,family="nbinom", zeroInflation = T))
try(glmm.zero<-glmer.nb(Sigmoid~Pop*Stage+ (1|Family/ID), data=data_factor,family="nbinom", zeroInflation = T))
AIC(glmm.zero, glmm.a)

#No difference in AIC with or without zero inflation

###(DEFUNCT) Parsed with column specification: ####
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

court %>%
  mutate(Pop = as.factor(Pop)) %>%
  mutate(Stage = as.factor(Stage)) %>%
  mutate(Family = as.factor(Family)) %>%
  mutate(ID = as.factor(ID)) -> court



############## DATA ANALYSIS ####################
###(DEFUNCT) What's the best distribution for my linear model?####

          #Poisson?
mean(court$Sigmoid)
var(court$Sigmoid)
mean(court$Forced_Cop_Attempt)
var(court$Forced_Cop_Attempt)
mean(court$G_Swing)
var(court$G_Swing)
#NO. Mean and variance must be equal

        #Negative binomial?
mean(court$Sigmoid)
alpha(court)

ggplot(data = court,
       mapping = aes(sample = court$Sigmoid)) + 
  stat_qq(distribution = stats::qpois,
          dparams = list(lambda = mean(court$Sigmoid)))

ggplot(data = court,
       mapping = aes(sample = court$Forced_Cop_Attempt)) + 
  stat_qq(distribution = stats::qpois,
          dparams = list(lambda = mean(court$Forced_Cop_Attempt)))

ggplot(data = court,
       mapping = aes(sample = court$G_Swing)) + 
  stat_qq(distribution = stats::qpois,
          dparams = list(lambda = mean(court$G_Swing)))
   #Another way
require(car)
require(MASS)
court$Sigmoid.t <- court$Sigmoid + 1
qqp(court$Sigmoid.t, "norm")
qqp(court$Sigmoid.t, "lnorm")
nbinom <- fitdistr(court$Sigmoid.t, "Negative Binomial")
qqp(court$Sigmoid.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(court$Sigmoid.t, "Poisson")
qqp(court$Sigmoid.t, "pois", poisson$estimate)  #Fails. What is lambda? Where do I put it?

court$Forced.t <- court$Forced_Cop_Attempt + 1
qqp(court$Forced.t, "norm")
qqp(court$Forced.t, "lnorm")
nbinom <- fitdistr(court$Forced.t, "Negative Binomial")
qqp(court$Forced.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(court$Forced.t, "Poisson")
qqp(court$Forced.t, "pois", poisson$estimate)

court$Swing.t <- court$G_Swing + 1
qqp(court$Swing.t, "norm")
qqp(court$Swing.t, "lnorm")
nbinom <- fitdistr(court$Swing.t, "Negative Binomial")
qqp(court$Swing.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(court$Forced.t, "Poisson")
qqp(court$Forced.t, "pois", poisson$estimate)

court$Lunge.t <- court$Lunge + 1
qqp(court$Lunge.t, "norm")
qqp(court$Lunge.t, "lnorm")
nbinom <- fitdistr(court$Lunge.t, "Negative Binomial")
qqp(court$Lunge.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])
poisson <- fitdistr(court$Lunge.t, "Poisson")
qqp(court$Lunge.t, "pois", poisson$estimate)

#Sigmoids by Population and Stage accounting for body length, then no length####

lmm.Sig <- lmer(Sigmoid ~ Pop*Stage + Length 
                + (1|Family/ID),
                data=court)

lmm.Sig <- lmer(Sigmoid ~ Pop*Stage
                + (1|Family/ID),
                data=court)
lmm.Sig

anova(lmm.Sig)

ranova(lmm.Sig)

#Total Sigmoid length by Population and Stage accounting for body length,then without length#################################

lmm.SigTot <- lmer(TTSigmoid ~ Pop*Stage + Length
                  + (1|Family/ID),
                  data=court)

lmm.SigTot <- lmer(TTSigmoid ~ Pop*Stage
                   + (1|Family/ID),
                   data=court)
lmm.SigTot

anova(lmm.SigTot)

ranova(lmm.SigTot)

#Average time sigmoiding

lmm.SigAvg <- lmer(TTAvgSigmoid ~ Pop*Stage + Length
                  + (1|Family/ID),
                  data=court)

lmm.SigAvg <- lmer(TTAvgSigmoid ~ Pop*Stage
                   + (1|Family/ID),
                   data=court)

lmm.SigAvg

anova(lmm.SigAvg)

ranova(lmm.SigAvg)

?ranova

#Forced Copulations by Population and Stage accounting for length, then without length

lmm.Force <- lmer(Forced_Cop_Attempt ~ Pop*Stage + Length
                  + (1|Family/ID),
                  data=court)

lmm.Force <- lmer(Forced_Cop_Attempt ~ Pop*Stage
                  + (1|Family/ID),
                  data=court)

anova(lmm.Force)

ranova(lmm.Force)

#Gonopodium Swings by Population and Stage accounting for length, then without length

lmm.Swing <- lmer(G_Swing ~ Pop*Stage + Length
                  + (1|Family/ID),
                  data=court)

lmm.Swing <- lmer(G_Swing ~ Pop*Stage
                  + (1|Family/ID),
                  data=court)

lmm.Swing

anova(lmm.Swing)

ranova(lmm.Swing)

#Lunges by Pop and Stage controlling for length

lmm.Lunge <- lmer(Lunge ~ Pop*Stage + Length
                  + (1|Family/ID),
                  data=court)

lmm.Lunge

anova(lmm.Lunge)

ranova(lmm.Lunge)

####Turn ANOVAS into objects

anova.Sig<-anova(lmm.Sig)
anova.Force<-anova(lmm.Force)
anova.Swing<-anova(lmm.Swing)

####Attempt pairwise comparison using emmeans

library(emmeans)
library(pbkrtest)

#Sig
emmeans(lmm.Sig, list(pairwise ~ Pop),adjust = "tukey")

emmeans(lmm.Sig, list(pairwise ~ Stage), adjust = "tukey")

#SigTT

emmeans(lmm.SigTot, list(pairwise ~ Stage), adjust = "tukey")

#SigAvg

emmeans(lmm.SigAvg, list(pairwise ~ Pop), adjust = "tukey")

#Forced copulations
emmeans(lmm.Force, list (pairwise ~ Pop), adjust = "tukey")
anova(lmm.Force)

#Gonopodium Swings
emmeans(lmm.Swing, list (pairwise ~ Pop)  , adjust = "tukey")


############################### GRAPHING ##################################################
### Function for getting standard error
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#######################################################################################
### Dodge

?as.factor

pd <- position_dodge(0.5) # move them 0.05 to the left and right

court$Pop <- factor(court$Pop, levels=c("CM","CQ","QC","QH"))
court$Stage <- factor(court$Stage, levels=c("Before", "After"))

dSig <- summarySE(court, measurevar="Sigmoid",
                  groupvars=c("Pop","Stage"))
dSig

head(dSig)

graph.Sig <- ggplot(dSig, aes(x=Stage, y=Sigmoid, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=Sigmoid-se, ymax=Sigmoid+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Exposure Stage", y="# Sigmoids +/- SE") +
  guides(color=guide_legend("Population/Cross"))  +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 16, vjust = -1),
        axis.text.x  = element_text(vjust=0.5, size=12, face = "bold", colour = "black"),
        axis.title.y = element_text(face = "bold", colour = "black", size = 16, vjust = 3),
        axis.text.y = element_text(vjust = 0.5, size = 12, face = "bold", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11, face = "italic"),
        legend.position=c(0.9, 0.9),
        legend.background = element_rect(size=0.5, color=1),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

graph.Sig

#Total Sigmoid Length
dSigTot <- summarySE(court, measurevar="TTSigmoid",
                  groupvars=c("Pop","Stage"))
dSigTot

head(dSigTot)

graph.SigTot <- ggplot(dSigTot, aes(x=Stage, y=TTSigmoid, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=TTSigmoid-se, ymax=TTSigmoid+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Exposure Stage", y="Total Sigmoid Time +/- SE") +
  guides(color=guide_legend("Population/Cross"))  +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 16, vjust = -1),
        axis.text.x  = element_text(vjust=0.5, size=12, face = "bold", colour = "black"),
        axis.title.y = element_text(face = "bold", colour = "black", size = 16, vjust = 3),
        axis.text.y = element_text(vjust = 0.5, size = 12, face = "bold", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11, face = "italic"),
        legend.position=c(0.9, 0.9),
        legend.background = element_rect(size=0.5, color=1),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

graph.SigTot

#Average Sigmoid Time
dSigAvg <- summarySE(court, measurevar="TTAvgSigmoid",
                     groupvars=c("Pop","Stage"))
dSigAvg

head(dSigAvg)

graph.SigAvg <- ggplot(dSigAvg, aes(x=Stage, y=TTAvgSigmoid, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=TTAvgSigmoid-se, ymax=TTAvgSigmoid+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Exposure Stage", y="Average Sigmoid Time +/- SE") +
  guides(color=guide_legend("Population/Cross"))  +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 16, vjust = -1),
        axis.text.x  = element_text(vjust=0.5, size=12, face = "bold", colour = "black"),
        axis.title.y = element_text(face = "bold", colour = "black", size = 16, vjust = 3),
        axis.text.y = element_text(vjust = 0.5, size = 12, face = "bold", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11, face = "italic"),
        legend.position=c(0.9, 0.9),
        legend.background = element_rect(size=0.5, color=1),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

graph.SigAvg

#Forced copulation attempts

dForced <- summarySE(court, measurevar="Forced_Cop_Attempt",
                     groupvars=c("Pop","Stage"))

head(dForced)
graph.Forced <- ggplot(dForced, aes(x=Stage, y=Forced_Cop_Attempt, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=Forced_Cop_Attempt-se, ymax=Forced_Cop_Attempt+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Exposure Stage", y="# Forced Copulations +/- SE") +
  guides(color=guide_legend("Population/Cross"))  +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 16, vjust = -1),
        axis.text.x  = element_text(vjust=0.5, size=12, face = "bold", colour = "black"),
        axis.title.y = element_text(face = "bold", colour = "black", size = 16, vjust = 3),
        axis.text.y = element_text(vjust = 0.5, size = 12, face = "bold", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11, face = "italic"),
        legend.position=c(0.15, 0.9),
        legend.background = element_rect(size=0.5, color=1),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

graph.Forced

str(court)

#Gonopodium Swings

dSwing <- summarySE(court, measurevar="G_Swing",
                    groupvars=c("Pop","Stage"))

head(dSwing)

graph.Swing <- ggplot(dSwing, aes(x=Stage, y=G_Swing, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=G_Swing-se, ymax=G_Swing+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Exposure Stage", y="# Gonopodium Swings +/- SE") +
  guides(color=guide_legend("Population/Cross"))  +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 16, vjust = -1),
        axis.text.x  = element_text(vjust=0.5, size=12, face = "bold", colour = "black"),
        axis.title.y = element_text(face = "bold", colour = "black", size = 16, vjust = 3),
        axis.text.y = element_text(vjust = 0.5, size = 12, face = "bold", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11, face = "italic"),
        legend.position=c(0.15, 0.9),
        legend.background = element_rect(size=0.5, color=1),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

graph.Swing


