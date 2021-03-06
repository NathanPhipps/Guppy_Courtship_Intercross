
#Clear R's brain
rm(list=ls())

setwd("C:/Users/phipp/OneDrive/Documents/Grad School/Courtship Analysis")
mydata<-read.csv("FINAL_court.csv")
court<-read.csv("FINAL_court.csv")
mydata
#Calling all libraries

names(mydata)

length(mydata$ID)
length(mydata$Stage)

nrow(court[court$Suc_SigCop > "0",])
nrow(court[court$Suc_ForcCop > "0",])

##Find n for all groups
nrow(court [court$Stage == "After",])
court.tmp <- court [court$Stage == "After",]
length (unique(court.tmp$Pop == "CM"))

nrow(court[court$Pop == "CM",])
court.tmp4 <- court [court$Pop == "CM",]
length (unique(court.tmp4$ID))

nrow(court [court$Pop == "CQ",])
court.tmp2 <- court [court$Pop == "CQ",]
length (unique(court.tmp2$ID))

nrow(court[court$Pop == "QC",])
court.tmp3 <- court [court$Pop == "QC",]
length (unique(court.tmp3$ID))

nrow(court[court$Pop == "QH",])
court.tmp4 <- court [court$Pop == "QH",]
length (unique(court.tmp4$ID))


#Calling packages

library(tidyverse)
library(lmerTest)

## Parsed with column specification:
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


# Trying to find family sizes

FamilyNumbers<-length(unique(mydata$Family))
length(unique(mydata$family["420"]))

######################################################################################
#ggplot to test aggression correlations
library(tidyverse)

court <- read_csv("FINAL_court.csv")

court %>%
  mutate(Pop = as.factor(Pop)) %>%
  mutate(Stage = as.factor(Stage)) %>%
  mutate(Family = as.factor(Family)) %>%
  mutate(ID = as.factor(ID)) -> court

head(court$Stage)


court

#Sigmoids by Population and Stage accounting for length

lmm.Sig <- lmer(Sigmoid ~ Pop*Stage + Length 
                + (1|Family/ID),
                data=court)

anova(lmer(Sigmoid ~ Stage*Pop + Length 
                + (1|Family/ID),
                data=court))

lmm.Sig

anova(lmm.Sig)

ranova(lmm.Sig)

#Forced Copulations by Population and Stage accounting for length

lmm.Force <- lmer(Forced_Cop_Attempt ~ Pop*Stage + Length
                  + (1|Family/ID),
                  data=court)

anova(lmm.Force)

ranova(lmm.Force)

#Gonopodium Swings by Population and Stage accounting for length

lmm.Swing <- lmer(G_Swing ~ Pop*Stage + Length
                  + (1|Family/ID),
                  data=court)

lmm.Swing

anova(lmm.Swing)

ranova(lmm.Swing)

####Turn ANOVAS into objects

anova.Sig<-anova(lmm.Sig)
anova.Force<-anova(lmm.Force)
anova.Swing<-anova(lmm.Swing)

####Attempt pairwise comparison using emmeans

library(emmeans)
emmeans(lmm.Sig)

####Attempt pairwise comparison using Tukey's HSD
?TukeyHSD
aov.Swing<-aov(G_Swing ~ Pop*Stage + Length,
     data=court)
TukeyHSD(aov.Swing, which ,ordered = FALSE, conf.level = 0.95)

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

mydata$Pop <- factor(mydata$Pop, levels=c("CM","CQ","QC","QH"))
mydata$Stage <- factor(mydata$Stage, levels=c("First", "Second"))

dSig <- summarySE(court, measurevar="Sigmoid",
                  groupvars=c("Pop","Stage"))
dSig

head(dSig)

#Re-order levels before and after
dSig$Stage <- factor(dSig$Stage, levels = c("Before", "After"))

graph.Sig <- ggplot(dSig, aes(x=Stage, y=Sigmoid, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=Sigmoid-se, ymax=Sigmoid+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Courtship Experience", y="# Sigmoids +/- SE") +
  scale_x_discrete(breaks=c("Before","After"),
                   labels=c("First","Second")) +
  guides(color=guide_legend("Genetic Line"))  +
  theme_classic() +
  expand_limits(y=c(0,14)) +
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 20, vjust = -1),
        axis.text.x  = element_text(vjust=0.5, size=18, face = "bold", colour = "black"),
        axis.title.y = element_text(face = "bold", colour = "black", size = 20, vjust = 3),
        axis.text.y = element_text(vjust = 0.5, size = 18, face = "bold", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11, face = "italic"),
        legend.position=c(0.9, 0.8),
        legend.background = element_rect(size=0.5, color=1),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

graph.Sig



dForced <- summarySE(court, measurevar="Forced_Cop_Attempt",
                     groupvars=c("Pop","Stage"))

# Re-ordering levels Before and After
dForced$Stage <- factor(dForced$Stage, levels = c("Before", "After"))

head(dForced)
graph.Forced <- ggplot(dForced, aes(x=Stage, y=Forced_Cop_Attempt, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=Forced_Cop_Attempt-se, ymax=Forced_Cop_Attempt+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Courtship Experience", y="# Forced Copulations +/- SE") +
  guides(color=guide_legend("Genetic Line"))  +
  scale_x_discrete(breaks=c("Before","After"),
                   labels=c("First","Second")) +
  expand_limits(y=c(0,14)) +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 20, vjust = -1),
        axis.text.x  = element_text(vjust=0.5, size=18, face = "bold", colour = "black"),
        axis.title.y = element_text(face = "bold", colour = "black", size = 20, vjust = 3),
        axis.text.y = element_text(vjust = 0.5, size = 18, face = "bold", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11, face = "italic"),
        legend.position=c(0.9, 0.8),
        legend.background = element_rect(size=0.5, color=1),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

graph.Forced

str(court)

dSwing <- summarySE(court, measurevar="G_Swing",
                    groupvars=c("Pop","Stage"))

#Re-ordering levels before and after
dSwing$Stage <- factor(dSwing$Stage, levels = c("Before", "After"))


head(dSwing)

graph.Swing <- ggplot(dSwing, aes(x=Stage, y=G_Swing, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=G_Swing-se, ymax=G_Swing+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Courtship Experience", y="# Gonopodium Swings +/- SE") +
  guides(color=guide_legend("Genetic Line"))  +
  theme_classic() +
  scale_x_discrete(breaks=c("Before","After"),
                   labels=c("First","Second")) +
  expand_limits(y=c(0,14)) +
  theme(axis.title.x = element_text(face = "bold", colour = "black", size = 20, vjust = -1),
        axis.text.x  = element_text(vjust=0.5, size=18, face = "bold", colour = "black"),
        axis.title.y = element_text(face = "bold", colour = "black", size = 20, vjust = 3),
        axis.text.y = element_text(vjust = 0.5, size = 18, face = "bold", colour = "black"),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11, face = "italic"),
        legend.position=c(0.15, 0.8),
        legend.background = element_rect(size=0.5, color=1),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"))

graph.Swing

####################################################################################
#GG scatterplots

ggplot(court, aes(x = G_Swing, y = ForcedCop)) + 
  geom_point() +
  theme_classic()
court
#Add a regression line
ggplot(court, aes(x = court$ForcedCop, y = court$Sigmoid)) + 
  geom_point(color='blue') +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE)

cor(court$ForcedCop, court$Sigmoid)
#Find moderate positive correlation with correlation coefficient 0.551

court

################ Power analysis ###################################################
pwr.f2.test(u =, v = , f2 = , sig.level = , power = )


  