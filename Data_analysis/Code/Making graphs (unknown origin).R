
rm(list=ls())
library(tidyverse)
setwd("C:/Users/phipp/Desktop/MyFirstAnalysis")
court2<-read_csv("courtForNathan3.csv")
names(court2)
head(court2)

ggplot(court2, aes(x = Aggro, y = Pass)) + 
  geom_point() +
  theme_classic()

ggplot(court2, aes(x = court2$ChaseAggro, y = court2$Pass)) + 
  geom_point(color='blue') +
  theme_classic() +
  geom_smooth(method = "lm", se = FALSE)

cor(court2$ChaseAggro, court2$Pass)

######################################################################
mean.ChaseAggro<-tapply(court2$ChaseAggro,list(court2$Pop),mean)
mean.ChaseAggro

sd.ChaseAggro<-tapply(court2$ChaseAggro,list(court2$Pop),sd)
sd.ChaseAggro
length.GSwingStage<-tapply(mydata$G_Swing,list(mydata$Stage),length)
length.GSwingStage



mids<- barplot(mean.ChaseAggro,
               beside=T, legend=T,
               xlab="Pop",
               ylab="ChaseAggro",
               ylim=c(0, 20),
               col=grey(c(0, 0.3, 0.6, 1)))


arrows(mids,
       mean.ChaseAggro + sd.ChaseAggro,
       mids,
       mean.ChaseAggro - sd.ChaseAggro,
       angle=90,
       code=3,
       length=0.1)
###################################################################################\
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
library(plyr)
library(dplyr)
pd <- position_dodge(0.5) # move them 0.05 to the left and right

court2$Pop <- factor(court2$Pop, levels=c("CM","CQ","QC","QH"))
court2$Stage <- factor(court2$Stage, levels=c("Before", "After"))

dAggro <- summarySE(court2, measurevar="ChaseAggro",
                  groupvars=c("Pop","Stage"))
dAggro

head(dAggro)

graph.Aggro <- ggplot(dAggro, aes(x=Stage, y=ChaseAggro, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=ChaseAggro-se, ymax=ChaseAggro+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Exposure Stage", y="# Aggressive Behaviors +/- SE") +
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

graph.Aggro

dPass <- summarySE(court2, measurevar="Pass",
                     groupvars=c("Pop","Stage"))

head(dPass)
graph.Pass <- ggplot(dPass, aes(x=Stage, y=Pass, group=Pop, col=Pop)) +
  geom_errorbar(aes(ymin=Pass-se, ymax=Pass+se), position = pd, width=0.1, colour="black", size=1) +
  geom_line(size=1, position = pd) + 
  geom_point(position = pd, size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(x="Exposure Stage", y="# Passive Behaviors +/- SE") +
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

graph.Pass

#################################################################################
#Aggression by Population and Stage accounting for length
lmm.ChaseAggro <- lmer(ChaseAggro ~ Pop*Stage + Length 
                + (1|Family/ID),
                data=court2)
lmm.ChaseAggro

anova(lmm.ChaseAggro)

ranova(lmm.ChaseAggro)

#################################################################################
#Passive by Population and Stage Accounting for length
lmm.Pass <- lmer(Pass ~ Pop*Stage + Length 
                + (1|Family/ID),
                data=court2)
lmm.Pass

anova(lmm.Pass)

ranova(lmm.Pass)
