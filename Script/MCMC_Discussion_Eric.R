#Playing with MCMC with Eric
library(tidyverse)
as_tibble(fit_gaussian_Sig$VCV)
rando<-as_tibble(fit_gaussian_Sig$VCV) %>%
  mutate(idx=1:1000)
ggplot(rando, aes(x=idx, y=`1.famil`)) + geom_point()
#Shows MCMC is mixing well
ggplot(rando, aes(x=idx, y=`1.ID`)) + geom_point()
#Shows that the MCMC isn't mixing very well. Try increasing number of iterations later
ggplot(rando, aes(x=idx, y=`units`)) + geom_point()
#Mixing well

fit_gaussian_Sig$Sol %>% View()
fix<-as_tibble(fit_gaussian_Sig$Sol) %>%
  mutate(idx=1:1000)
sfix<-fix %>%
  gather(key="variable", value="value", -idx)

ggplot(sfix, aes(x=idx, y=value)) + geom_point() + facet_wrap(~variable)
sfix
View(fix)

ggplot(sfix, aes(x=value)) + geom_histogram(bin=20) + facet_wrap(~variable)

rando
