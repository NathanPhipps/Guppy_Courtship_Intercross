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
fit_gaussian <- MCMCglmm(cop_attempt ~ variety + experience,
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
