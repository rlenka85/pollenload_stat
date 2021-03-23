### install if necessary and then load the libraries you need

j <- c("rstudioapi","plyr","dplyr","arm")

new.packages <- j[!(j %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(j, require, character.only = TRUE)  # loads up any libraries that aren't already loaded

### set working directory to script's saved location

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



### read in data
pollenload <- read.csv("pollenload_test.csv")

summary(pollenload)

# proboscis type is currently numeric, but it should be a factor 

pollenload$proboscis_type <- as.factor(pollenload$proboscis_type)
summary(pollenload)


# check structure of response variable

hist(pollenload$pollen_load)
hist(log(pollenload$pollen_load))

# important to note here that the distribution is NOT a normal distribution
# in fact, it's likely to be a better approximation to a Poisson distribution
# but the variance is such that it's almost certainly overdispersed
# this limits the range of suitable statistical tests


### test proboscis type vs pollen load
# there are two classes and the data is non-normal, so we'll need a generalised linear model
# further, the data is poisson-type but overdispersed so we'll need a quasipoisson model

# first plot the test

boxplot(pollen_load ~ proboscis_type,
        data = pollenload)

# then construct a model

mod1 <- glm(pollen_load ~ proboscis_type,
            family = quasipoisson,
            data = pollenload)


# check the model's fit by inspecting its residuals
sresid <- resid(mod1)
hist(sresid)
fitted.glmm <- fitted(mod1, level=1)        # Extract the fitted (predicted) values
plot(sresid ~ fitted.glmm)                   # Check for homoscedasticity
par(mfrow=c(2,2))
plot(mod1)                                  # gives a heteroscedasticity plot #fans out, not good
par(mfrow=c(1,1))
binnedplot(fitted(mod1),resid(mod1))

# these aren't perfect but are probably acceptable

### next step is to test significance

summary(mod1)

drop1(mod1, test = "F")




