## Author: Ruben Dezeure
## comp stat series 2

## reading the dataset
prostate <- read.table("http://stat.ethz.ch/Teaching/Datasets/prostcancer.dat",header = TRUE)
prostate
prost.full <- lm(psa~.,data=prostate)
prost.empty <- lm(psa~1,data=prostate)
pairs(prostate,pch = ".")

plot(residuals(prost.full))
## model assumptions not valid

prostate$logpsa <- log(prostate$psa)
prostate$logbph <- log(prostate$bph)
pairs(prostate,pch = ".")
pairs(prostate)
prost.full <- lm(logpsa~. - psa -bph,data=prostate)
prost.empty <- lm(logpsa~1,data=prostate)
plot(residuals(prost.full))

## check model assumptions
## tukey-anscombe plot
plot(fitted(prost.full),residuals(prost.full))
## qq plot
qqnorm(resid(prost.full))

## plotting the residuals vs. the ordering index we find a clear trend
## plot for detecting serial correlation
plot(resid(prost.full))
## this is due to the fact that the residuals increase with logpsa and the data is ordered according to increasing logpsa!
## This is an artifact
## usually it is not a good idea to plot the response variable vs. the residuals, while plots of the residuals vs. the predictors might be useful.

## Backward elimination, starting from the full model
prost.bw <- step(prost.full,direction="backward")
prost.fw <- step(prost.empty,direction="forward",scope= logpsa ~ cavol+weight+age+logbph+svi+cp+gleason+pgg45)

## logpsa ~ cavol + logbph + svi + pgg45
## seems to be the optimal?

## loading the package for all-subsets regression
library(leaps)
## All subsets model choice, compare to the stepwise methods
prost.alls <- regsubsets(logpsa~cavol+weight+age+logbph+svi+cp+gleason+pgg45,data=prostate,nbest=1,method="exhaustive")

## load function to produce a nice figure of C_p versus p
source("ftp://stat.ethz.ch/Teaching/maechler/CompStat/cp-plot.R")
p.regsubsets(prost.alls)
