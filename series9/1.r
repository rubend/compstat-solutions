## Author: Ruben Dezeure
ozone <- read.table("http://stat.ethz.ch/Teaching/Datasets/ozone.dat")

## a)
pairs(ozone)
ozone
ozone <- ozone[max(ozone$wdsp)!=ozone$wdsp,]
d.ozone <- subset(transform(ozone,logupo3=log(upo3)),select=-upo3)
pairs(d.ozone)

## b)
library(earth)

## leave one out cv
err <- numeric(4)
degrees <- seq(1,4,1)

for(i in 1:nrow(d.ozone)){
  dat <- d.ozone[-i,]
  
  for(j in 1:length(degrees)){
    err[j] <- err[j] + (d.ozone$logupo3[i]
                        -predict(earth(logupo3 ~ .,data=dat,degree=degrees[j]),d.ozone[i,]))^2
  }
  
}
optdegree <- degrees[min(err) == err]
## optimal degree is 1! == correct see model solution

## c)
## now we work with a maximal interaction degree equal 2
fit <- earth(logupo3 ~ .,data=d.ozone,degree=2)
summary(fit)
## tukey anscombe plot
plot(fit$fitted.values,fit$residuals)
## looks reasonable, the variance seems to be constant
plot(fit$residuals)

plotmo(fit)

## part d)
## now we want to compare MARS with an additive model
fit.mars <- earth(logupo3 ~ .,data=d.ozone,degree=1)
library(mgcv)
fit.am <- gam(logupo3 ~ s(vdht) + s(wdsp) + s(hmdt) + s(sbtp) + s(ibht) + s(dgpg) + s(ibtp) + s(vsty) + s(day),data=d.ozone)

summary(fit.mars)
summary(fit.am)
dat <- subset(d.ozone,select=-ibtp)
##source("ftp://stat.ethz.ch/Teaching/maechler/CompStat/potGAM.R")
##p.gam()
fit.am <- gam(logupo3 ~ s(vdht) + s(wdsp) + s(hmdt) + s(sbtp) + s(ibht) + s(dgpg) + s(vsty) + s(day),data=dat)
summary(fit.am)

source("ftp://stat.ethz.ch/Teaching/maechler/CompStat/plotGAM.R")
p.gam(fit.am, rug=FALSE)
