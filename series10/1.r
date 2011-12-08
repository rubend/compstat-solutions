## Author: Ruben Dezeure
ozone <- read.table("http://stat.ethz.ch/Teaching/Datasets/ozone.dat")
d.ozone <- subset(transform(ozone,logupo3=log(upo3)),select=-upo3)
d.ozone.e <- d.ozone[-which.max(d.ozone[,"wdsp"]),]
d.ozone.es <- d.ozone.e
d.ozone.es[,-10] <- scale(d.ozone.e[,-10])

## a general function for cross validation
source("http://stat.ethz.ch/teaching/lectures/FS_2010/CompStat/cv-dozonees.R")
noterms <- 4
cv.gcv <- cv.std <- cv.spl <- numeric(noterms)
for(j in 1:noterms){
  cv.gcv[j] <- cv(ppr,sm.method="gcvspline",nterms=j,max.terms=5)
  cv.spl[j] <- cv(ppr,sm.method="spline",nterms=j,max.terms=5)
  cv.std[j] <- cv(ppr,nterms=j,max.terms=5)
}

library(earth)
cv.mars <- numeric(3)
for(i in 1:3){
  cv.mars[i] <- cv(earth,degree=i)
}

cv.gcv ##[1] 54.85800 55.75201 49.68809 51.30815
cv.spl ##[1] 54.83099 49.44023 43.82388 42.79899
## doing for spline nterms = 5 max.terms=10 ==> 38.67422 
cv.std ##[1] 57.27479 55.81857 54.18426 53.16166
cv.mars ##[1] 41.14956 43.80965 44.39073

## we need bigger pc to get to minimum?

##yes I think so

## b)
## best solution found in model solution
## sm.method = "spline", nterms = 4 and df = 6. 
fit <- ppr(logupo3 ~ .,data = d.ozone.es,sm.method="spline",nterms=4,df=6,max.terms=7)
cv(ppr,sm.method="spline",nterms=4,df=6,max.terms=7)
par(mfrow=c(2,2))
plot(fit)
## plots all the ridge functions

## c)
alpha.matrix <- round(fit$alpha,2)
