## Author: Ruben Dezeure
library(mvtnorm) ## Needed for rmvnorm
library(MASS) ## Needed for lda/qda
## Read in a function that plots LDA/QDA decision boundaries
source("http://stat.ethz.ch/teaching/lectures/FS_2010/CompStat/predplot.R")
## Covariance matrix
sigma <- cbind(c(0.5,0.3),c(0.3,0.5))
## Mean vectors
mu1 <- c(3,1.5)
mu2 <- c(4,4)
mu3 <- c(8.5,2)
m <- matrix(0,nrow=300,ncol=3)
## Grouping vector
m[,3] <- rep(1:3,each = 100)
## Simulate data
m[1:100,1:2] <- rmvnorm(n=100,mean=mu1,sigma=sigma)
m[101:200,1:2] <- rmvnorm(n=100,mean=mu2,sigma=sigma)
m[201:300,1:2] <- rmvnorm(n=100,mean=mu3,sigma=sigma)
m <- data.frame(m)

## Perform LDA and plot the results:
fit <- lda(x=m[,1:2],grouping=m[,3])
predplot(fit,m)

## manually calculate see c) the boundary between group 1 and 2
## add your solution to the plot with abline()

## using the results from lda
A <- fit$scaling
sigma.inv <- A%*%t(A)
## only need data from groups 1 and 2
means.lda <- t(fit$means[1:2,])
c <- numeric(2)
c[1] <- log(fit$prior[1])-1/2*t(means.lda[,1]) %*% sigma.inv %*% means.lda[,1]
c[2] <- log(fit$prior[2])-1/2*t(means.lda[,2]) %*% sigma.inv %*% means.lda[,2]

b <- sigma.inv %*% means.lda
bdiff <- b[,2]-b[,1] ## b1-b0

intercept <- (c[1]-c[2])/bdiff[2]
slope <- -bdiff[1]/bdiff[2]

abline(intercept,slope,col="purple",lty=3)
