## Author: Ruben Dezeure
x2 <- rep(1:10,10)
x3 <- rep(1:10,each = 10)
data <- cbind(x2,x3)
B <- 1000 ## want it to be 100 eventually
n <- length(x2)


coefs <- function(dat,ind){
  coef(lm(y~x2+x3,data=dat[ind,]))
}

bootstrap <- function(x2,x3,y){
  betas <- matrix(0,B,3) ## to fill with the results
  dat <- data.frame(y,x2,x3)
  ## faster
  indices <- replicate(B,sample(1:n,n,replace=TRUE))
  betas <- apply(indices,2,coefs,dat=dat)
  ## deprecated
  ##  for(i in 1:B){
  ##   indices <- sample(1:n,n,replace=TRUE)
  ##  out <- y[indices]
  ## bs.sample <- data.frame(x2[indices],x3[indices],out)
  ## betas[i,] <- lm(out~.,data=bs.sample)$coefficients
  ##}

  
  exp <- apply(betas,1,mean)

  ## est betas, we need beta^ for confidence interval, not beta^*!
  dat <- data.frame(y,x2,x3)
  est <- coef(lm(y~.,data=dat))

  ## vars <- apply(betas,2,var)
  ## we have to calculate the confidence interval in a different way
  quantiles <- apply(betas,1,quantile,probs=c(0.025,0.975),names=FALSE)
  
  interval <- matrix(0,2,3)
  interval <- cbind(c(2*est[1]-quantiles[2,1],
                      2*est[1]-quantiles[1,1]),
                    c(2*est[2]-quantiles[2,2],
                      2*est[2]-quantiles[1,2]),
                    c(2*est[3]-quantiles[2,3],
                      2*est[3]-quantiles[1,3]))
  interval
}


boot.builtin <- function(y){
  dat <- data.frame(y,x2,x3)
  bst.sample <- boot(data=dat,statistic=coefs,R=B)
  bst.ci.1 <- boot.ci(bst.sample,conf=1-0.05,type="basic",index=1)
  bst.ci.2 <- boot.ci(bst.sample,conf=1-0.05,type="basic",index=2)
  bst.ci.3 <- boot.ci(bst.sample,conf=1-0.05,type="basic",index=3)
  
  t(cbind(bst.ci.1$basic[c(4,5)],bst.ci.2$basic[c(4,5)],bst.ci.3$basic[c(4,5)]))
}
## part b
betas.sol <- c(1,-2,3)
X <- cbind(rep(1,100),x2,x3)
base <- betas.sol %*% t(X)
nodatasets <- 10

## to make the results reproducible
set.seed(11)

hit.classic.normal <- hit.classic.rt <- hit.classic.exp <- 0
hit.boot.normal <- hit.boot.rt <- hit.boot.exp <- 0
hit.builtin.normal <- hit.builtin.rt <- hit.builtin.exp <- 0
for(j in 1:nodatasets){
  y.normal <- base + rnorm(100)
  y.rt <- base + rt(100,df=3)
  y.exp <- base + rexp(100) - 1
  
  ##classical confidence intervals
  y <- t(y.normal)
  set <- data.frame(y,x2,x3)
  conf.normal <- confint(lm(y~.,data=set),level=0.95)
  y <- t(y.rt)
  set <- data.frame(y,x2,x3)
  conf.rt <- confint(lm(y~.,data=set),level=0.95)
  y <- t(y.exp)
  set <- data.frame(y,x2,x3)
  conf.exp <- confint(lm(y~.,data=set),level=0.95)

  ##hits in the classical confidence interval
  hit.classic.normal <- hit.classic.normal +
    sum((betas.sol >=conf.normal[,1]) & (betas.sol <= conf.normal[,2]))
  hit.classic.rt <- hit.classic.rt +
    sum((betas.sol >=conf.rt[,1]) & (betas.sol <= conf.rt[,2]))
  hit.classic.exp <- hit.classic.exp +
    sum((betas.sol >=conf.exp[,1]) & (betas.sol <= conf.exp[,2]))
  
  ## B=1000 is too slow for this
  boot.normal <- bootstrap(x2,x3,t(y.normal))
  boot.rt <- bootstrap(x2,x3,t(y.rt))
  boot.exp <- bootstrap(x2,x3,t(y.exp))

  ## to get the same data format as what comes out of confint
  boot.normal <- t(boot.normal)
  boot.rt <- t(boot.rt)
  boot.exp <- t(boot.exp)

  ##hits in the classical confidence interval
  hit.boot.normal <- hit.boot.normal +
    sum((betas.sol >=boot.normal[,1]) & (betas.sol <= boot.normal[,2]))
  hit.boot.rt <- hit.boot.rt +
    sum((betas.sol >=boot.rt[,1]) & (betas.sol <= boot.rt[,2]))
  hit.boot.exp <- hit.boot.exp +
    sum((betas.sol >=boot.exp[,1]) & (betas.sol <= boot.exp[,2]))


  ## doing the bootstrap with the package boot
  library(boot)
  ## need function coefs for this
  builtin.normal <- boot.builtin(t(y.normal))
  builtin.rt <- boot.builtin(t(y.rt))
  builtin.exp <- boot.builtin(t(y.exp))
  hit.builtin.normal <- hit.builtin.normal +
    sum((betas.sol >=builtin.normal[,1]) & (betas.sol <= builtin.normal[,2]))
  hit.builtin.rt <- hit.builtin.rt +
    sum((betas.sol >=builtin.rt[,1]) & (betas.sol <= builtin.rt[,2]))
  hit.builtin.exp <- hit.builtin.exp +
    sum((betas.sol >=builtin.exp[,1]) & (betas.sol <= builtin.exp[,2]))    
}


hit.classic.normal
hit.classic.rt
hit.classic.exp
hit.boot.normal
hit.boot.rt
hit.boot.exp
hit.builtin.normal
hit.builtin.rt
hit.builtin.exp


## part d)
betas.sol <- c(1,-2,3)
X <- cbind(rep(1,100),x2,x3)
base <- betas.sol %*% t(X)
nosimul <- 100

## to make the results reproducible
set.seed(11)

boot.builtin.betas <- function(y){
  dat <- data.frame(y,x2,x3)
  bst.sample <- boot(data=dat,statistic=coefs,R=B)
  bst.sample$t
}

l1.normal <- l1.rt <- l1.exp <- numeric(nosimul)
l1ge.normal <- l1ge.rt <- l1ge.exp <- numeric(nosimul)
for(j in 1:nosimul){
  y.normal <- base + rnorm(100)
  y.rt <- base + rt(100,df=3)
  y.exp <- base + rexp(100) - 1

  ## turn these things into vectors
  y.normal <- y.normal[1:length(y.normal)]
  y.rt <- y.rt[1:length(y.rt)]
  y.exp <- y.exp[1:length(y.exp)]
  ##classical confidence intervals
  y <- y.normal
  set <- data.frame(y,x2,x3)
  l1.normal[j] <- mean(abs(y- fitted(lm(y~.,data=set))))
  y <- y.rt
  set <- data.frame(y,x2,x3)
  l1.rt[j] <- mean(abs(y- fitted(lm(y~.,data=set))))
  y <- y.exp
  set <- data.frame(y,x2,x3)
  l1.exp[j] <- mean(abs(y- fitted(lm(y~.,data=set))))

  betas.normal <- boot.builtin.betas(y.normal)

  
  betas.rt <- boot.builtin.betas(y.rt)
  betas.exp <- boot.builtin.betas(y.exp)

  yvals.normal <- betas.normal %*% t(X)
  avg.boot <- apply(abs(matrix(rep(y.normal,B),n,B)-t(yvals.normal)),2,mean) ## average over the n elements
  l1ge.normal[j] <- mean(avg.boot) ## average over B bootstrap runs

  yvals.rt <- betas.rt %*% t(X)
  avg.boot <- apply(abs(matrix(rep(y.rt,B),n,B)-t(yvals.rt)),2,mean) ## average over the n elements
  l1ge.rt[j] <- mean(avg.boot) ## average over B bootstrap runs

  yvals.exp <- betas.exp %*% t(X)
  avg.boot <- apply(abs(matrix(rep(y.exp,B),n,B)-t(yvals.exp)),2,mean) ## average over the n elements
  l1ge.exp[j] <- mean(avg.boot) ## average over B bootstrap runs
}

mean(l1.normal)
mean(l1.rt)
mean(l1.exp)
mean(l1ge.normal)
mean(l1ge.rt)
mean(l1ge.exp)

par(mfrow=c(2,3))
hist(l1.normal)
hist(l1.rt)
hist(l1.exp)
hist(l1ge.normal)
hist(l1ge.rt)
hist(l1ge.exp)
