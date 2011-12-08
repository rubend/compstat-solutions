## Author: Ruben Dezeure
## series 3: nonparametric regression
m <- function(x){
  x+4*cos(7*x)
}

x <- seq(-1,1,length=101)
n <- 101
nrep <- 1000
set.seed(79)

Snw <- Slp <- Sss <- matrix(0,nrow=101,ncol=101)
In <- diag(101)
for(j in 1:101){
  y <- In[,j]
  Snw[,j] <- ksmooth(x,y,kernel="normal",bandwidth=0.2,x.points=x)$y
  Slp[,j] <- predict(loess(y~x,span=0.2971339),newdata=x)
  Sss[,j] <- smooth.spline(x,y,control.spar=0.623396)$y
}

estnw <- estlp <- estss <- matrix(0,101,ncol=nrep)
senw <- selp <- sess <- matrix(0,101,ncol=nrep)
for(i in 1:nrep){
  ## Simulate y-values
  y <- m(x) + rnorm(length(x))
  ## Get estimates for the mean function
  estnw[,i] <- ksmooth(x,y,kernel="normal",bandwidth = 0.2,x.points=x)$y
  estlp[,i] <- predict(loess(y~x,span = 0.2971339),newdata=x)
  estss[,i] <- smooth.spline(x,y,control.spar=0.623396)$y

  sigma2nw <- sum((y-estnw[,i])^2)/(n-sum(diag(Snw)))
  sigma2lp <- sum((y-estlp[,i])^2)/(n-sum(diag(Slp)))
  sigma2ss <- sum((y-estss[,i])^2)/(n-sum(diag(Sss)))

  senw[,i] <- sqrt(sigma2nw*diag(Snw%*%t(Snw)))
  selp[,i] <- sqrt(sigma2lp*diag(Slp%*%t(Slp)))
  sess[,i] <- sqrt(sigma2ss*diag(Sss%*%t(Sss)))
}

## compute the empirical bias
par(mfrow=c(1,4))
means <- cbind(apply(estnw,1,mean),apply(estlp,1,mean),apply(estss,1,mean))
plot(x,m(x) - apply(estnw,1,mean))
plot(x,m(x) - apply(estlp,1,mean))
plot(x,m(x) - apply(estss,1,mean))
plot(x,m(x))

par(mfrow=c(1,2))
means <- cbind(apply(estnw,1,mean),apply(estlp,1,mean),apply(estss,1,mean))
matplot(x,means,lty = 2:4, col = 2:4, type = "l",xlab = "x", ylab = "m(x)")
lines(seq(-1, 1, by = 0.01), m(seq(-1, 1, by = 0.01)))
matplot(x,cbind(m(x),m(x),m(x))-means,lty = 2:4, col = 2:4, type = "l",xlab = "x", ylab = "m(x)")
abline(h=0)
## compute the variances
apply(estnw,1,var)
apply(estlp,1,var)
apply(estss,1,var)

par(mfrow=c(1,3))
matplot(x, estnw, type = "l", col = "grey")
lines(x,apply(estnw,1,mean),col="red")
matplot(x, estlp, type = "l", col = "grey")
lines(x,apply(estlp,1,mean),col="red")
matplot(x, estss, type = "l", col = "grey")
lines(x,apply(estss,1,mean),col="red")

## how many times does the pointwise confidence interval at x=0.5
## contain the true value m(0.5)?
## x = 0.5
nonw <- nolp <- noss <- 0
for(i in 1:nrep){
  nonw = nonw + ((m(0.5) > estnw[x==0.5,i] - 1.96*senw[x==0.5,i]) &&
    (m(0.5) < estnw[x==0.5,i] + 1.96*senw[x==0.5,i]))
  nolp = nolp + ((m(0.5) > estlp[x==0.5,i] - 1.96*selp[x==0.5,i]) &&
    (m(0.5) < estlp[x==0.5,i] + 1.96*selp[x==0.5,i]))
  noss = noss + ((m(0.5) > estss[x==0.5,i] - 1.96*sess[x==0.5,i]) &&
    (m(0.5) < estss[x==0.5,i] + 1.96*sess[x==0.5,i]))
}
