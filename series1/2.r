## simple linear regression
x <- seq(1,40,1)
y <- 2*x+1+5*rnorm(length(x))
reg <- lm(y~x)
summary(reg)
plot(x,y)
abline(reg)

## nonparametric regression
fit <- loess(y~x)
lines(predict(fit),lty=2)
txt <- c("Regression","Smooth")
legend("topleft",txt,lty=1:2)

## a) write a sequence of R commands which randomly generates 100
## times a vector of y-values according to the above model with the
## given x-values and genereates a vector of slopes of the regression lines.
slopes = numeric(100)

for (i in 1:100)
{
  x <- seq(1,40,1)
  y <- 2*x+1+5*rnorm(length(x))
  reg <- lm(y~x)
  slopes[i] <- reg$coefficients[2]
}

## draw a histogram of the 100 estimated slopes and add the normal density of the theoretically true distribution of the slopes to the histogram.
par(mfrow=c(1,2))
hist(slopes,freq=FALSE)
X <- cbind(rep(1,40),x)
standarddeviation <- sqrt(5^2*solve(t(X) %*% X)[2,2])
lines(seq(1.8,2.3,by=0.01),dnorm(seq(1.8,2.3,by=0.01),mean = 2,sd= standarddeviation))

## c)
mean(slopes)
sd(slopes)

## d)
slopes = numeric(100)

for (i in 1:100)
{
  x <- seq(1,40,1)
  y <- 2*x+1+5*(1-rchisq(length(x),df=1))/sqrt(2)
  reg <- lm(y~x)
  slopes[i] <- reg$coefficients[2]
}
## repetition of part b but now for the skewed distribution
hist(slopes,freq=FALSE)
X <- cbind(rep(1,40),x)
standarddeviation <- sqrt(5^2*solve(t(X) %*% X)[2,2])
lines(seq(1.8,2.3,by=0.01),dnorm(seq(1.8,2.3,by=0.01),mean = 2,sd= standarddeviation))
