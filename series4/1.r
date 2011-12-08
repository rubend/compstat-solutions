## Author: Ruben Dezeure
## part c)
bmwlr <- scan("http://stat.ethz.ch/Teaching/Datasets/bmw.dat")
## ksmooth orders the data internally so we should do that beforehand for everything so that it gives comparable results
x <- bmwlr[1:999]
ox <- order(x)
x <- x[ox]
y <- bmwlr[2:1000]
y <- y^2
y <- y[ox]
par(mfrow=c(1,2))
acf(bmwlr)
acf(bmwlr^2)
plot(x,y)

## part d)
bmwloess <- loess(y~x)
estdf <- bmwloess$trace.hat
bmwspline <- smooth.spline(x,y,df=estdf)
bmwnw <- ksmooth(x,y,kernel = "normal",bandwidth=3.53)

## we need the residuals for later on
y.loess <- fitted(bmwloess)
y.spline <- fitted(bmwspline)
y.nw <- ksmooth(x,y,kernel = "normal",bandwidth=3.53,x.points=x)$y

res.loess <- y - y.loess
res.spline <- y - y.spline
res.nw <- y - y.nw

## results
par(mfrow=c(2,1))
plot(x,y,main = "The original data x,y",col="gray")
lines(x,y.loess,col="yellow")
lines(x,y.spline,col="black")
lines(x,y.nw,col="red")

## check model assumptions
##tukey-anscombe plot
par(mfrow=c(1,3))
plot(y.loess,res.loess)
plot(y.spline,res.spline)
plot(y.nw,res.nw)

plot(ox,res.loess,type="h")
plot(ox,res.spline,type="h")
plot(ox,res.nw,type="h")
abline(h=0)
##qqnorm(res.loess)
##qqnorm(res.spline)
##qqnorm(res.nw)

xout <- seq(-15,15,length=length(x))
library(lokern)
bmw.glkerns <- glkerns(x,y)
bmw.lokerns <- lokerns(x,y)

other.glkerns <- glkerns(x,y,x.out=xout)$est
other.lokerns <- lokerns(x,y,x.out=xout)$est

y.glkerns <- fitted(bmw.glkerns)
plot(x,y)
lines(xout,other.glkerns,lty=3,col="red")

y.lokerns <- fitted(bmw.lokerns)
lines(xout,other.lokerns,lty=5,col="blue")
legend(6,70,legend=c("glkerns","lokerns"),lty=c(3,5),col=c("red","blue"))
res.glkerns <- y-y.glkerns
res.lokerns <- y-y.lokerns

## check model assumptions
## Try tukey-anscombe plot
plot(y.glkerns,res.glkerns)
plot(y.lokerns,res.lokerns)

plot(ox,y.glkerns,type="h")
plot(ox,y.lokerns,type="h")
plot(ox,res.spline,type="h",col="red")
lines(ox,y.lokerns,type="h")

## plot the local bandwidths from lokerns and compare them to the global bandwidth of the function glkerns
plot(bmw.lokerns$bandwidth)
abline(h=bmw.glkerns$bandwidth,col="red")
