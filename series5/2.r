## Author: Ruben Dezeure
diabetes <- read.table("http://stat.ethz.ch/Teaching/Datasets/diabetes2.dat",header=TRUE)
reg <- diabetes[,c("Age","C.Peptide")]
names(reg) <- c("x","y")
## sort the observations along x, for easier dealing of hat matrix:
reg <- reg[sort.list(reg$x),]

x <- reg$x
y <- reg$y

dof <- 0
## plot(x,y) to guess a good bandwidth
plot(reg$x,reg$y,col="grey")
fit.kernel <- ksmooth(reg$x,reg$y,bandwidth=2,x.points=reg$x)$y
lines(reg$x,fit.kernel,lty=3,col="yellow")
fit.kernel <- ksmooth(reg$x,reg$y,bandwidth=2.5,x.points=reg$x)$y
lines(reg$x,fit.kernel,lty=3)
fit.kernel <- ksmooth(reg$x,reg$y,bandwidth=3,x.points=reg$x)$y
lines(reg$x,fit.kernel,lty=3,col="magenta")
fit.kernel <- ksmooth(reg$x,reg$y,bandwidth=3.5,x.points=reg$x)$y
lines(reg$x,fit.kernel,lty=3,col="orange")

fit.kernel <- ksmooth(reg$x,reg$y,bandwidth=4,x.points=reg$x)$y
lines(reg$x,fit.kernel,lty=3,col="blue")
fit.kernel <- ksmooth(reg$x,reg$y,bandwidth=4.5,x.points=reg$x)$y
lines(reg$x,fit.kernel,lty=3,col="red")
fit.kernel <- ksmooth(reg$x,reg$y,bandwidth=5,x.points=reg$x)$y
lines(reg$x,fit.kernel,lty=3,col="green")


## 4 seems about right
bw <- 4

## calculating cv through the S matrix for ksmooth
In <- diag(length(x))
S <- matrix(0,length(x),length(x))
for(i in 1 : length(x)){
  S[,i] <- ksmooth(x,In[,i],kernel="normal",x.point=x,bandwidth=bw)$y
}
## fit with the full data
fit.ksm <- ksmooth(x,y,kernel="normal",bandwidth=bw,x.point=x)$y
  
## calculate the leave-one-out cv for smoothing spline fit

cv.nw <- mean(((y-fit.ksm)/(1-diag(S)))^2)
## is the same as the following
sum <- 0
for(i in 1:length(x)){
  sum <- sum + ((y[i]-fit.ksm[i])/(1-S[i,i]))^2
}
sum <- sum/length(x)
## cv = 0.3905108

## doing leave one out by hand for ksmooth: (nw)
## so calculatin the cv the traditional way
cv.manual <- 0
for(i in 1:length(x)){
  cdat <- reg[-i,]
  yval <- ksmooth(cdat$x,cdat$y,kernel="normal",bandwidth=bw,x.point = reg$x[i])$y
  cv.manual <- cv.manual + (reg$y[i]-yval)^2
}
cv.manual <- cv.manual/length(x)
## cv.manual = 0.3905108 exactly the same!!! :p awesome!


df <- sum(diag(S)) ## dof needed for further calculations and comparable results of the different methods

reg.fcn.lp <- function(reg.x,reg.y,x){
  lp.reg <- loess(reg.y~reg.x,enp.target=df,surface="direct")
  predict(lp.reg,x)
}

## do the local polynomial fit
In <- diag(length(x))
S.loess <- matrix(0,length(x),length(x))
for(i in 1 : length(x)){
  S.loess[,i] <- reg.fcn.lp(x,In[,i],x)
}
## fit with the full data
fit.loess <- reg.fcn.lp(x,y,x)
  
## calculate the leave-one-out cv for smoothing spline fit

cv.loess <- mean(((y-fit.loess)/(1-diag(S.loess)))^2)

## do smoothing spline fit with both the df calculated earlier and the self optimised by the method one!
off <- duplicated(reg$x)
newx <- unique(reg$x)
newy <- reg$y[!off]

## smoothing spline fit automatic cv calculation in the function : wir brauchen eindeutigen 'x' Werte fur die option cv=TRUE :s
cv.self <- smooth.spline(newx,newy,cv=TRUE,df=df)$cv.crit
cv.optim <- smooth.spline(newx,newy,cv=TRUE)$cv.crit ## no df specified --> leave one out cv optimised lambda chosen

## get S matrix out of smooth.spline
In <- diag(1,length(newx))
S <- S.opt<- matrix(0,length(newx),length(newx))
for(i in 1 : length(newx)){
  S[,i] <- fitted(smooth.spline(newx,In[,i],df=df))
  S.opt[,i] <- fitted(smooth.spline(newx,In[,i]))
}

## fit with the full data
fit.spline.opt <- fitted(smooth.spline(newx,newy)) ## why the f is this blown up? the S matrix has elements 1 in there!
## MAYBE BECAUSE IT DETERMINES IT'S OWN LAMBDA WHEN DOING THE FIT ON In! It's not the same lambda as if we do smooth.spline(x,y)!
fit.spline.self <- fitted(smooth.spline(newx,newy),df=dof)

## calculate the leave-one-out cv for smoothing spline fit
cv.spline.opt <- mean(((newy-fit.spline.opt)/(1-diag(S.opt)))^2)
cv.spline.self <- mean(((newy-fit.spline.self)/(1-diag(S)))^2)

##now fitting with the constant fit
## leave one out cv
cv.manual <- 0
for(i in 1:length(x)){
  cdat <- reg[-i,]
  yval <- mean(cdat$y)
  cv.manual <- cv.manual + (reg$y[i]-yval)^2
}
cv.manual <- cv.manual/length(x)
