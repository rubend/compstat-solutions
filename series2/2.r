## a) I would guess 0.25 0.5 or so?
## b)
data <- numeric(100)
for(i in 1:100){
  p <- runif(1,min=0,max=1)
  if(p<0.2)
    data[i] <- rnorm(1,mean=0,sd=sqrt(0.01))
  else
    data[i] <- rnorm(1,mean=0,sd=0.01)
}

data

## compute the kernel density estimator
ke <- density(data,bw = 0.1,n=61,from=-1,to=5)

index <- c(9,11,13,16,21,26,31,36,41,51)
baw <- list(0.02,0.1,0.3,0.6,1,1.5,0.25,"sj")
quality <- matrix(numeric(200*8),200)
for(nsimul in 1:200){
  data <- numeric(100)
  for(i in 1:100){
    p <- runif(1,min=0,max=1)
    if(p<0.2)
      data[i] <- rnorm(1,mean=0,sd=sqrt(0.01))
    else
      data[i] <- rnorm(1,mean=0,sd=0.01)
  }

  for(j in 1:length(baw)){
    ke <- density(data,bw=baw[[j]],n=61,from=-1,to=5)
    dmix <- 0.2*dnorm(ke$x[index],mean = 0, sd = sqrt(0.01))+0.8*dnorm(ke$x[index],mean=2,sd=1)
    quality[nsimul,j] <- mean(ke$y[index] - dmix)^2
  }
  
}
avgquality <- apply(quality,2,mean)
min(avgquality)

ke <- density(data,bw=0.1,n=61,from=-1,to=5)
dmix <- 0.2*dnorm(ke$x[index],mean = 0, sd = sqrt(0.01))+0.8*dnorm(ke$x[index],mean=2,sd=1)
best <- mean(ke$y[index] - dmix)^2
best

