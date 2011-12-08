## Author: Ruben Dezeure
vehicle <- read.table("http://stat.ethz.ch/Teaching/Datasets/NDK/vehicle.dat")

library(rpart)
tree <- rpart(Class~.,data=vehicle,
              control = rpart.control(cp=0.0,minsplit=30))
plot(tree, uniform=TRUE)
text(tree,use.n=TRUE,all=TRUE,cex=0.8,fancy=FALSE,pretty=3)

##b)
## access the cost-complexity table
printcp(tree)
plotcp(tree)
## the one we chose has size 7 --> 6 splits have been done
## we look it up in princp table and we see corresponding cp is
## cp = 0.0127389
bestcp <- 0.0127389

par(mfrow=c(1,2))
tree.pruned <- prune.rpart(tree,cp=bestcp)
plot(tree.pruned,uniform=TRUE)
text(tree.pruned,n=TRUE,all=TRUE,cex=0.8,fancy=FALSE,pretty=3)
plot(tree, uniform=TRUE)
text(tree,use.n=TRUE,all=TRUE,cex=0.8,fancy=FALSE,pretty=3)

## calculate the misclassification rate
res.tree <- residuals(tree)
mis.tree <- sum(res.tree)/length(res.tree)
res.prunedtree <- residuals(tree.pruned)
mis.prunedtree <- sum(res.prunedtree)/length(res.prunedtree)

mis.tree
mis.prunedtree

## c)
## investigate the predictive power. Compute the bootstrap generalization error and the leave-one-out cv performance
B <- 1000
set.seed(100)
rpart.funct <- function(data,ind){
  dat <- data[ind,]
  boot.tree <- rpart(Class~.,data=dat,cp=bestcp)
  sum(residuals(boot.tree))/length(residuals(boot.tree))
}
boot.rpart <- boot(data=vehicle,statistic=rpart.funct,R=B)
bootstrap.ge <- mean(boot.rpart$t)
bootstrap.ge ## the bootstrap generalization error

## leave-one-out cv
cv.lo <- 0
for(i in 1:nrow(vehicle)){
  dat <- vehicle[-i,]
  class.pred <- predict(rpart(Class~.,data=dat,cp=bestcp),newdata=vehicle[i,],type="class")
  cv.lo <- cv.lo + (vehicle[i,"Class"] != class.pred)
}
cv.lo/nrow(vehicle)

## out-of-sample generalization error
rpart.outofsample <- function(data,ind){
  dat <- data[ind,]
  boot.tree <- rpart(Class~.,data=dat,cp=bestcp)
  outof.ind <- setdiff(1:nrow(data),ind)
  outofsample <- data[outof.ind,]
  err <-sum(vehicle[outof.ind,"Class"]
                  != predict(boot.tree,newdata=outofsample,type="class"))

  err/nrow(outofsample)
}
boot.rpart.outofsample <- boot(data=vehicle,statistic=rpart.outofsample,R=B)
bootstrap.ge <- mean(boot.rpart.outofsample$t)
bootstrap.ge ## 0.3289015

