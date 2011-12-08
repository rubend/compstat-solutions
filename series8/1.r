## Author: Ruben Dezeure
## The dataset heart.dat contains data for 99 people sorted by age.
## In each age group the total number of individuals is known,
## as well the number of those with symptons of heart disease
heart <- read.table("http://stat.ethz.ch/Teaching/Datasets/heart.dat",header=TRUE)
## age = age
## m = the total number of individuals is known
## y = the number of those with symptoms of heart disease

fit <- glm(cbind(y,m-y) ~ age,family=binomial,data=heart)
## the coefficient with age is 0.1084 > 0 this means that the older
## you get the more chance you have of getting the symptons of heart disease

## plot the probability estimate against age.
## At what age would you expect 10% 20% ... 90% of people to have symptoms of heart disease?
## Discuss!
plot(heart$age,fitted(fit))
