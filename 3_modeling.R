library(survivoR)
library(tidyverse)
library(gbm)
library(nnet)
library(randomForest)

setwd("~/SurvivorPred")
source("2-1_fns_normalized.R")


generateWinner <- function(seasonno, fit, type, ...) {
  p<- df_norm %>% 
    filter(season == seasonno) %>% 
    mutate(f = predict(fit, ., type, ...)) %>% 
    mutate(predicted= as.numeric(f==max(f)))
  
  return(pull(p, predicted))
}


# Neural network

df_norm$winner <- factor(df_norm$winner)

nodes=(1:3)
lambdas=(3^(-5:2))
Nrep <- 1
K<-40  #K-fold CV on each replicate
n.models = length(nodes)*length(lambdas) #number of different models to fit
n=40
y=df_norm$winner
yhat=matrix(0,nrow(df_norm),n.models)
misclass<-matrix(0,Nrep,n.models)

for (k in 1:K) {
  train_ind <- filter(df_norm, season != k) %>% pull(sid)
  train <- df_norm[train_ind,-(1:3)]
  test_ind <- filter(df_norm, season == k) %>% pull(sid)
  
  cur <- 1
  for (node in nodes) {
    for (lambda in lambdas) {
      out <- nnet(winner~., size=node, data=train, linout=F, maxit=1000, decay=lambda, skip=FALSE, trace=FALSE)
      yhat[test_ind,cur] <- generateWinner(k, out, type="raw")
      cur <- cur + 1
    }
  }
} #end of k loop

misclass[1,]=apply(yhat,2,function(x) sum(x != y)/length(x))
 #end of j loop

matrix(c(misclass), nrow=length(nodes), 
                    ncol=length(lambdas), 
                    byrow=TRUE, 
                    dimnames = list(paste(nodes,"nodes"),
                                    paste(round(lambdas,3))))


# Descriptive stats
# Mean of majority vote
# Mean of prevtribe_jury (less than 50% means what?)

#
#
#

# Boosted tree

train$winner <- as.numeric(train$winner)-1

nodes=1:4
lambdas=3^(-6:0)
Nrep <- 1
K<-40  #K-fold CV on each replicate
n.models = length(nodes)*length(lambdas) #number of different models to fit
n=40
y=df_norm$winner
yhat=matrix(0,nrow(df_norm),n.models)
misclass<-matrix(0,Nrep,n.models)

for (k in 1:K) {
  train_ind <- filter(df_norm, season != k) %>% pull(sid)
  train <- df_norm[train_ind,-(1:3)]
  test_ind <- filter(df_norm, season == k) %>% pull(sid)
  
  cur <- 1
  for (node in nodes) {
    for (lambda in lambdas) {
      out<- gbm(winner~., data=train, var.monotone=NULL, 
                                      distribution = "bernoulli",
                                      n.trees=1000, 
                                      shrinkage=lambda, 
                                      interaction.depth=3, 
                                      bag.fraction = .5, 
                                      n.minobsinnode = node,
                                      cv.folds=2,
                                      verbose=FALSE)
      yhat[test_ind,cur] <- generateWinner(k, out, type="response", n.trees=gbm.perf(out, plot.it=FALSE))
      cur <- cur + 1
      print(cur)
    }
  }
} #end of k loop

misclass[1,]=apply(yhat,2,function(x) sum(x != y)/length(x))
#end of j loop

matrix(c(misclass), nrow=length(nodes), 
       ncol=length(lambdas), 
       byrow=TRUE, 
       dimnames = list(paste(nodes,"interactions"),
                       paste(round(lambdas,3))))


############################################################
set.seed(123)
yhat=matrix(0,nrow(df_norm),1)

for (k in 1:40) {
  train_ind <- filter(df_norm, season != k) %>% pull(sid)
  train <- df_norm[train_ind,-(1:3)]
  test_ind <- filter(df_norm, season == k) %>% pull(sid)
  
  out <- nnet(winner~., size=2, data=train, linout=F, maxit=1000, decay=0.037, skip=FALSE, trace=FALSE)
  yhat[test_ind,1] <- generateWinner(k, out, type="raw")
} #end of k loop

df_norm <- df_norm %>% 
  mutate(predicted=c(yhat))


#############################################################

##############################################################

# Logistic regression

train$winner <- as.numeric(train$winner)-1

Nrep <- 1
K<-40  #K-fold CV on each replicate
n.models = 1 #number of different models to fit
n=40
y=df_norm$winner
yhat=matrix(0,nrow(df_norm),n.models)
misclass<-matrix(0,Nrep,n.models)

for (k in 1:K) {
  train_ind <- filter(df_norm, season != k) %>% pull(sid)
  train <- df_norm[train_ind,-(1:3)]
  test_ind <- filter(df_norm, season == k) %>% pull(sid)
  
  out <- glm(factor(winner)~., data=train, family=binomial)
  yhat[test_ind,1] <- generateWinner(k, out, type="response")
} #end of k loop

misclass[1,]=apply(yhat,2,function(x) sum(x != y)/length(x)); misclass

##############################################################

#k-nn









# neural net
# 0.004     0.012     0.037     0.111     0.333         1         3         9
# 1 nodes 0.4476190 0.4380952 0.4571429 0.4190476 0.4190476 0.4761905 0.4761905 0.4761905
# 2 nodes 0.4761905 0.4571429 0.4380952 0.4380952 0.4380952 0.4761905 0.4761905 0.4761905
# 3 nodes 0.4476190 0.4571429 0.5142857 0.4952381 0.4380952 0.4761905 0.4761905 0.4761905


# 
# gbm
# 0.001     0.004     0.012     0.037     0.111     0.333         1
# 1 interactions 0.4666667 0.4476190 0.4952381 0.5142857 0.4952381 0.4095238 0.4380952
# 2 interactions 0.4285714 0.4761905 0.4857143 0.3809524 0.4285714 0.4761905 0.4380952
# 3 interactions 0.4666667 0.5238095 0.5142857 0.5333333 0.4761905 0.4761905 0.5238095
# 4 interactions 0.4761905 0.4095238 0.4857143 0.5142857 0.4380952 0.5333333 0.4571429
# 
# 1000 simulations
# [1] 905  95 822 178  50 950 396 604 352 648  50 950 266 734 615 385 128 872 679 321 800 200 440 560 252 747   1 861  20 119 598 102 300 869
# [35] 131 912   2  86 222 778 323   3 674  36 864 100   1 541 458 631   9 360 123 388 489 384 182 434  90  85 825 380  16 604 101 185 714 939
# [69]  61  21 913  66 487  53 460 970  30   0  20 309 671 415 443 142 268 442 290 613 382   5 532 258 210 447  11 542  41 773 186 187 448 365
# [103] 735 188  77