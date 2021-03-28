library(survivoR)
library(tidyverse)
library(gbm)
library(nnet)
library(randomForest)
library(ALEPlot)

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
  mutate(predicted=c(yhat),
         correct=as.numeric(predicted==winner))


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

# Final training

train <- df_norm[,-(1:3)]


final_fit <- nnet(winner~., size=2, data=train, linout=T,
                  maxit=1000, decay=0.037, skip=FALSE, trace=FALSE)
arrange(varImp(final_nn), desc(Overall))

final_gbm <- gbm(winner~., data=train, var.monotone=NULL,
                                       distribution = "bernoulli",
                                       n.trees=1000,
                                       shrinkage=0.333,
                                       interaction.depth=2,
                                       bag.fraction = .5,
                                       n.minobsinnode = 2,
                                       cv.folds=2)

yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata, type="raw"))
par(mfrow=c(2,4))
for (j in 1:7)  {
  ALEPlot(as.data.frame(train[,-1]), final_nn, pred.fun=yhat, J=j, K=50, NA.plot = TRUE)
}
