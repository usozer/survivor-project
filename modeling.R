library(survivoR)
library(tidyverse)
library(gbm)
library(nnet)
library(randomForest)

setwd("~/survivor-project")
source("fns_normalized.R")


generateWinner <- function(seasonno, fit, type, ...) {
  p<- df_norm %>% 
    filter(season == seasonno) %>% 
    mutate(f = predict(fit, ., type, ...)) %>% 
    mutate(predicted= as.numeric(f==max(f)))
  
  return(pull(p, predicted))
}


# Neural network

df_norm$winner <- factor(df_norm$winner)

nodes=1:3
lambdas=3^(-5:2)
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

for (k in 18:K) {
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



# 
# #
# #
# #
# #
# #
# finalists <- as_tibble(finalists)
# 
# finalists %>% 
#   select(age, immunity_idols_won, appearance, 
#          jury_simil, prevtribe_jury, rightside) %>% 
#   cor()
# 
# 
# 
# train <- df_norm[,4:11]
# train$winner <- factor(train$winner)
# 
# # Logistic regression
# winner_log <- glm(winner~., family=binomial, data=train)
# summary(winner_log)
# 
# prediction <- predict(winner_log, df_norm, type="response")
# 
# preds <- df_norm %>% 
#   mutate(prediction=prediction) %>% 
#   group_by(season) %>% 
#   mutate(perc_prob = prediction/sum(prediction),
#          predicted = as.numeric(perc_prob == max(perc_prob)))
# 
# preds %>% 
#   filter(winner==1 & predicted == 1) %>% 
#   View()
# 
# # Boosted tree
# # TUNE GBM
# winner_nn <- nnet(factor(winner)~., size=4, data=train, maxit=1000)
# 
# predict(winner_nn, df_norm)


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