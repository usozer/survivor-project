library(survivoR)
library(tidyverse)
library(gbm)
library(nnet)
library(randomForest)

source("fns.R")

Nrep <- 1
K<-40  #K-fold CV on each replicate
n.models = 5 #number of different models to fit
n=40
y=df_norm$winner
yhat=matrix(0,nrow(df_norm),n.models)
misclass<-matrix(0,Nrep,n.models)

for (k in 1:K) {
  train_ind <- filter(df_norm, season != k) %>% pull(sid)
  train <- df_norm[train_ind,-(1:3)]
  test_ind <- filter(df_norm, season == k) %>% pull(sid)
  
  out
  yhat[test_ind,1] <- generateWinner(k, out)
  
  out
  yhat[test_ind,2] <- generateWinner(k, out)
  
  out
  yhat[test_ind,3] <- generateWinner(k, out)
  
  out
  yhat[test_ind,4] <- generateWinner(k, out)
  
  out <- glm(winner~., 
             family=binomial, 
             data=train)
  yhat[test_ind,5] <- generateWinner(k, out)
} #end of k loop
  misclass[1,]=apply(yhat,2,function(x) sum(x != y)/length(x))
 #end of j loop

misclass



generateWinner <- function(seasonno, fit, type="response") {
  p<- df_norm %>% 
    filter(season == seasonno) %>% 
    mutate(f = predict(winner_log, ., type)) %>% 
    mutate(winnerpred = f/sum(f),
           predicted= as.numeric(winnerpred==max(winnerpred)))
  
  return(pull(p, predicted))
}








# Correct prediction rate
sum(finalists[finalists$result == "Sole Survivor","predicted"])/40




# Descriptive stats
# Mean of majority vote
# Mean of prevtribe_jury (less than 50% means what?)



finalists <- as_tibble(finalists)

finalists %>% 
  select(age, immunity_idols_won, appearance, 
         jury_simil, prevtribe_jury, rightside) %>% 
  cor()



train <- df_norm[,4:11]
train$winner <- factor(train$winner)

# Logistic regression
winner_log <- glm(winner~., family=binomial, data=train)
summary(winner_log)

prediction <- predict(winner_log, df_norm, type="response")

preds <- df_norm %>% 
  mutate(prediction=prediction) %>% 
  group_by(season) %>% 
  mutate(perc_prob = prediction/sum(prediction),
         predicted = as.numeric(perc_prob == max(perc_prob)))

preds %>% 
  filter(winner==1 & predicted == 1) %>% 
  View()

# Boosted tree
# TUNE GBM
winner_nn <- nnet(factor(winner)~., size=4, data=train, maxit=1000)

predict(winner_nn, df_norm)

