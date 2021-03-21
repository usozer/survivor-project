library(survivoR)
library(tidyverse)

source("fns.R")

# Logistic regression
winner_log <- glm(winner~age+immunity_idols_won+appearance+factor(returning)+jury_simil, family=binomial, data=finalists)
summary(winner_log)

# Boosted tree
# TUNE GBM
winner_gbm <- gbm(winner~age+immunity_idols_won+appearance+factor(returning), data=df)


finalists$winnerpred =predict(winner_gbm, finalists, type="response")
finalists <- finalists %>% 
  group_by(season) %>% 
  mutate(winnerpred = winnerpred/sum(winnerpred),
         predicted= as.numeric(winnerpred==max(winnerpred))) %>% 
  ungroup()

# Correct prediction rate
sum(finalists[finalists$result == "Sole Survivor","predicted"])/40