library(tidyverse)
library(survivoR)
library(usedist)

source("cleaning.R")


df <- s %>% 
  select(sid, castaway, season, season_name, age, day, order, result,
         immunity_idols_won, jury, ftc, appearance, returning) %>% 
  group_by(season) %>% 
  mutate(rel_order = order/max(order),
         age = (age-mean(age))/sd(age), 
         appearance = (appearance-mean(appearance)),
         immunity_idols_won = immunity_idols_won - mean(immunity_idols_won),
         winner = as.numeric(result == "Sole Survivor"),
         season_name = str_match(season_name, "Survivor: (.+)")[,2],
         .after=order) %>% 
  ungroup()

order_ln <- lm(rel_order~age+immunity_idols_won+appearance+factor(returning), data=df)
summary(order_ln)

winner_log <- glm(winner~age+immunity_idols_won+appearance+factor(returning), family=binomial, data=finalists)
summary(winner_log)

winner_gbm <- gbm(winner~age+immunity_idols_won+appearance+factor(returning), data=df)


df[predict(winner_log, df, type="response") %>% 
  sort(decreasing = TRUE) %>% 
  head(15) %>% 
  names() %>% 
  as.numeric(),]

df %>% 
  filter(sid %in% (df %>% 
                     filter(result %in% c("Sole Survivor","Runner-up","2nd runner-up","Co-runner-up","2nd Runner-up")) %>% 
                     pull(sid))) %>% 
  mutate(winnerpred = predict(winner_log, ., type="response")) %>% 
  group_by(season) %>% 
  mutate(winnerpred = winnerpred/sum(winnerpred)) %>% 
  View()

################################################################################

# Were they on the right side of the vote?

getSeasonSummary <- function(seasonno) {
  vh %>% 
    filter(season == seasonno) %>% 
    pivot_wider(id_cols=castaway, names_from=order, values_from=vote,
                values_fn = last)
}

p <- getSeasonSummary(15) %>% 
  as.matrix()
rownames(p) <- p[,1]
p <- p[,-1]



getVoteHistory <- function(seasonno) {
  s %>% 
    filter(season == seasonno) %>% 
    arrange(order) %>% 
    pull(castaway)
}

t <- getVoteHistory(20)
t <- t[as.numeric(colnames(p))]


simil(t(apply(p, 1, function(x) as.numeric(x==t))), method="cosine")

dist(t(apply(p, 1, function(x) as.numeric(x==t))), method="")




calcJurySimil <- function(seasonno, castaway) {
  # Jaccard distance
  p <- getSeasonSummary(seasonno) %>% 
    as.matrix()
  rownames(p) <- p[,1]
  p <- p[,-1]
  
  distmat <- dist_make(p, distance_fcn = function(v1, v2) {sum(as.numeric(v1 == v2), na.rm=T)/sum(!is.na(v1==v2))})
  jury <- df %>% filter(season==seasonno, jury==1) %>% pull(castaway)
  final <- df %>% filter(season==seasonno, ftc==1) %>% pull(castaway)
  return(as.numeric(rowMeans(as.matrix(distmat)[final,jury], na.rm=TRUE)[castaway]))
}


finalists <- (df %>% filter(ftc==1) %>% as.data.frame())
finalists[,"season"] = as.double(finalists[,"season"])

finalists$jury_simil = 0
finalists$jury_simil[1:18] = apply(finalists[1:18,],1, function(x) calcJurySimil(x[3], x[2]))
finalists$jury_simil[19:90] = apply(finalists[19:90,],1, function(x) calcJurySimil(x[3], x[2]))
finalists$jury_simil[94:105] = apply(finalists[94:105,],1, function(x) calcJurySimil(x[3], x[2]))

####
seasonno =36
p <- getSeasonSummary(seasonno) %>% 
  as.matrix()
rownames(p) <- p[,1]
p <- p[,-1]

distmat <- dist_make(p, distance_fcn = function(v1, v2) {sum(as.numeric(v1 == v2), na.rm=T)/sum(!is.na(v1==v2))})
jury <- df %>% filter(season==seasonno, jury==1) %>% pull(castaway)
final <- df %>% filter(season==seasonno, ftc==1) %>% pull(castaway)
####
finalists$jury_simil[91:93] = as.numeric(rowMeans(as.matrix(distmat)[final,jury[-1]], na.rm=TRUE))

finalists <- finalists %>% mutate(jury_simil=(jury_simil-mean(jury_simil))/sd(jury_simil))

winner_log <- glm(winner~age+immunity_idols_won+appearance+factor(returning)+jury_simil, family=binomial, data=finalists)
summary(winner_log)

winner_gbm <- gbm(winner~age+immunity_idols_won+appearance+factor(returning), data=df)

finalists <- finalists %>% 
  mutate(winnerpred = predict(winner_gbm, ., type="response")) %>% 
  group_by(season) %>% 
  mutate(winnerpred = winnerpred/sum(winnerpred),
         predicted= as.numeric(winnerpred==max(winnerpred)))

sum(finalists[finalists$result == "Sole Survivor","predicted"])

finalists$winnerpred =predict(winner_gbm, finalists, type="response")
finalists <- finalists %>% 
  group_by(season) %>% 
  mutate(winnerprediction = winnerpred/sum(winnerpred),
         predicted= as.numeric(winnerpred==max(winnerpred)))

