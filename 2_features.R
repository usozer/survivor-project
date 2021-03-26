library(tidyverse)
library(survivoR)
library(lubridate)
library(usedist)

source("cleaning.R")
source("enddates.R")

df <- s %>% 
  select(sid, castaway, season, season_name, age, day, order, result,
         original_tribe, immunity_idols_won, jury, ftc, appearance, returning) %>% 
  group_by(season) %>% 
  mutate(winner = as.numeric(result == "Sole Survivor"),
         season_name = str_match(season_name, "Survivor: (.+)")[,2],
         .after=order) %>% 
  ungroup()

################################################################################

getSeasonSummary <- function(seasonno) {
  vh %>% 
    filter(season == seasonno) %>% 
    pivot_wider(id_cols=castaway, names_from=order, values_from=vote,
                values_fn = last)
}

getVoteHistory <- function(seasonno) {
  s %>% 
    filter(season == seasonno) %>% 
    arrange(order) %>% 
    pull(castaway)
}

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

finalists$jury_simil[91:93] = as.numeric(rowMeans(as.matrix(distmat)[final,jury[-1]], na.rm=TRUE))
####


clean_votes(vh) %>% 
  mutate(matched = as.numeric(vote==voted_out)) %>% 
  group_by(season, castaway) %>% 
  summarise(rightside=sum(matched)/n())

# perc. of former first-tribe members in jury
# votes received
# time of final tribal
# 

jurycomp <- s %>% 
  filter(jury == 1) %>%
  group_by(season, original_tribe) %>% 
  summarise(number = n()) %>% 
  ungroup(original_tribe) %>%
  mutate(perc = number/sum(number)) %>%
  ungroup()

votes <- clean_votes(vh) %>%
  mutate(matched = as.numeric(vote==voted_out)) %>% 
  group_by(season, castaway) %>% 
  summarise(rightside=sum(matched)/n())

finalists <- as_tibble(finalists)

finalists <- left_join(finalists, jurycomp, by=c("season", "original_tribe")) %>% 
  select(-number) %>% 
  rename(prevtribe_jury = perc) %>% 
  left_join(votes, by=c("season", "castaway")) %>% 
  left_join(select(season_summary, season, filming_ended), by="season") %>% 
  rename(tribaldate = filming_ended)

finalists$prevtribe_jury <- replace_na(finalists$prevtribe_jury, 0)


