library(tidyverse)
library(survivoR)
library(lubridate)
library(usedist)

source("1_cleaning.R")
source("0_enddates.R")

# Main dataframe
df <- s %>% 
  select(sid, castaway, season, season_name, age, day, order, result,
         original_tribe, immunity_idols_won, jury, ftc, appearance, returning) %>% 
  group_by(season) %>% 
  mutate(winner = as.numeric(result == "Sole Survivor"),
         season_name = str_match(season_name, "Survivor: (.+)")[,2],
         .after=order) %>% 
  ungroup()

# Normalized dataset
survivors <- s %>% 
  mutate(age = (age-mean(age))/sd(age)) %>% 
  select(sid, castaway, season, season_name, age, day, order, result,
         original_tribe, immunity_idols_won, jury, ftc, appearance, returning) %>% 
  group_by(season) %>% 
  mutate(immunity_idols_won = immunity_idols_won/sum(immunity_idols_won),
         winner = as.numeric(result == "Sole Survivor"),
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
  jury <- survivors %>% filter(season==seasonno, jury==1) %>% pull(castaway)
  final <- survivors %>% filter(season==seasonno, ftc==1) %>% pull(castaway)
  return(as.numeric(rowMeans(as.matrix(distmat)[final,jury], na.rm=TRUE)[castaway]))
}

finalists <- (survivors %>% filter(ftc==1) %>% as.data.frame())
finalists[,"season"] = as.double(finalists[,"season"])

finalists$jury_simil = 0
finalists$jury_simil[1:18] = apply(finalists[1:18,],1, function(x) calcJurySimil(x[3], x[2]))
finalists$jury_simil[19:90] = apply(finalists[19:90,],1, function(x) calcJurySimil(x[3], x[2]))
finalists$jury_simil[94:105] = apply(finalists[94:105,],1, function(x) calcJurySimil(x[3], x[2]))


#### Manual imputation for season 36
seasonno =36
p <- getSeasonSummary(seasonno) %>% 
  as.matrix()
rownames(p) <- p[,1]
p <- p[,-1]

distmat <- dist_make(p, distance_fcn = function(v1, v2) {sum(as.numeric(v1 == v2), na.rm=T)/sum(!is.na(v1==v2))})
jury <- survivors %>% filter(season==seasonno, jury==1) %>% pull(castaway)
final <- survivors %>% filter(season==seasonno, ftc==1) %>% pull(castaway)
####

finalists$jury_simil[91:93] = as.numeric(rowMeans(as.matrix(distmat)[final,jury[-1]], na.rm=TRUE))


clean_votes(vh) %>% 
  mutate(matched = as.numeric(vote==voted_out)) %>% 
  group_by(season, castaway) %>% 
  summarise(rightside=sum(matched)/n())

###########

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


df_unnorm <- finalists %>% 
  select(season, sid, castaway, winner, age, necklaces=immunity_idols_won,
         appearance, returning, jury_simil, prevtribe_jury, 
         majorityvote = rightside)

df_norm <- df_unnorm %>% 
  mutate(across(.cols = c(necklaces,
                          appearance,
                          jury_simil,
                          prevtribe_jury,
                          majorityvote),
                .fns = function (x) (x-mean(x))/sd(x)),
         returning=factor(returning),
         sid=row_number())
         

###############################################################

summary.kmeans <- function(fit){
  p = ncol(fit$centers)
  K = nrow(fit$centers)
  n = sum(fit$size)
  xbar = t(fit$centers)%*%fit$size/n
  print(data.frame(
    n=c(fit$size, n),
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*(fit$size-1)),
                        fit$tot.withinss/(p*(n-K)))), 4)
  ))
  cat("SSE=", fit$tot.withinss, "; SSB=", fit$betweenss, "; SST=", fit$totss, "\n")
  cat("R-Squared = ", fit$betweenss/fit$totss, "\n")
  cat("Pseudo F = ", (fit$betweenss/(K-1))/(fit$tot.withinss/(n-K)), "\n\n");
  invisible(list(Rsqr=fit$betweenss/fit$totss,
                 F=(fit$betweenss/(K-1))/(fit$tot.withinss/(n-K))) )
}

plot.kmeans <- function(fit,boxplot=F){
  require(lattice)
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers)))
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){
                  panel.dotplot(...)
                  panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1),
                xlab="Cluster Mean"))
  invisible(plotdat)
}

