library(tidyverse)
library(survivoR)

source("cleaning.R")

getSeasonSummary <- function(seasonno) {
  vh %>% 
    filter(season == seasonno) %>% 
    pivot_wider(id_cols=castaway, names_from=order, values_from=vote,
                values_fn = last)
}

p <- getSeasonSummary(20) %>% 
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

# Were they on the right side of the vote?
apply(p, 1, function(x) x==t)

dist(t(apply(p, 1, function(x) x==t)))
