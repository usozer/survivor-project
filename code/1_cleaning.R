# devtools::install_github("doehm/survivoR")

library(tidyverse)
library(survivoR)

s <- castaways  %>%
  group_by(castaway, season) %>%
  filter(day==max(day)) %>% # In case of vote out & comebacks, keep last one
  ungroup() %>%
  arrange(season) %>%
  mutate(sid=row_number()) %>% # assign unique ID to each Survivor appearance
  group_by(full_name) %>%
  mutate(appearance=row_number(),
         returning=if_else(appearance>1, 1, 0)) %>% # how many appearances, if they were a returning player that season
  ungroup() %>%
  mutate(jury=as.numeric(!is.na(jury_status)),
         ftc=as.numeric(result %in%
           c("Sole Survivor","Runner-up","2nd runner-up","Co-runner-up","2nd Runner-up")),
         .after=jury_status)

# Identify "unusual" non-returns,
# i.e. in a returning player season, original tribes have 100%
# return player ratio.
# Happens when castaways go under name changes
s %>%
  group_by(original_tribe) %>%
  summarise(perc=sum(returning)/n()) %>%
  arrange(desc(perc)) %>%
  filter(perc !=1)

s %>%
  select(sid, season, full_name, castaway, day,
         order, result, original_tribe, returning, appearance) %>%
  filter(original_tribe %in% c("Galang", "Malakal", "Dakal"))

# Manually fix 4 returning castaways with name changes
s[258,20:21] = list(2,1)
s[464,20:21] = list(3,1)
s[712,20:21] = list(3,1)
s[723,20:21] = list(2,1)

vh <- clean_votes(vote_history)
