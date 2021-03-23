source("2-1_fns_normalized.R")

df %>% 
  mutate(relevance=case_when(jury==1 ~ "jury",
                             ftc==1 ~ "ftc")) %>% 
  filter(!is.na(relevance)) %>% 
  select(age, relevance) %>% 
  ggplot() + geom_density()