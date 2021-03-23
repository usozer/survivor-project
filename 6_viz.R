source("2-1_fns_normalized.R")

rel <- df %>% 
  mutate(relevance=case_when(jury==1 ~ "jury",
                             ftc==1 ~ "ftc")) %>% 
  filter(!is.na(relevance))


write_csv(df, "df.csv")

ggplot(rel, aes(age)) + geom_density(aes(color = relevance)) 

