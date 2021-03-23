library(cluster)

source("2-1_fns_normalized.R")

mat <- df_norm %>% 
  select(age, necklaces, appearance, 
         jury_simil, prevtribe_jury, majorityvote) %>% 
  as.matrix()


F = double(9) 
SSE = double(9) 
si = double(9)
for(K in 2:10) {
  set.seed(123)
  kfit <- kmeans(mat, K, nstart=100, iter.max = 100) 
  F[K-1] = (kfit$betweenss/(K-1))/(kfit$tot.withinss/(nrow(mat)-K)) 
  SSE[K-1] = kfit$tot.withinss
  si2 = silhouette(kfit$cluster, dist(mat, "euclidean"))
  si[K-1] = summary(si2)$avg.width
}

plot(2:10, SSE, type="b", xlab="Number Clusters K")
plot(2:10, F, type="b", xlab="Number Clusters K")
plot(2:10, si, type="b", xlab="Number Clusters K")

##################

kfit <- kmeans(mat, 4, nstart=100, iter.max = 100) 

plot.kmeans(kfit)

summary.kmeans(kfit)

df_norm %>% 
  mutate(cluster = kfit$cluster,
         winner=as.numeric(winner)) %>% 
  group_by(cluster) %>% 
  summarise(winrate=sum(winner)/n())


factors = princomp(mat)

df_norm %>% 
  mutate(fc1 = factors$scores[,1],
         fc2 = factors$scores[,2],
         fc3 = factors$scores[,3],
         cluster = factor(kfit$cluster)) %>% 
  ggplot(aes(x=fc1, y=fc2)) + geom_point(aes(color=cluster))

df_norm %>% 
  mutate(fc1 = factors$scores[,1],
         fc2 = factors$scores[,2],
         fc3 = factors$scores[,3],
         cluster = factor(kfit$cluster)) %>% 
  ggplot(aes(x=fc3, y=fc2)) + geom_point(aes(color=cluster))
