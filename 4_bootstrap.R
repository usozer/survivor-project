library(nnet)

source("2-1_fns_normalized.R")

# Bootstrapping

no.reps <- 1000
yhat=matrix(0,105,no.reps)

progress <- 0
bar <- txtProgressBar(min=0, max=40000, style=3)
for (k in 1:40) {
  test_ind <- filter(df_norm, season == k) %>% pull(sid)
  
  for (i in 1:no.reps) {
    seasons <- sample((1:40)[-k], 39, replace=TRUE)
    
    train <- data.frame()
    for (a in seasons) train <- rbind(train, filter(df_norm, season==a))
    train <- train[,4:11]
    
    out <- nnet(winner~., size=2, data=train, linout=F, maxit=1000, decay=0.037, skip=FALSE, trace=FALSE)
    yhat[test_ind,i] <- generateWinner(k, out, type="raw")
    
    progress <- progress+1
    setTxtProgressBar(bar, progress)
  }
  
} #end of k loop

