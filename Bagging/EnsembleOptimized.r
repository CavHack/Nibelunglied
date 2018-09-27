#----library--------
library(anytime)
library(rowr)
library(elmNN) #extreme learning machine
library(rBayesianOptimization)
library(foreach)
library(magrittr)
library(clusterSim)
library(doRNG) #Pseudo randomness

#-----prepare-------------
evalq({
  
  dt <- PrepareData(Data, Max, Min, Errors, Latency)
  DT <- SplitData(dt, 4000, 5000, 500, 250, start = 1)
  pre.outl <- PreOutlier(DT$pretrain)
  DTcap <- CappingData(DT, impute = T, fill = T, dither = F, pre.outl = pre.outl)
  preproc <- PreNorm(DTcap, meth = meth)
  DTcap.n <- NormData(DTcap, preproc = preproc)
  
  #---X-Topology
  list(
    pretrain = list(
      x = DTcap.n$pretrain %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DTcap.n$pretrain$Class %>% as.numeric() %>% subtract(1)
    ),
    train = list(
      x = DTcap.n$train %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
      y = DTcap.n$train$Class %>% as.numeric() %>% subtract(1)
    
  ),
  test = list(
    x = DTcap.n$val %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
    y = DTcap.n$val$Class %>% as.numeric() %>% subtract(1)
  
  ),
  test1 = list(
    x = DTcap.n$test %>% dplyr::select(-c(Data, Class)) %>% as.data.frame(),
    y = DTcap.n$test$Class %>% as.numeric() %>% subtract(1)
  
  )
  
) -> x
  
  #---2----bestF---------
  #require(clusterSim)
  numFeature <- 10
  HINoV.mod(x = X$pretrain$x %>% as.matrix(), type = "metric", s = 1, 4,
            distance = NULL,
            method = "kmeans",
            index = "cRAND") %>% stopri[,1] -> orderX
  orderX %>% head(numFeature) -> bestF}, env)

#------Train-----------
evalq({
  
    Xtrain <- X$pretrain$x[, bestF]
    Ytrain <- X$pretrain$y
    setMKLthreads(1)
    n <- 500
    r <- 7
    nh <- 5
    k <- 1
    rng = RNGseq(n, 12345)
    Ens <- foreach(i = 1:n, .packages = "elmNN") %do% {
      
      rngtools::setRNG(rng[[k]])
      k <- k + 1
      idx <- rminer::holdout(Ytrain, ratio = r/10, mode = "random")$tr
      elmtrain(x = Xtrain[idx, ], y = Ytrain[idx],
               nhid = nh, actfun = "sin")
      
    }
    
    setMKLthreads(2)

  
}, env)

#----4------Predict-----------

evalq({
  
  Xtest <- X$train$x[, bestF]
  Ytest <- x$train$y
  foreach(i = 1:n, .packages= "elmNN", .combine="cbind") %do% {
    predict(Ens[[i]], newdata = Xtest)
  } -> y.latdat #[ ,n]
  
  
}, env)

#--5--------best---=----
evalq({
  
  numEns <- 3
  foreach(i = 1:n, .combine="c") %do% {
    ifelse(y.latdat[ ,i] > 0.5, 1, 0) -> Ypred
    Evaluate(actual = Ytest, predicted = Ypred)$Metrics$F1 %>%
      mean()
  } -> Score
  Score %>% order(decreasing= TRUE) %>% head((numEns*2+1)) -> bestNN
  Score[bestNN] %>% round(3)
  
}, env)

















  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}, env)