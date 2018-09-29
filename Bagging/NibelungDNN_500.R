library(reticulate)
library(keras)
py_set_seed(12345)

num_classes <- 2L
batch_size <- 32L
learning_rate <- 0.001
epochs <- 100L

#----------------------------------
x_train <- env$res$InputTrain
y_train <- env$Ytest %>% to_categorical()
x_test <- env$res$InputTest
y_test <- env$Ytest1 %>% to_categorical()
x_test1 <- env$res$InputTest1
y_test1 <- env$Ytest2 %>% to_categorical()
#----------------------------
early_stopping <- callback_early_stopping(monitor="val_acc", 
                                          min_delta =1e-5,
                                          patience = 20, verbose = 0,
                                          mode = "auto")

#0-----nibelungDNNwithKeras--------------------



nibelungDNN500 <- keras_model_sequential()

#add layers and compile
nibelungDNN500 %>%
  layer_gaussian_noise(stddev = 0.001, input_shape = dim(x_train)[2],
                       name = "GN") %>%
  layer_batch_normalization() %>%
  layer_dense(units=100, activation = "elu", name = "dense1") %>%
  layer_dropout(rate = 0.5, name = "dp1") %>%
  layer_batch_normalization() %>%
  layer_dense(units=50, activation = "elu", name = "dense2") %>%
  layer_dropout(rate = 0.5, name = "dp2") %>%
  layer_batch_normalization() %>%
  layer_dense(units=10, activation = "elu", name = "dense3") %>%
  layer_dropout(rate = 0.2, name = "dp3") %>%
  layer_batch_normalization() %>%
  layer_dense(units=num_classes, activation = "softmax", name = "soft") %>%
  compile(
    loss = 'binary_crossentropy'
    optimizer = optimizer_rmsprop(lr = learning_rate, decay = 0.01),
    metrics = 'accuracy'
  )

#Training and Evaluation
#Fit model to data
nibelungDNN500 %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs= epochs,
  verbose = 0,
  view_metrics = TRUE,
  shuffle = TRUE,
  validation_split = 0.2,
  #we must validate the tuple(x_test, y_test),
  callbacks = list(early_stopping)) -> history

predict(nibelungDNN500, x_test) -> Ypr.test
Ypr.test %>% max.col() - 1 -> y_pr_test
evalq(res_mod_test <- Eval(Ytest1, y_pr_state), env)
#bias
#require(randomUniformForest)
import_fun(randomUniformForest, biasVarCov, BiasVar)
target = env$Ytest1
bias1 <- BiasVar(predictions = y_pr_test,
                 target = target,
                 regression = FALSE, idx=1:length(target))
#-------model-----test-1---------
predict(nibelungDNN500, x_test1) -> Ypr.test1
Ypr.test1 %>% max.col() -1 -> y_pr_test1
evalq(res_mod_test1 <- Eval(Ytest2, y_pr_test1), env)


































  





























