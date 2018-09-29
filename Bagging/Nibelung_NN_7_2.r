#prepare data
library(keras)
num_classes <- 2L
batch_size <- 32L
epochs <- 100L
#-----------
bestNN <- env$resBest$bestNN
x_train <- env$res$InputTrain[ ,bestNN]
y_train <- env$Ytest %>% to_categorical()
x_test <- env$res$InputTest [, bestNN]
#repeat for x_test1, x_testn

#model definition
nibelungNN72 <- keras_model_sequential()
#add layers and compile
nibelungNN72 %>%
  layer_dense(units=num_classes, input_shape = dim(x_train)[2]) %>%
  layer_activation(activation= 'softmax') %>%
  compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = 'accuracy'
  )
#training and Evaluation
nibelungNN72 %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 0,
  view_metrics = TRUE,
  shuffle = TRUE,
  #split the std
  validation_split = 0.2) -> history

#output metrics
#model---test-------
predict(nibelungNN72, x_test) -> Ypr.test
Ypr.test %>% max.col() - 1 -> y_pr_test
evalq(res_mod_test <- Eval(Ytest1, y_pr_test), env)
#bias test
#require(randomUniformForest)
import_fun(randomUniformForest, biasVarCov, BiasVar)
target = env$Ytest1
bias1 <- BiasVar(predictions = y_pr_test,
                 target = target,
                 regression = FALSE, idx=1:length(target))


#variant earlystopping
#prepare-data
library(reticulate)
library(keras)
py_set_seed(12345)

num_classes <- 2L
batch_size <- 32L
learning_rate <- 0.0005
epochs <- 100L

#---------------------------------
early_stopping <- callback_early_stopping(monitor = "val_acc",
                                          min_delta = 1e-5,
                                          patience= 20, verbose=0,
                                          mode = "auto")
log_dir <- paste0(getwd(), "/run_1")
tensboard <- callback_tensorboard(log_dir = log_dir,
                                  histogram_freq = 1,
                                  batch_size = 32, write_graph = TRUE,
                                  write_grads = TRUE, write_images = FALSE)

#model-keras
#definition
nibelungNN72 <- keras_model_sequential()
#add layers and compile
nibelungNN72 %>%
  layer_gaussian_noise(stddev = 0.05, input_shape = dim(x_train)[2], name="GN") %>%
  layer_dense(units = num_classes, name="dense1") %>%
  layer_activation_softmax(name="soft") %>%
  layer_activity_regularization(l2=1.0, name="reg") %>% #l1=0.01,
  compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_rmsprop(lr = learning_rate, decay = 0.01),
    metrics = 'accuracy'
  )

#fit to data
nibelungNN72 %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs, 
  verbose =0, 
  view_metrics = TRUE,
  shuffle= TRUE,
  validation_split = 0.2,
  callbacks = list(early_stopping, tensboard) ->history
)






































  