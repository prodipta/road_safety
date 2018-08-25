require(keras)

y_train <- train_df$event
y_test <- test_df$event
x_train <- subset(train_df,select = -c(event, case))
x_test <- subset(test_df,select = -c(event, case))


rm(train_df); rm(test_df)


num_cols <- NCOL(x_train)
num_classes <- NROW(event_types)
batch_size <- 32
epochs <- 5

y_train <- to_categorical(y_train-1,num_classes = num_classes)
y_test <- to_categorical(y_test-1,num_classes = num_classes)
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 128, input_shape = c(num_cols)) %>% 
  layer_activation(activation = 'relu') %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = num_classes) %>% 
  layer_activation(activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  verbose = 1,
  callbacks = callback_tensorboard("logs/data_set_1"),
  validation_split = 0.1
)

score <- model %>% evaluate(
  x_test, y_test,
  batch_size = batch_size,
  verbose = 1
)

print(paste('Test score:', score[[1]], 'Test accuracy', score[[2]]))

save_model_hdf5(model, 'data_set_1_model.hd5', overwrite = TRUE,
                include_optimizer = TRUE)

tensorboard("logs/data_set_1")
