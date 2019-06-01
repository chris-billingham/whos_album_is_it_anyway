# this needs chopping up into bits
library(tidyverse)
library(keras)
library(forcats)

# let's fix up the data so we can keras it
text <- albums$lyric

# this is max unique tokens
max_features <- 1000

# begin tokenising
tokenizer <- text_tokenizer(num_words = max_features)
tokenizer %>% 
  fit_text_tokenizer(text)

# look it works
tokenizer$document_count

# it works again
tokenizer$word_index %>%
  head()

# turn that into a sequence of numbers
text_seqs <- texts_to_sequences(tokenizer, text)

# see what we did there
text_seqs %>% head()

# Set parameters:
maxlen <- 100
batch_size <- 32
embedding_dims <- 50
filters <- 64
kernel_size <- 3
hidden_dims <- 50
epochs <- 5

# create the train set
x_train <- text_seqs %>%
  pad_sequences(maxlen = maxlen)
dim(x_train)

# turn the categorical into a number, python likes us to start at 0 and as_factor
# orders as it's found rather than alphabetical
class_num <- as.numeric(as_factor(albums$album)) - 1

# one-hot encode that
x_class <- to_categorical(class_num, num_classes = 7)

# set a sequential model up
model <- keras_model_sequential()

# set up the model, we have 7 classes for 7 albums and need softmax
model %>%
  layer_embedding(input_dim = max_features, output_dim = 128) %>% 
  layer_lstm(units = 64, dropout = 0.2, recurrent_dropout = 0.2) %>% 
  layer_dense(units = 7, activation = 'softmax')

# Try using different optimizers and different optimizer configs
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# fit the model
# i probably need to set up the validation proprely...
model %>% fit(
  x_train, x_class,
  batch_size = batch_size,
  epochs = 15,
  validation_data = list(x_train, x_class)
)

# the stats man
scores <- model %>% evaluate(
  x_train, x_class,
  batch_size = batch_size
)

#   https://github.com/rstudio/keras/blob/master/vignettes/examples/imdb_lstm.R
# https://shirinsplayground.netlify.com/2019/01/text_classification_keras_data_prep/
# https://www.kaggle.com/ngyptr/multi-class-classification-with-lstm

# to do
# keras-token the b-sides
# predict those and check how accurate they are against the actual