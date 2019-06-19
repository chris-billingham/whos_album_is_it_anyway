# this needs chopping up into bits
library(tidyverse)
library(keras)
library(forcats)
library(caret)

# let's fix up the data so we can keras it
# first we're going to make a train and test set
albums <- readRDS("data/albums.rds")

# i want to concatenate every two lines together to get more data
# this gives me like a 5% improvement in val_accuracy
albums$new_line <- (albums$line + (albums$line %% 2))/2

# this does it
albums <- albums %>% 
  group_by(album, track_n, track_title, new_line) %>% 
  mutate(new_lyric = paste0(lyric, collapse = " ")) %>%
  ungroup() %>%
  select(-line, -lyric) %>%
  rename(lyric = new_lyric, line = new_line) %>%
  unique()

set.seed(1979)
split <- createDataPartition(albums$album, p = 0.9, list = FALSE)

# time to
text <- albums$lyric

# this is max unique tokens
max_features <- 1000

# begin tokenising
tokenizer <- text_tokenizer(num_words = max_features)
tokenizer %>% 
  fit_text_tokenizer(text)

# turn that into a sequence of numbers
text_seqs <- texts_to_sequences(tokenizer, text)

# Set parameters:
maxlen <- 100
batch_size <- 32
embedding_dims <- 50
filters <- 64
kernel_size <- 3
hidden_dims <- 50
epochs <- 15

# create the train set
padded <- text_seqs %>%
  pad_sequences(maxlen = maxlen)
train_data <- padded[split,]
test_data <- padded[-split,]

# turn the categorical into a number, python likes us to start at 0 and as_factor
# orders as it's found rather than alphabetical
class_num <- as.numeric(as_factor(albums$album)) - 1

# one-hot encode that
all_class <- to_categorical(class_num, num_classes = 7)
train_class <- all_class[split,]
test_class <- all_class[-split, ]

# set a sequential model up
model <- keras_model_sequential()

# set up the model, we have 7 classes for 7 albums and need softmax
model %>%
  layer_embedding(input_dim = max_features, output_dim = 256) %>% 
  layer_lstm(units = 64, dropout = 0.2, recurrent_dropout = 0.1) %>% 
  layer_dense(units = 7, activation = 'softmax')

# Try using different optimizers and different optimizer configs
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adamax(),
  metrics = c('accuracy')
)

# fit the model
# i probably need to set up the validation proprely...
model %>% fit(
  train_data, train_class,
  batch_size = batch_size,
  epochs = 15,
  validation_data = list(test_data, test_class)
)

# the stats man
scores <- model %>% evaluate(
  test_data, test_class,
  batch_size = batch_size
)

# https://github.com/rstudio/keras/blob/master/vignettes/examples/imdb_lstm.R
# https://shirinsplayground.netlify.com/2019/01/text_classification_keras_data_prep/
# https://www.kaggle.com/ngyptr/multi-class-classification-with-lstm
# https://towardsdatascience.com/how-to-create-a-sequential-model-in-keras-for-r-1437aaf778e2


# to do
# keras-token the b-sides
# predict those and check how accurate they are against the actual