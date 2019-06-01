
library(tidyverse)

# first let's scrape wikipedia for the list of Blur Studio albums
library(rvest)
library(janitor)
url <- "https://en.wikipedia.org/wiki/Blur_discography#Studio_albums"

# a fair amount of jiggery pokery here to get rid of stuff I don't need.
blur_albums <- read_html(url) %>% 
  html_nodes("table.wikitable.plainrowheaders") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[[1]] %>%
  clean_names() %>%
  slice(2:(n()-1)) %>%
  .[,1] %>%
  tolower()
