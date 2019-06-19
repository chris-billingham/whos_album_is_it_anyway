library(tidyverse)

# first let's scrape wikipedia for the list of Blur Studio albums
library(rvest)
library(janitor)
url <- "https://en.wikipedia.org/wiki/Blur_discography#Studio_albums"

# a fair amount of jiggery pokery here to get rid of stuff I don't need.
blur_albums <- read_html(url) %>% 
  html_nodes("table.wikitable.plainrowheaders") %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  clean_names() %>%
  slice(2:(n()-1)) %>%
  .[,1]

# however we want the b-sides. luckily blur released special editions with all the b-sides 
# for the relevant album, however Magic Whip had neither a special edition nor b-sides
# so we're going to whip that one out (arf arf)
blur_albums <- blur_albums[-length(blur_albums)]
blur_special_albums <- paste0(blur_albums, " [Special Edition]")

library(genius)

# a little function to download the album lyrics but append the album name
# note i regexp out the Special Editions, but thats for a little trick later
lyric_plus_album <- function(album_name) {
  df <- genius_album(artist = "Blur", album = album_name) %>%
    mutate(album = str_remove_all(album_name, " \\[Special Edition\\]$"))
  return(df)
}

# get all the straight albums and their lyrics
albums <- map_dfr(blur_albums, lyric_plus_album)

# get all the special edition albums and their lyrics,
# then remove the "standard" tracks with a cunning anti-join
bsides <- map_dfr(blur_special_albums, lyric_plus_album) %>%
  anti_join(albums)

# right we're good to go
# save it off so we don't have to again
saveRDS(albums, "data/albums.rds")
saveRDS(bsides, "data/bsides.rds")
