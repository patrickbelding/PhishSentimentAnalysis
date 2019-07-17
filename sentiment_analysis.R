rm(list = ls())
library(rvest)
library(data.table)
library(sqldf)
library(stringr)
library(ngram)

all_songs <- data.frame()
for(i in letters){
html <- read_html(paste0('https://m.phish.net/songs.php?letter=',i))
nodes <- html_nodes(html_nodes(html, xpath = '/html/body/div/div[2]'),'a')
songs <- as.data.frame(html_text(nodes))
links <- as.data.frame(str_sub(html_attr(nodes, 'href'),16))
df <- cbind(songs, links)
all_songs <- rbind(all_songs, df)
}

colnames(all_songs) <- c("songs", "links")

songs_and_lyrics <- data.frame()
for(j in all_songs$links[1:30]){
  html2 <- read_html(paste0('https://m.phish.net/songs.php?song=',j))
  nodes2 <- html_nodes(html2, xpath = '/html/body/div/div[2]/div[2]')
  lyrics <- gsub('\\\r|\\\n',' ',html_text(nodes2))
  lyrics2 <- if(length(lyrics)==0){
    "No Lyrics Found"
  } else {lyrics}
  df2 <- cbind(as.data.frame(j), as.data.frame(lyrics2))
  df2 <- df2 %>% mutate(word_count = wordcount(as.character(lyrics2)))
  songs_and_lyrics <- rbind(songs_and_lyrics, df2)
}

cleaned_songs <- songs_and_lyrics[!grepl('No Lyrics Found',songs_and_lyrics$lyrics2),]

View(cleaned_songs)
              