
library(rvest)
library(tidyverse)
library(stringr)
html.population <- read_html('https://en.wikipedia.org/wiki/List_of_Roman_emperors')

##The Princeipate

Julio <- html.population %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table(fill = TRUE)


# inspect
Julio %>% head()
Julio %>% names()


Flavian<- html.population %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(fill = TRUE)


# inspect
Flavian %>% head()
Flavian%>% names()


Nerva<- html.population %>%
  html_nodes("table") %>%
  .[[4]] %>%
  html_table(fill = TRUE)


# inspect
Nerva %>% head()
Nerva%>% names()


Severan<- html.population %>%
  html_nodes("table") %>%
  .[[5]] %>%
  html_table(fill = TRUE)


# inspect
Severan %>% head()
Severan%>% names()


Gordian <- html.population %>%
  html_nodes("table") %>%
  .[[6]] %>%
  html_table(fill = TRUE)


# inspect
Gordian  %>% head()
Gordian %>% names()

Princeipate <- rbind(Julio,Nerva,Flavian,Severan,Gordian)
write.csv(Princeipate, "Princeipate.csv")
# pattern is by finding a set of numbers in the start and capturing them

