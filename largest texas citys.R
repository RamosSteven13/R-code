#==============
# LOAD PACKAGES
#==============

library(rvest)
library(tidyverse)
library(ggmap)
library(stringr)


html.population <- read_html('https://en.wikipedia.org/wiki/List_of_cities_in_Texas_by_population')


df.texas_cities <- html.population %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table(fill = TRUE)


# inspect
df.texas_cities %>% head()
df.texas_cities %>% names()

#============================
# REMOVE EXTRANEOUS VARIABLES
#============================

df.texas_cities <- df.texas_cities[,c(2,3)]
df.texas_cities$`Place name` <- as.character(df.texas_cities$`Place name`)
df.texas_cities$`Place name` <- str_replace(df.texas_cities$`Place name`, "\\[.*\\]","")


# inspect
df.texas_cities %>% names()
df.texas_cities %>% head()

texas <- rep("Texas",68)

df.texas_cities <- cbind(df.texas_cities,texas)
df.texas_cities %>% head()

colnames(df.texas_cities) <- c("city", "population", "state")


df.texas_cities <- df.texas_cities %>% mutate(city_full_name = str_c(df.texas_cities$city, df.texas_cities$state, sep = ', '))


#inspect
df.texas_cities %>% head()

df.texas_cities <- df.texas_cities %>% select(city, state, city_full_name, population)


df.texas_cities <- df.texas_cities %>% as_tibble()

df.texas_cities$population <- as.numeric(gsub(",", "", df.texas_cities$population))

#========================================================
# GEOCODE
# - here, we're just getting longitude and latitude data 
#   using ggmap::geocode()
#========================================================


data.geo <- geocode(df.texas_cities$city_full_name)

print(data.geo)
df.texas_cities <- cbind(df.texas_cities, data.geo)

write.csv(df.texas_cities, "D:/TexasCities.csv")

#map it
map.texas %>% ggmap()

map.texas <- get_map('Texas ', zoom = 6, 
                     source = "google", maptype = "hybrid")


# FIRST ITERATION
df.texas_cities$population <- as.numeric(df.texas_cities$population)
ggmap(map.texas) +
  geom_point(data = df.texas_cities, aes(x = lon, y = lat, size = as.numeric(population)), color = "red", alpha = .3) +
  geom_point(data = df.texas_cities, aes(x = lon, y = lat, size = as.numeric(population)), color = "red", shape = 1)



#OK Data work
#Getting OK cities populations and geocodes

#source
html.populationOK <- read_html('https://en.wikipedia.org/wiki/List_of_towns_and_cities_in_Oklahoma_by_population')

#data  management 
df.OK_cities <- html.populationOK %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table(fill = TRUE)
df.OK_cities<- df.OK_cities[1:10,1:4]
OK <- rep("Oklahoma",10)
df.OK_cities <- cbind(df.OK_cities,OK)
df.OK_cities %>% names()
colnames(df.OK_cities) <- c("rank","city", "population","cen", "state")
df.OK_cities <- df.OK_cities %>% mutate(city_full_name = str_c(df.OK_cities$city, df.OK_cities$state, sep = ', '))

#Getting Lats and Lons

data.geo.OK <- geocode(df.OK_cities$city_full_name)

print(data.geo.OK)
df.OK_cities <- cbind(df.OK_cities, data.geo.OK)
df.OK_cities$population <- as.numeric(gsub(",", "", df.OK_cities$population))
df.OK_cities<- df.OK_cities[,c(2,3,5,6,7,8)]
df.texas_cities <- rbind(df.texas_cities,df.OK_cities)







#==================================================
# FINALIZED MAP
# - here I've added titles, modified theme elements
#   like the text, etc
#==================================================

ggmap(map.texas) +
  geom_point(data = df.texas_cities, aes(x = lon, y = lat, size = population), color = "red", alpha = .1) +
  geom_point(data = df.texas_cities, aes(x = lon, y = lat, size = population), color = "red", shape = 1) +
  labs(x = NULL, y = NULL) +
  labs(size = 'Population (millions)') +
  labs(title = "Largest Cities in Texas", subtitle = "https://en.wikipedia.org/wiki/List_of_cities_in_Texas_by_population") +
  scale_size_continuous(range = c(.6,18), labels = scales::comma_format(), breaks = c(1500000, 750000, 100000)) +
  theme(text = element_text(color = "#4A4A4A", family = "Gill Sans")) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(plot.title = element_text(size = 32)) +
  theme(plot.subtitle = element_text(size = 10)) +
  theme(legend.key = element_rect(fill = "white"))
