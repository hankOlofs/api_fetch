# install.packages("httr")
library(httr)

# install.packages("jsonlite")
library(jsonlite)

# install.packages("XML")
library(XML)

# install.packages("geojsonio")
library(geojsonio)


date <- "1990-09-28"
url<-paste0("http://api.thenmap.net/v2/se-4/geo/", date)

map_data <- geojson_json(url, what = "list")
map_data <- geojson_read()