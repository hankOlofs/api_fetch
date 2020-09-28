# install.packages("httr")
library(httr)

# install.packages("jsonlite")
library(jsonlite)

# install.packages("XML")
library(XML)

# install.packages("geojsonio")
library(geojsonio)

#install.packages("geojsonR")
library(geojsonR)

# install.packages("RCurl")
library(RCurl)

library(sp)

#https://ropensci.org/blog/2013/10/23/style-geojson-polygon/

date <- "1990-09-28"
url <- paste0("http://api.thenmap.net/v2/world-2/geo/", date)
download.file(url = url, "test.geojson", replace = TRUE)

# file_fetch <- getURL(url)
# as_json <- toJSON(file_fetch)
# 
# map_data2 <- geojson_list(as_json)
# map_data <- fromJSON(url)

# map_data3 <- geojson_json(map_data, type = "FeatureCollection")

map_data <- geojson_read("test.geojson", what = "sp")

plot(map_data)
