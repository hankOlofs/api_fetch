#' fetch
#' 
#' @description API fetch function
#'
#' @param date_str a date string expressed as "yyyy-mm-dd". if contains_date is set to FALSE, this should be left blank
#' @param url a url pointing to a geojson file
#' @param contains_date a boolean indicating whether or not to include the date_str at the end of the url
#'
#' @return Returns a list with the data as well as a note if there was no data the specified date.
#' @export 
#' 
#' @importFrom geojsonio geojson_sf
#'
#' @examples
#' fetch("2000-01-01")
#' 
# if no date is specified, use today's date
fetch <- function(date_str = as.character(Sys.Date()), url = "http://api.thenmap.net/v2/world-2/geo/", contains_date = TRUE) {
  stopifnot("Error, date_str must be a character string on the form yyyy-MM-dd." = is.character(date_str))
  stopifnot("Error, url must be a character string." = is.character(url))
  stopifnot("Error, contains_date must be boolean." = is.logical(contains_date))
  
  date <- as.Date(date_str)
  notes <- "" # pass a note if the date is missing for some reason
  
  # constructs a url to fetch data from 
  urler <- function(url, date, contains_date) {
    if (contains_date == TRUE) {
      url <- paste0(url, date)
    }
    return(url)
  }
  
  url <- urler(url = url, date = date, contains_date = contains_date)
  
  # function to fetch data and convert to a simple features object
  fetch_data <- function(url) {
    fetched_data <- geojsonio::geojson_sf(url)
    return(fetched_data)
  }
  
  # actually fetching the data
  fetched_data <- fetch_data(url)
  
  # a nearly empty object should trigger a search for the next available date
  cond <- function(fd) {
    utils::object.size(fd) < 100
  }
  
  if (cond(fetched_data)) {
    while (cond(fetched_data)) {
      date <-  date + 1
      fetched_data <- fetch_data(date)
      if(date - as.Date(date_str) > 100) {
        no_data <- 1
        break
      }
    }
    if (no_data == 1) {
      notes <- "Error, there does not seem to be any data to fetch."
    } else {
      notes <- paste(
        "The data was not available for the selected date. Data fetched for next available date instead:",
        date
      )
    }
  }
  
  if (notes == "") {
    notes <- "Nothing to note."
  }
  
  # returning a list with the current geojson file and a note
  # might make map_geojson a list of every fetched piece of data to preserve and speed up old queries
  # (although that will start using too much space - perhaps the last 20 or something)
  return(list("data" = fetched_data, "notes" = paste0("Data fetched from: ", url, ". ", notes)))
}
