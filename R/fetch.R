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
# fetch inputs are date, url and a boolean to exclude date if fetching from a static source (i.e. no date parameter)
# if no date is specified, today's date is used
fetch <- function(date_str = as.character(Sys.Date()), url = "http://api.thenmap.net/v2/world-2/geo/", contains_date = TRUE) {
  
  # block of input checks
  # stopifnot("Error, date_str must be a character string on the form yyyy-MM-dd or blank." = is.character(date_str))
  stopifnot("Error, date_str must be a character string on the form yyyy-MM-dd or blank." = nchar(date_str) == 10)
  stopifnot("Error, check format and contents of date_str (yyyy-MM-dd)." = class(try(as.Date(date_str), silent = TRUE)) == "Date")
  stopifnot("Error, check format of date_str (yyyy-MM-dd)." = grepl("....-..-..", date_str, fixed = FALSE))
  stopifnot("Error, URL must be a character string." = is.character(url))
  stopifnot("The URL provided does not have the correct format" = grepl(".*tp.?://.*\\..*/.*", url, fixed = FALSE))
  stopifnot("Error, contains_date must be boolean." = is.logical(contains_date))
  
  # checking if user really wants to use a different API than intended
  if (!grepl("api.thenmap.net", url, fixed = TRUE)) {
    print(
      "You seem to have chosen a non-standard url. This API has not been tested for API's outside of thenmap.net's API."
    )
    c <- readline(prompt = "Continue (Y/N)?")
    if (all((c != "Y"), (c != "y"), (c != "yes"), (c != "Yes"))) {
      stop("Attempt to fetch data from non-standard API stopped.")
    } else {
      print(paste("All right, continuing with", url))
    }
  }
  
  # treating date as date format for later use
  date <- as.Date(date_str)
  
  # pass a note if the date is missing for some reason
  notes <- "" 
  
  # function to construct a url to fetch data from 
  urler <- function(url, date, contains_date) {
    if (contains_date == TRUE) {
      # making sure the url is constructed properly depending on if the url end with a slash
      if (substr(url, nchar(url), nchar(url)) == "/") {
        url <- paste0(url,date)
      } else {
        url <- paste0(url, "/", date)
      }
    }
    return(url)
  }
  
  # calling fo he url to be constructed
  url <- urler(url = url, date = date, contains_date = contains_date)
  
  # part of unit testing for correct url structure
  url_check <- c(
    #grepl(":http*://*.*/*", url, fixed = FALSE),
    grepl("\\s", url, fixed = FALSE),
    grepl(".*:http.*", url, fixed = FALSE)
  )

  stopifnot("The URL does not seem to be proper:" = !all(url_check))
  
  # function to fetch data and convert to a simple features object
  fetch_data <- function(url) {
    fetched_data <- geojsonio::geojson_sf(url)
    return(fetched_data)
  }
  
  # actually fetching the data
  fetched_data <- fetch_data(url)
  
  # a nearly empty fetched object should trigger a search for the next available date and add a note
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
  # might make data a list of every fetched piece of data to preserve and speed up old queries
  # (although that will start using too much space after a few queries - perhaps the last 20 or something)
  return(list("data" = fetched_data, "notes" = paste0("Data fetched from: ", url, ". ", notes)))
}
