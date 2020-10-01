#' API fetch function
#'
#' @param date_str a date string expressed as "yyyy-mm-dd"
#' @param ... optional parameters
#'
#' @return Returns a list with the data as well as a note if there was no data the specified date.
#' @export
#'
#' @examples
#' fetch("2000-01-01")
fetch <-
function(date_str = "2000-01-01", ...) {
  date <- as.Date(date_str)
  notes <- "" # pass a note if the date is missing
  
  # function to fetch data
  fetch_data <- function(date) {
    url <- paste0("http://api.thenmap.net/v2/world-2/geo/", date)
    fetched_data <- geojsonio::geojson_sf(url)
    return(fetched_data)
  }
  
  # actually fetching the data
  fetched_data <- fetch_data(date)
  
  # a nearly empty string should trigger a search for the next available date
  cond <- function(fd) {
    utils::object.size(fd) < 100
  }
  
  if (cond(fetched_data)) {
    while (cond(fetched_data)) {
      date <-  date + 1
      fetched_data <- fetch_data(date)
      # add something to make the loop always end
    }
    notes <-
      paste(
        "The data was not available for the selected date. Data fetched for next available date instead:",
        date
      )
  }
  
  # returning a list with the current geojson file and a note
  # might make map_geojson a list of every fetched piece of data to preserve and speed up old queries
  # (although that will start using too much space - perhaps the last 20 or something)
  return(list("data" = fetched_data, "notes" = notes))
}
