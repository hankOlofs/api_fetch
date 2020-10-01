\name{fetch}
\alias{fetch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fetch(date_str = "2000-01-01", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date_str}{
%%     ~~Describe \code{date_str} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (date_str = "2000-01-01", ...) 
{
    date <- as.Date(date_str)
    notes <- ""
    fetch_data <- function(date) {
        url <- paste0("http://api.thenmap.net/v2/world-2/geo/", 
            date)
        fetched_data <- geojson_sf(url)
        return(fetched_data)
    }
    fetched_data <- fetch_data(date)
    cond <- function(fd) {
        object.size(fd) < 1000
    }
    if (cond(fetched_data)) {
        while (cond(fetched_data)) {
            date <- date + 1
            fetched_data <- fetch_data(date)
        }
        notes <- paste("The data was not available for the selected date. Data fetched for next available date instead:", 
            date)
    }
    return(list(data = fetched_data, notes = notes))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
