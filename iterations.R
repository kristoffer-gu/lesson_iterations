library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)



# Today we are going to work with some more advanced topics in 
# terms of data handling and processing. We will play with an API
# from Vegvesenet. Vegvesenet has an API we can query for data
# on traffic volumes at many sensor stations in Norway. 
# 
# The API uses graphQL for requests. This is a relatively
# new language we might see more of in the future?
# 
# Let's define a function where we can submit queries to an external API. 
GQL <- function(query,
                ...,
                .token = NULL,
                .variables = NULL,
                .operationName = NULL,
                .url = url) {
  pbody <-
    list(query = query,
         variables = .variables,
         operationName = .operationName)
  if (is.null(.token)) {
    res <- POST(.url, body = pbody, encode = "json", ...)
  } else {
    auth_header <- paste("bearer", .token)
    res <-
      POST(
        .url,
        body = pbody,
        encode = "json",
        add_headers(Authorization = auth_header),
        ...
      )
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if (!is.null(res$errors)) {
    warning(toJSON(res$errors))
  }
  res$data
}

# The URL we will use is stored below: 
url <- "https://www.vegvesen.no/trafikkdata/api/"


# Let's figure out which sensor stations that are operable. 
# The query below extracts all the stations, with a date for 
# when the station was in operation as well as a long/latitude. 
qry <-
  '
{
    trafficRegistrationPoints {
        id
        name
        latestData {
            volumeByDay
        }
        location {
            coordinates {
                latLon {
                    lat
                    lon
                }
            }
        }
    }
}
'

# Allright - let's try submitting the query: 
stations <-GQL(qry) 


# We now have the a long list in memory - 11mb! - with just 
# a little information on each station. We can note that this 
# is a list, not a dataframe. For our purposes, it would be better if
# the list was instead a data frame, with one row pr. sensor station. 


# Note that the list only has one entry..   
length(stations)