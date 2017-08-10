library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    city <- x$results[[1]]$address_components[[2]]$long_name
  } else {
    return(NA)
  }
}


#Test with a single address
address <- geoCode("11209")
# address
# [1] "38.8976831"
# [2] "-77.0364972"
# [3] "APPROXIMATE"
# [4] "The White House, 1600 Pennsylvania Avenue Northwest, Washington, D.C., DC 20500, USA"

# Use plyr to getgeocoding for a vector
address <- c("The White House, Washington, DC","The Capitol, Washington, DC")
locations <- ldply(address, function(x) geoCode(x))
names(locations) <- c("lat","lon","location_type", "forAddress")