address <- as.data.frame(read.csv("C:/Users/manis/Desktop/Addresses.csv",header = TRUE))

geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

locations <- apply(address,1, function(x) geocodeAdddress(x))
locations<-t(locations)
View(locations)
write.csv(locations,file="geotags_addresses2.csv")
