sf_to_utm_epsg <- function(sf_object) {
  sf_object_4326 <- st_transform(sf_object, 4326)
  
  lon <- st_coordinates(sf_object_4326)[,1]
  lat <- st_coordinates(sf_object_4326)[,2]
  
  zone_number <- (floor((lon + 180) / 6) %% 60) + 1
  
  # Special zones for Norway
  cond_32 <- lat >= 56.0 & lat < 64.0 & lon >= 3.0 & lon < 12.0
  zone_number[cond_32] <- 32
  
  # Special zones for Svalbard
  cond_lat <- lat >= 72.0 & lat < 84.0
  
  cond_31 <- cond_lat & lon >= 0.0 & lon <  9.0
  zone_number[cond_31] <- 31
  
  cond_33 <- cond_lat & lon >= 9.0 & lon < 21.0
  zone_number[cond_33] <- 33
  
  cond_35 <- cond_lat & lon >= 21.0 & lon < 33.0
  zone_number[cond_35] <- 35
  
  cond_37 <- cond_lat & lon >= 33.0 & lon < 42.0
  zone_number[cond_37] <- 37
  
  # EPSG code
  utm <- zone_number[!is.na(zone_number)]
  lat <- lat[!is.na(zone_number)]
  utm[lat > 0] <- utm[lat > 0] + 32600
  utm[lat <= 0] <- utm[lat <= 0] + 32700
  utm <- tail(names(sort(table(utm))), 1)
  
  return(as.integer(utm))
}