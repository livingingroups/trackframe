library(units)
x <- set_units(1:3, "m")
x

c(x, NA) # does not work
x > 1 # does not work

c(x, set_units(NA, "m")) # works
x > set_units(1, "m") # works


library(trackframe)
df <- data.frame(
  time_col = as.POSIXct(Sys.time() + 1:5),
  easting_col = runif(5, 0, 10),
  northing_col = runif(5, 0, 10),
  id = 1:5
)

tf <- trackframe(data = df)

units(tf$easting_col) <- "m"
class(tf$easting_col)

easting(tf)
class(easting(tf))

library(units)
c(easting(tf), northing(tf)) #works
c(easting(tf), NA) # does not work
c(easting(tf), set_units(NA, "m")) #works
c(easting(tf), 1) # does not work
easting(tf) > 0.01 # does not work
