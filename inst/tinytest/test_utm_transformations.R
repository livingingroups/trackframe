# library(tinytest)
library(trackframe)
library(sf)

x <- 1:10
y <- 1:10
t <- 1:10

# move2
library(move2)
data_move2 <- mt_as_move2(data.frame(t=as.POSIXct(t), x = x, y = y, id = 1),
                          coords = c("x", "y"),
                          time_column = "t",
                          track_id_column = "id",
                          crs = 32631)
expect_equal(sf::st_crs(data_move2)$input, "EPSG:32631")
expect_equal(gsub("[^0-9.-]", "",sf::st_crs(data_move2)$input), "32631")
tf <- as.trackframe(data_move2)
expect_equal(st_coordinates(data_move2[[attr(data_move2, "sf_column")]]),
             as.matrix(tf[, c("easting", "northing")]),
             check.attributes = FALSE)

# sftrack
library(sftrack)
data_sftrack <- as_sftrack(data.frame(t=as.POSIXct(t), x = x, y = y, id = 1),
                           coords = c("x", "y"),
                           time = "t",
                           crs = 32632)
expect_equal(sf::st_crs(data_sftrack)$input, "EPSG:32632")
expect_equal(gsub("[^0-9.-]", "",sf::st_crs(data_sftrack)$input), "32632")
tf <- as.trackframe(data_sftrack)
expect_equal(st_coordinates(data_sftrack[[attr(data_sftrack, "sf_column")]]),
             as.matrix(tf[, c("easting", "northing")]),
             check.attributes = FALSE)


# move2
data_move2 <- mt_as_move2(data.frame(t=as.POSIXct(t), x = x, y = y, id = 1),
                          coords = c("x", "y"),
                          time_column = "t",
                          track_id_column = "id",
                          crs = 4326)
expect_equal(sf::st_crs(data_move2)$input, "EPSG:4326")
expect_equal(gsub("[^0-9.-]", "",sf::st_crs(data_move2)$input), "4326")
tf <- as.trackframe(data_move2)
expect_equal(attr(tf, "utm_epsg"), 32632)
# cat(deparse(easting(tf)))
expect_equal(easting(tf),
             c(-392989.214538343, -280405.627933054, -167964.360223136, -55732.1960808097,
               56225.071791751, 167842.208283379, 279054.111380756, 389795.39792216,
               5e+05, 609600.772514454),
             tolerance = 1e-3)
# cat(deparse(northing(tf)))
expect_equal(northing(tf),
             c(111623.606591084, 222731.066067836, 333428.71264557, 443822.141497229,
               554016.06262504, 664114.162066235, 774218.966374571, 884431.706491422,
               994852.17728468, 1105578.5891924),
             tolerance = 1e-3)

# sftrack
data_sftrack <- as_sftrack(data.frame(t=as.POSIXct(t), x = x, y = y, id = 1),
                           coords = c("x", "y"),
                           time = "t",
                           crs = 4326)
expect_equal(sf::st_crs(data_sftrack)$input, "EPSG:4326")
expect_equal(gsub("[^0-9.-]", "",sf::st_crs(data_sftrack)$input), "4326")
tf2 <- as.trackframe(data_sftrack)
expect_equal(tf2[, c("easting", "northing")],
             tf[, c("easting", "northing")])
