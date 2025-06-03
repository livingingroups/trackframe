
# #' Create a new Track Frame
# #'
# #' @param data A data.frame or tibble or data.table.
# #' @param index Column name of the index column
# #' @param lon Column name of the longitude column
# #' @param lat Column name of the latitude column
# #' @param alt Column name of the altitude column
# #' @param id Column name of the id column
# #' @return A track_frame object
# #' @export
# track_frame <- function(data,
#                         index,
#                         lon,
#                         lat,
#                         alt,
#                         id,
#                         ...) {
# 
#     stop("TODO")
#     cols <- c("id", "index", "lon", "lat", "alt")
#     cols <- c(intersect(cols, colnames(data)), setdiff(colnames(data), cols))
#     data <- data[, cols]
# 
#     data
# }


#' Convert an object to a Track Frame
#' 
#' @param data the object to be converted.
#' @param ... additional arguments passed to the method.
#' @return A track_frame object
#' @export
as.track_frame <- function(data, ...) {
    UseMethod("as.track_frame")
}


#' Convert a Data Frame to a Track Frame Object
#'
#' This function converts a `data.frame` into a `track_frame` object,
#' ensuring required columns exist and have valid data types.
#'
#' @param data A `data.frame` containing the tracking data.
#' @param index A character string specifying the timestamp column name.
#' @param lon_col A character string specifying the longitude column name.
#' @param lat_col A character string specifying the latitude column name.
#' @param id_cols Optional character vector specifying identifier column names.
#' @param ... Additional arguments (unused).
#'
#' @return A `track_frame` object with appropriate attributes set.
#' @examples
#' df <- data.frame(
#'   time = as.POSIXct(Sys.time() + 1:5),
#'   lon = runif(5, -180, 180),
#'   lat = runif(5, -90, 90),
#'   id = 1:5
#' )
#' track <- as.track_frame(df, index = "time", lon_col = "lon", lat_col = "lat", id_cols = "id")
#' @export
as.track_frame.data.frame <- function(data,
                                      time_index_col,
                                      easting_col,
                                      northing_col,
                                      track_id_col = NULL,
                                      # id_cols = NULL,
                                      ...) {
    assert_choice(time_index_col, colnames(data))
    assert_choice(easting_col, colnames(data))
    assert_choice(northing_col, colnames(data))
    # assert_character(track_id, null.ok = TRUE)
    # for (id_col in id_cols) {
    #     assert_choice(id_col, colnames(data), null.ok = TRUE)
    # }
    # check_multi_class(data[[index]], )
    assert_numeric(data[[easting_col]])
    assert_numeric(data[[northing_col]])
    assert_posixct(data[[time_index_col]])
    attr(data, "time_index") <- time_index_col
    attr(data, "easting_col") <- easting_col
    attr(data, "northing_col") <- northing_col
    attr(data, "track_id") <- track_id_col
    class(data) <- union("track_frame", class(data))
    return(data)
}


#FIXME with time_index_col, time_index_col, ...
#' @export
as.track_frame.move2 <- function(data, ...) {
    data_attr <- attributes(data)
    lon_lat <- st_coordinates(data[[attr(data, "sf_column")]])
    index <- attr(data, "time_column")
    id_cols <- attr(data, "track_id_column")
    cols <- setdiff(colnames(data), attr(data, "sf_column"))
    class(data) <- "list"
    data <- data[cols]
    data[["lon"]] <- lon_lat[, 1]
    data[["lat"]] <- lon_lat[, 2]
    class(data) <- c("tbl_df", "tbl", "data.frame")
    attr(data, "row.names") <- data_attr[["row.names"]]
    as.track_frame(data, index = index, lon_col = "lon", lat_col = "lat", id_cols = id_cols)
}
