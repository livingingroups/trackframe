
# #' Create a new Track Frame
# #'
# #' @param data A data.frame or tibble or data.table.
# #' @param index Column name of the index column
# #' @param easting Column name of the easting column
# #' @param northing Column name of the northing column
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
#' @param data a `data.frame` containing the tracking data.
#' @param time_col a character string specifying the column name of the time column.
#' @param easting_col A character string specifying the column name of the easting column.
#' @param northing_col A character string specifying the column name of the northing column.
#' @param id_col Optional character vector specifying identifier column names.
#' @param ... Additional arguments (unused).
#'
#' @return A `track_frame` object with appropriate attributes set.
#' @examples
#' df <- data.frame(
#'   time_col = as.POSIXct(Sys.time() + 1:5),
#'   easting_col = runif(5, 0, 10),
#'   northing_col = runif(5, 0, 10),
#'   id = 1:5
#' )
#' tf <- as.track_frame(df, time_col = "time_col", easting_col = "easting_col",
#'                         northing_col = "northing_col", id_col = "id")
#' attributes(tf)
#' @export
as.track_frame.data.frame <- function(data,
                                      time_col,
                                      easting_col,
                                      northing_col,
                                      id_col = NULL,
                                      # id_cols = NULL,
                                      ...) {
    assert_choice(time_col, colnames(data))
    assert_choice(easting_col, colnames(data))
    assert_choice(northing_col, colnames(data))
    assert_choice(id_col, colnames(data),  null.ok = TRUE)
    assert_character(id_col, len = 1, null.ok = TRUE)
    assert_numeric(data[[easting_col]])
    assert_numeric(data[[northing_col]])
    assert_posixct(data[[time_col]])
    attr(data, "time") <- time_col
    attr(data, "easting") <- easting_col
    attr(data, "northing") <- northing_col
    attr(data, "id") <- id_col
    class(data) <- union("track_frame", class(data))
    return(data)
}


# unique_id <- function(data) {
#   # FIXME: make id col unique
#   if(length(id_cols) > 1) {
#     id_col <- paste(id_cols, collapse = "__" )
#     data[[id_col]] <- apply(data[, id_cols] , 1 , paste , collapse = "__")
#   } else {
#     id_col <- id_cols
#   }
#   attr(data, "id") <- id_col
#   return(data)
# }

#' @export
as.track_frame.move2 <- function(data, ...) {
    data_attr <- attributes(data)
    x_y <- st_coordinates(data[[attr(data, "sf_column")]])
    index <- attr(data, "time_column")
    id_col <- attr(data, "track_id_column") #move2: The `track_id_column` attribute should be a <character> of length 1
    cols <- setdiff(colnames(data), attr(data, "sf_column"))
    class(data) <- "list"
    data <- data[cols]
    #FIXME: transformations to easting/northing
    data[["easting"]] <- x_y[, 1]
    data[["northing"]] <- x_y[, 2]
    class(data) <- c("tbl_df", "tbl", "data.frame")
    attr(data, "row.names") <- data_attr[["row.names"]]
    as.track_frame(data, time_col = index, easting_col = "easting",
                   northing_col = "northing", id_col = id_col)
}
