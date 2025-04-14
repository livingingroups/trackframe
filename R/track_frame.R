
#' Create a new Track Frame
#' 
#' @param data A data.frame or tibble or data.table.
#' @param index Column name of the index column
#' @param lon_col Column name of the longitude column
#' @param lat_col Column name of the latitude column
#' @param alt_col Column name of the altitude column
#' @param id_col Column name of the id column
#' @return A track_frame object
#' @export
track_frame <- function(index,
                        lon,
                        lat,
                        id,
                        ...) {

    stop("TODO")
    cols <- c("id", "index", "lon", "lat", "alt")
    cols <- c(intersect(cols, colnames(data)), setdiff(colnames(data), cols))
    data <- data[, cols]
    
    data
}


#' Convert an object to a Track Frame
#' 
#' @param data the object to be converted.
#' @param ... additional arguments passed to the method.
#' @return A track_frame object
#' @export
as.track_frame <- function(data, ...) {
    UseMethod("as.track_frame")
}


#' @export
as.track_frame.data.frame <- function(data,
                                      index,
                                      lon_col,
                                      lat_col,
                                      id_cols = NULL,
                                      ...) {
    assert_choice(index, colnames(data))
    assert_choice(lon_col, colnames(data))
    assert_choice(lat_col, colnames(data))
    assert_character(id_cols, null.ok = TRUE)
    for (id_col in id_cols) {
        assert_choice(id_col, colnames(data), null.ok = TRUE)
    }
    # check_multi_class(data[[index]], )
    assert_numeric(data[[lon_col]])
    assert_numeric(data[[lat_col]])
    assert_posixct(data[[index]])
    attr(data, "index") <- index
    attr(data, "lon_col") <- lon_col
    attr(data, "lat_col") <- lat_col
    attr(data, "id_cols") <- id_cols
    class(data) <- union("track_frame", class(data))
    return(data)
}


#' @export
as.track_frame.move2 <- function(data, ...) {
    data_attr <- attributes(data)
    lon_lat <- st_coordinates(data[[attr(data, "sf_column")]])
    index <- attr(data, "indexumn")
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
