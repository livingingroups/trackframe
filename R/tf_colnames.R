#' Get and set column names of Trackframe Key columns
#'
#' Key columns meaning: easting, northing, time, and (opt) id column
#' All 3-4 key column names can be accessed or set with `tf_colnames()`.
#' Each also has a dedicated functions to access or set the names of the key columns.
#' Setting column names in this way will rename the currently configured key column
#' in the trackframe. It does not change the values of the column.
#'
#' To identify a different (existing) column as a key column, use `as.trackframe`.
#' @seealso tf_coords, as.trackframe, tf_id, tf_time
#' @export
#' @param tf a trackframe
#' @return X_col returns a character object representing the column name.
#' tf_colnames returns a named character vector of length 4 indicating the
#' column names of each of the key columns.
#' @examples
#' tf_colnames(tf_mini)
#' @rdname tf_colnames
tf_colnames <- function(tf) {
  assert_trackframe(tf)
  unlist(attributes(tf)[key_cols])
}

#' @export
#' @examples
#' tf_colnames(tf_mini)["id"] <- "animal_id"
#' @param value new column names. `tf_colnames` takes a
#' named character vector with names `easting`, `northing`, `time`, and (opt) `id`.
#' `X_col()` functions take a single string
#' @rdname tf_colnames
"tf_colnames<-" <- function(tf, value) {
  assert_trackframe(tf)
  assert_character(value)
  assert_subset(names(value), key_cols)
  for (name in key_cols) {
    if (name %in% names(value)) {
      names(tf)[which(names(tf) == attr(tf, name))] <- value[[name]]
      attr(tf, name) <- value[[name]]
    }
  }
  tf
}



#' @export
#' @param value new column names
#' @examples
#' names(tf_mini)[4] <- "animal_id"
#' names(tf_mini) <- c("t", "n", "e", "i")
#' names(tf_mini)
#' tf_colnames(tf_mini)
#' @rdname names
`names<-.trackframe` <- function(x, value) {
  assert_trackframe(x)
  assert_character(value)
  assert_subset(names(value), key_cols)
  tf_colnames_order <- match(names(x), tf_colnames(x))
  is_key_col <- names(x) %in% tf_colnames(x)
  new_key_col_names <- value[is_key_col[seq_along(value)]]
  names(new_key_col_names) <- names(tf_colnames(x))[tf_colnames_order[!is.na(tf_colnames_order)]]

  # update tf_columns
  for (name in key_cols) {
    if (name %in% names(new_key_col_names)) {
      attr(x, name) <- new_key_col_names[[name]]
    }
  }

  # update value
  attr(x, "names") <- value
  x
}
