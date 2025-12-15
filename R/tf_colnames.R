
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
#' @param value new column naems. `tf_colnames` takes a
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

# FIXME: should we try making a names method that automatically updates the attributes?
# "names.trackframe<-" <- function(x, value) {
#   is_key_col <- names(x) %in% tf_colnames(x)
#   new_key_col_names <- value[is_key_col[seq_len(length(value))]]
#   names(new_key_col_names) <- names(tf_colnames(x))[tf_colnames(x) %in% names(x)]
#   tf_colnames(x) <- new_key_col_names
#   base::names(x) <- value
# }
