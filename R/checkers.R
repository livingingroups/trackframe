# checkmate style checks and expectations

# nolint start: object_name_linter

convertCamelCase <- function(x) {
  tolower(gsub(
    "((?<=[a-z0-9])[A-Z]|(?!^)[A-Z](?=[a-z]))",
    "_\\1",
    x,
    perl = TRUE
  ))
}

#' Checkmate style check if x is a trackframe object
#' @templateVar fn trackframe
#' @param x Object to check
#' @param ... arguments passed to checkmate::check_data_frame
#' @param unsorted.ok Is a trackframe not in order([id], timestamp) allowed? Default: TRUE
#' @template checker
#' @examples
#' check_trackframe(tf_mini)
#' expect_trackframe(tf_mini)
#' assert_trackframe(tf_mini)
#' @export
#' @rdname check_trackframe
check_trackframe <- checkTrackframe <- function(x, ..., unsorted.ok = TRUE) {
  if (!is.trackframe(x)) {
    return(
      sprintf("%s is not a trackframe. It has classes: %s", vname(x), class(x))
    )
  }
  if (!(df_check <- checkmate::check_data_frame(x, ...))) {
    return(df_check)
  }

  if (!unsorted.ok) {
    # not using accessors here to avoid circular dependency
    track_id <- attr(x, "id")
    len <- nrow(x)
    return(all(
      order(x[[track_id]], x[[attr(x, 'time')]]) == seq_len(len)
    ))
  }
  return(TRUE)
}

# edited version of output of checkmate::makeXFunction
# editing to workaround mllg/checkmate#281

#' @export
#' @template assert
#' @rdname check_trackframe
assert_trackframe <- assertTrackframe <- function(
  x,
  ...,
  unsorted.ok = TRUE,
  .var.name = checkmate::vname(x),
  add = NULL
) {
  if (missing(x)) {
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  }
  res <- check_trackframe(x, ..., unsorted.ok = unsorted.ok)
  checkmate::makeAssertion(x, res, .var.name, add)
}

#' @export
#' @template expect
#' @rdname check_trackframe
expect_trackframe <- expectTrackframe <- function(
  x,
  ...,
  unsorted.ok = TRUE,
  info = NULL,
  label = vname(x)
) {
  if (missing(x)) {
    stop(sprintf("Argument '%s' is missing", label))
  }
  res <- check_trackframe(x, ..., unsorted.ok = unsorted.ok)
  makeExpectation(x, res, info, label)
}

test_trackframe <- testTrackframe <- function(x, ..., unsorted.ok = TRUE) {
  isTRUE(check_trackframe(x, ..., unsorted.ok = unsorted.ok))
}

# nolint end: object_name_linter
