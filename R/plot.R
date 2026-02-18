#' Set cols of facet
#'
#' Calculates the desired number of columns for trackframe plots using tinyplot engine.
#'
#' @param n an integer value (corresponding to the number of different ids)
#' @return numeric
#'
#' @examples
#' set_facet_ncol(12)
#'
#' @export
set_facet_ncol <- function(n) {
  assert_integerish(n)
  if (n < 4) {
    return(as.integer(n))
  }
  if (n == 4) {
    return(2L)
  }
  if (n %% 4 == 0) {
    return(4L)
  }
  if (n %% 3 == 0) {
    return(3L)
  }
  return(2L)
}


#' Plot trackframes
#'
#' Plots coordinates of objects of class \code{\link[trackframe]{trackframe}} based on
#' \code{\link[tinyplot]{tinyplot}} functionality.
#'
#' @param x an object of class \code{trackframe}
#' @param direction logical indicator if the path direction should be added to the plot
#' @param direction_style a list of length, code, col, lty, lwd of the arrow of the direction
#' (argument passed to \code{\link[graphics]{arrows}}) specifying the style of the arrows
#' @param facet logical if facets should be used (TRUE is default). If FALSE all lines are plotted
#' in a single plot.
#' @param nfacet_col number of columns used in facet.args argument ncol
#' @param start_point logical if starting point of each track should be plotted
#' @param start_point_style a list where col, pch and cex for starting points are specified
#' @param end_point logical if end point of each track should be plotted
#' @param end_point_style a list where col, pch and cex for end points are specified
#' @param marker (optional) column name of additional markers to be ploted (depending on a 0/1
#'   labeling)
#' @param marker_style a list where col, pch and cex for markers are specified
#' @param ... other arguments used in \code{\link[tinyplot]{tinyplot}}
#'
#' @examples
#' library(trackframe)
#'
#' data("tf_mini", package = "trackframe")
#'
#' data <- tf_mini
#' class(data)
#'
#' plot(data)
#' # set different theme
#' library(tinyplot)
#' tinytheme("clean2")
#' plot(data)
#'
#' plot(data, direction = TRUE)
#'
#' # allow free y axis
#' plot(data, facet.args = list("free" = TRUE))
#'
#' # without facets
#' plot(data, facet = FALSE)
#'
#' track_1 <- select_id(data, "track_1")
#' plot(track_1)
#' plot(track_1, direction = TRUE)
#'
#' plot(track_1, start_point = TRUE, start_point_style = list(col = "blue"), end_point = TRUE)
#' @return No return value, called for side effect of producing a plot.
#'
#' @export
plot.trackframe <- function(
  x,
  direction = FALSE,
  direction_style = list(
    length = 0.1,
    code = 2,
    col = "black",
    lty = 3,
    lwd = 1
  ),
  facet = TRUE,
  nfacet_col = NULL,
  start_point = FALSE,
  start_point_style = list(col = "green", pch = 0, cex = 1),
  end_point = FALSE,
  end_point_style = list(col = "red", pch = 1, cex = 1),
  marker = NULL,
  marker_style = list(col = "blue", pch = 4, cex = 1),
  ...
) {
  assert_class(x, "trackframe")
  assert_logical(direction)
  assert_list(direction_style)
  assert_logical(facet)
  assert_integerish(nfacet_col, null.ok = TRUE)
  assert_logical(start_point)
  assert_list(start_point_style)
  assert_logical(end_point)
  assert_list(end_point_style)
  assert_character(marker, null.ok = TRUE)
  assert_list(marker_style)
  # sort data by id and time
  x <- sort(x)

  x_col <- easting_col(x)
  y_col <- northing_col(x)
  i_col <- id_col(x)

  if (is.null(i_col)) {
    i_col <- "id_int"
    x[[i_col]] <- "id_1"
    as.trackframe(x, id_col = i_col)
  }

  n_id <- length(unique(id(x)))
  nfacet_col <- nfacet_col %||% set_facet_ncol(n_id)

  if (n_id > 1) {
    form <- as.formula(paste(y_col, "~", x_col, "|", i_col))
    default_options <- list(
      type = "l",
      grid = TRUE,
      main = "Paths"
    )
    if (facet) {
      default_options <- c(
        default_options,
        facet = "by",
        facet.args = list("free" = FALSE, ncol = nfacet_col)
      )
    }
  } else {
    form <- as.formula(paste(y_col, "~", x_col))
    default_options <- list(type = "l", grid = TRUE, main = "")
  }

  # delete restricted elements
  restricted <- c("y", "data")
  args <- list(...)
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted as formula and data are extracted from the trackframe itself and
      is therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  do.call(tinyplot, c(list(form, data = x), control))

  # add starting point
  if (isTRUE(start_point)) {
    start_point_style_defaults <- list(col = "green", pch = 0, cex = 1)
    start_point_style <- modifyList(
      start_point_style_defaults,
      start_point_style
    )
    tinyplot_add(
      data = x[!duplicated(x[[i_col]]), ],
      type = "p",
      cex = start_point_style[["cex"]],
      pch = start_point_style[["pch"]],
      col = start_point_style[["col"]]
    )
  }
  # add end point
  if (isTRUE(end_point)) {
    end_point_style_defaults <- list(col = "red", pch = 1, cex = 1)
    end_point_style <- modifyList(end_point_style_defaults, end_point_style)
    tinyplot_add(
      data = x[!duplicated(x[[i_col]], fromLast = TRUE), ],
      type = "p",
      cex = end_point_style[["cex"]],
      pch = end_point_style[["pch"]],
      col = end_point_style[["col"]]
    )
  }

  # add change points
  if (!is.null(marker)) {
    if (any(x[[marker]] != 0)) {
      marker_style_defaults <- list(col = "blue", pch = 4, cex = 1)
      marker_style <- modifyList(marker_style_defaults, marker_style)
      tinyplot_add(
        data = x[x[[marker]] != 0, ],
        type = "p",
        cex = marker_style[["cex"]],
        pch = marker_style[["pch"]],
        col = marker_style[["col"]]
      )
    }
  }

  if (isTRUE(direction)) {
    # add arrow in path direction from (x1, y1) to (x2, y2)
    starting_points <- get_starting_points(x)
    direction_points <- get_direction_points(x)
    if (NROW(starting_points) != NROW(direction_points)) {
      stop("direction points do not exist for all IDs. Set direction = FALSE.")
    }
    # needed to match ids to ensure correct ordering in id's
    uids <- unique(id(x))
    direction_style_defaults <- list(
      length = 0.1,
      code = 2,
      col = "black",
      lty = 3,
      lwd = 1
    )
    direction_style <- modifyList(direction_style_defaults, direction_style)
    tinyplot(
      xmin = easting(starting_points)[match(uids, id(starting_points))],
      ymin = northing(starting_points)[match(uids, id(starting_points))],
      xmax = easting(direction_points)[match(uids, id(direction_points))],
      ymax = northing(direction_points)[match(uids, id(direction_points))],
      type = type_arrows(
        length = direction_style[["length"]],
        code = direction_style[["code"]]
      ),
      col = direction_style[["col"]],
      lty = direction_style[["lty"]],
      lwd = direction_style[["lwd"]],
      facet = if (facet) {
        id(starting_points)[match(uids, id(starting_points))]
      } else {
        "id"
      },
      facet.args = control$facet.args,
      add = TRUE
    )
  }
}


#' Add arrows to a plot
#'
#' This function adds arrows to the current (tinyplot) plot. Defines a type for tinyplots.
#'
#' @param ... arguments passed to [graphics::arrows]
#' for example `length`, `angle`, `code`
#' length in \code{\link[graphics]{arrows}}
#' @return a tinyplot_type containing corresponding draw function
#'
#' @examples
#' library(trackframe)
#' library(tinyplot)
#'
#' df_mini
#' tinyplot(x = df_mini$easting, y = df_mini$northing, type = "l")
#' tinyplot_add(
#'   xmin = df_mini$easting[1],
#'   ymin = df_mini$northing[1],
#'   xmax = df_mini$easting[2],
#'   ymax = df_mini$northing[2],
#'   type = type_arrows(
#'     length = 0.5,
#'     code = 2
#'     ),
#'   col = "blue",
#'   lty = 2,
#'   lwd = 2
#' )
#'
#' @export
#' @export
type_arrows <- function(...) {
  arrow_par <- list(...)
  out <- list(
    draw = function(ixmin, iymin, ixmax, iymax, ilty, ilwd, icol, ...) {
      do.call(
        arrows,
        c(
          list(
            x0 = ixmin,
            y0 = iymin,
            x1 = ixmax,
            y1 = iymax,
            lty = ilty,
            lwd = ilwd,
            col = icol
          ),
          arrow_par
        )
      )
    },
    data = NULL,
    name = "segments"
  )
  class(out) <- "tinyplot_type"
  return(out)
}


#' Plot Time Path
#'
#' This functions plots x-coordinates as well as y-coordinates over time in separate plots.
#'
#' @param x an object of class trackframe
#' @param marker (optional) column name of additional markers to be plotted (depending on a 0/1
#'   labeling)
#' @param marker_style a list where col, pch and cex for markers are specified
#' @param nfacet_col number of columns used in facet.args argument ncol
#' @param mfrow number of rows used in par()
#' @param mar margins used in par()
#' @param ... other arguments passed to the tinyplot call
#' @return No return value, called for side effect of producing a plot.
#'
#' @examples
#' library(trackframe)
#' plot_coords_by_time(path_trackframe)
#'
#' path_trackframe$change_points <- 0
#' path_trackframe$change_points[c(5 , 112, 205, 400, 700)] <- 1
#' plot_coords_by_time(path_trackframe, marker = "change_points")
#'
#' @export
plot_coords_by_time <- function(
  x,
  marker = NULL,
  marker_style = list(col = "blue", pch = 4, cex = 1),
  nfacet_col = NULL,
  mfrow = c(2, 1),
  mar = c(2, 2, 2, 1),
  ...
) {
  assert_class(x, "trackframe")
  assert_character(marker, null.ok = TRUE)
  assert_list(marker_style)
  assert_integerish(nfacet_col, null.ok = TRUE)
  # sort data by id and time
  x <- sort(x)
  x_col <- easting_col(x)
  y_col <- northing_col(x)
  t_col <- time_col(x)
  i_col <- id_col(x)

  if (is.null(i_col)) {
    i_col <- "id_int"
    x[[i_col]] <- "id_1"
    as.trackframe(x, id_col = i_col)
  }

  n_id <- length(unique(id(x)))

  if (n_id > 1) {
    stop("Only implemented for a single ID.")
  }

  nfacet_col <- nfacet_col %||% set_facet_ncol(n_id)

  if (n_id > 1) {
    form_x <- as.formula(paste(x_col, "~", t_col, "|", i_col))
    form_y <- as.formula(paste(y_col, "~", t_col, "|", i_col))
    default_options <- list(
      facet = "by",
      type = "l",
      facet.args = list("free" = FALSE, ncol = nfacet_col),
      grid = TRUE,
      main = "Paths"
    )
  } else {
    form_x <- as.formula(paste(x_col, "~", t_col))
    form_y <- as.formula(paste(y_col, "~", t_col))
    default_options <- list(type = "l", grid = TRUE, main = unique(id(x)))
  }

  # delete restricted elements
  restricted <- c("x", "y", "data")
  args <- list(...)
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted and therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  par(mfrow = mfrow, mar = mar)
  do.call(tinyplot, c(list(form_x, data = x), control))
  # add change points
  if (!is.null(marker)) {
    if (any(x[[marker]] != 0)) {
      marker_style_defaults <- list(col = "blue", pch = 4, cex = 1)
      marker_style <- modifyList(marker_style_defaults, marker_style)
      tinyplot_add(
        data = x[x[[marker]] != 0, ],
        type = "p",
        cex = marker_style[["cex"]],
        pch = marker_style[["pch"]],
        col = marker_style[["col"]]
      )
    }
  }
  do.call(tinyplot, c(list(form_y, data = x), control))
  # add change points
  if (!is.null(marker)) {
    if (any(x[[marker]] != 0)) {
      tinyplot_add(
        data = x[x[[marker]] != 0, ],
        type = "p",
        cex = marker_style[["cex"]],
        pch = marker_style[["pch"]],
        col = marker_style[["col"]]
      )
    }
  }
  NULL
}
