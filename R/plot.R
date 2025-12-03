#' Obtain arrow points of starting points
#'
#' @param tf an object of class trackframe
#' @param sort a logical indicator
#'
#' @export
get_arrow_points <- function(tf, sort = TRUE) {
  x <- attr(tf, "easting")
  y <- attr(tf, "northing")
  id <- attr(tf, "id")
  if (isTRUE(sort)) {
    tf <- sort(tf)
  }
  tf <- tf[!duplicated(tf[, c(x, y, id)]), ]
  starting_points <- tf[!duplicated(tf[[id]]), ]
  tf2 <- tf[duplicated(tf[[id]]), ]
  direction_points <- tf2[!duplicated(tf2[[id]]), ]
  if (NROW(starting_points) != NROW(direction_points)) {
    stop("direction points do not exist for all IDs. Set direction = FALSE.")
  }
  list(
    x0 = starting_points[[x]],
    y0 = starting_points[[y]],
    x1 = direction_points[[x]],
    y1 = direction_points[[y]]
  )
}


#' Set cols of facet
#'
#' calculates the number of columns specified for plots using tinyplot engine.
#'
#' @param n an integer value (corresponding to the number of differents ids)
#'
#' @export
set_facet_ncol <- function(n) {
  if (n < 4) return(n)
  if (n == 4) return(2)
  if (n %% 4 == 0) return(4)
  if (n %% 3  == 0) return(3)
  return(2)
}


#' Evaluates list
#'
#' @param x  a list
#'
#' @export
eval_list <- function(x) {
  mode(x) <- "call"
  eval(x)
}

#' Add plots to tinyplot plots
#'
#' @param call tinyplot call
#' @param ... ...
#'
#' @export
plot_add <- function(call, ...) {
  args <- list(...)
  if ("data" %in% names(args)) {
    call$data <- args$data
  }
  new_call <- modifyList(call, args)
  new_call[["add"]] <- TRUE
  eval_list(new_call)
}


#' Plot trackframes
#'
#' Plots coordinates of objects of class \code{\link[trackframe]{trackframe}} based on
#' \code{\link[tinyplot]{tinyplot}} functionality.
#'
#' @param x an object of class \code{trackframe}
#' @param direction logical indicator if the path direction should be added to the plot
#' @param arrow_style a list of length, code, col, lty, lwd of the arrow of the direction (argument
#' passed to
#' \code{\link[graphics]{arrows}}) specifying the style of the arrows
#' @param nfacet_col number of columns used in facet.args argument ncol
#' @param start_point logical if starting point should be plotted
#' @param start_point_style a list where col, pch and cex for starting points are specified
#' @param end_point logical if starting point should be plotted
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
#' track_1 <- select_id(data, "track_1")
#' plot(track_1)
#' plot(track_1, direction = TRUE)
#'
#' plot(track_1, start_point = TRUE, start_point_style = list(col = "blue"), end_point = TRUE)
#'
#' @export
plot.trackframe <- function(
  x,
  direction = FALSE,
  arrow_style = list(length = 0.1, code = 2, col = "black", lty = 3, lwd = 1),
  nfacet_col = NULL,
  start_point = FALSE,
  start_point_style = list(col = "green", pch = 0, cex = 1),
  end_point = FALSE,
  end_point_style = list(col = "red", pch = 1, cex = 1),
  marker = NULL,
  marker_style = list(col = "blue", pch = 4, cex = 1),
  ...
) {
  # sort data by id and time
  x <- sort(x)

  x_col <- attr(x, "easting")
  y_col <- attr(x, "northing")
  id_col <- attr(x, "id")

  if (is.null(id_col)) {
    id_col <- "id_int"
    attr(x, "id") <- id_col
    x$id_int <- "id_1"
  }

  n_id <- length(unique(id(x)))
  nfacet_col <- nfacet_col %||% set_facet_ncol(n_id)

  if (n_id > 1) {
    form <- as.formula(paste(y_col, "~", x_col, "|", id_col))
    default_options <- list(
      facet = "by",
      type = "l",
      facet.args = list("free" = TRUE, ncol = nfacet_col),
      grid = TRUE,
      main = "Paths"
    )
    arrows_facet <- "by"
  } else {
    form <- as.formula(paste(y_col, "~", x_col))
    default_options <- list(type = "l", grid = TRUE, main = "")
    arrows_facet <- id_col
  }

  # delete restricted elements
  restricted <- c("y", "data")
  args <- list(...) # args = list()
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted as formula and data are extracted from the trackframe itself and
      is therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  plt_call <- c(list(tinyplot, form, data = x), control)
  eval_list(plt_call)

  # add starting point
  if (isTRUE(start_point)) {
    start_point_style_defaults <- list(col = "green", pch = 0, cex = 1)
    start_point_style <- modifyList(start_point_style_defaults, start_point_style)
    plot_add(plt_call, add = TRUE, data = x[!duplicated(x[[id_col]]), ], type = "p",
      cex = start_point_style[["cex"]], pch = start_point_style[["pch"]],
      col = start_point_style[["col"]])
  }
  # add end point
  if (isTRUE(end_point)) {
    end_point_style_defaults <- list(col = "red", pch = 1, cex = 1)
    end_point_style <- modifyList(end_point_style_defaults, end_point_style)
    plot_add(plt_call, add = TRUE, data = x[!duplicated(x[[id_col]], fromLast = TRUE), ],
      type = "p", cex = end_point_style[["cex"]], pch = end_point_style[["pch"]],
      col = end_point_style[["col"]])
  }

  # add change points
  if (!is.null(marker)) {
    if (any(x[[marker]] != 0)) {
      marker_style_defaults <- list(col = "blue", pch = 4, cex = 1)
      marker_style <- modifyList(marker_style_defaults, marker_style)
      plot_add(plt_call, add = TRUE, data = x[x[[marker]] != 0, ], type = "p",
        cex = marker_style[["cex"]], pch = marker_style[["pch"]], col = marker_style[["col"]])
    }
  }

  if (isTRUE(direction)) {
    # add arrow in path direction from (x1, y1) to (x2, y2)
    arrow_points <- get_arrow_points(x)
    arrow_style_defaults <- list(length = 0.1, code = 2, col = "black", lty = 3, lwd = 1)
    arrow_style <- modifyList(arrow_style_defaults, arrow_style)
    plot_add(
      plt_call,
      add = TRUE,
      type = type_arrows(
        x0 = arrow_points[["x0"]],
        y0 = arrow_points[["y0"]],
        x1 = arrow_points[["x1"]],
        y1 = arrow_points[["y1"]],
        length = arrow_style[["length"]],
        code = arrow_style[["code"]],
        arrow_col = arrow_style[["col"]],
        arrow_lty = arrow_style[["lty"]],
        arrow_lwd = arrow_style[["lwd"]]
      ),
      facet = arrows_facet
    )
  }
}


#' Add arrows to a plot
#'
#' This function adds an arrow to a current plot.
#'
#' @param x0 x0 in \code{\link[graphics]{arrows}}
#' @param y0 y0 in \code{\link[graphics]{arrows}}
#' @param x1 x1 in \code{\link[graphics]{arrows}}
#' @param y1 y1 in \code{\link[graphics]{arrows}}
#' @param length length in \code{\link[graphics]{arrows}}
#' @param angle angle in \code{\link[graphics]{arrows}}
#' @param code code in \code{\link[graphics]{arrows}}
#' @param arrow_col col in \code{\link[graphics]{arrows}}
#' @param arrow_lty lty in \code{\link[graphics]{arrows}}
#' @param arrow_lwd lwd in \code{\link[graphics]{arrows}}
#'
#' @export
type_arrows <- function(
  x0,
  y0,
  x1,
  y1,
  length = 0.25,
  angle = 30,
  code = 2,
  arrow_col = "black",
  arrow_lty = par("lty"),
  arrow_lwd = par("lwd")
) {
  # assert_numeric(x0)
  data_arrows <- function(datapoints, lwd, lty, col, ...) {
    if (nrow(datapoints) == 0) {
      msg <- "`type_hline() only works on existing plots with x and y data points."
      stop(msg, call. = FALSE)
    }
    ul_lwd <- length(unique(lwd))
    ul_lty <- length(unique(lty))
    ul_col <- length(unique(col))
    return(list(
      type_info = list(ul_lty = ul_lty, ul_lwd = ul_lwd, ul_col = ul_col)
    ))
  }

  draw_arrows <- function() {
    fun <- function(
      ifacet,
      iby,
      data_facet,
      icol,
      ilty,
      ilwd,
      ngrps,
      nfacets,
      by_continuous,
      facet_by,
      type_info,
      ...
    ) {
      grp_aes <- type_info[["ul_col"]] == 1 ||
        type_info[["ul_lty"]] == ngrps ||
        type_info[["ul_lwd"]] == ngrps
      if (length(x0) != 1) {
        if (!length(x0) %in% c(ngrps, nfacets, ngrps * nfacets)) {
          msg <- "Length of 'x0' must be 1, or equal to the number of facets or number of groups 
          (or product thereof)."
          stop(msg, call. = FALSE)
        }
        if (!facet_by && length(x0) == nfacets) {
          x0 <- x0[ifacet]
          y0 <- y0[ifacet]
          x1 <- x1[ifacet]
          y1 <- y1[ifacet]
          if (!grp_aes && type_info[["ul_col"]] != ngrps) {
            icol <- 1
          } else if (by_continuous) {
            icol <- 1
          }
        } else if (!by_continuous && length(x0) == ngrps * nfacets) {
          x0 <- x0[ifacet * ngrps - c(ngrps - iby)]
          y0 <- y0[ifacet * ngrps - c(ngrps - iby)]
          x1 <- x1[ifacet * ngrps - c(ngrps - iby)]
          y1 <- y1[ifacet * ngrps - c(ngrps - iby)]
        } else if (!by_continuous) {
          x0 <- x0[iby]
          y0 <- y0[iby]
          x1 <- x1[iby]
          y1 <- y1[iby]
        }
      } else if (!grp_aes) {
        icol <- 1
      }
      arrows(
        x0 = x0,
        y0 = y0,
        x1 = x1,
        y1 = y1,
        length = length,
        angle = angle,
        code = code,
        col = arrow_col,
        lty = arrow_lty,
        lwd = arrow_lwd
      )
    }
    return(fun)
  }
  out <- list(draw = draw_arrows(), data = data_arrows, name = "hline")
  class(out) <- "tinyplot_type"
  return(out)
}


# FIXME: implement for multiple IDs if desired

#' Plot Time Path
#'
#' @param x an object of class trackframe
#' @param change_point_id column id of change points if available
#' @param change_point_col color of change points
#' @param change_point_pch pch of change points
#' @param change_point_cex cex of change points
#' @param nfacet_col number of columns used in facet.args argument ncol
#' @param mfrow number of rows used in par()
#' @param mar margins used in par()
#' @param ... ...
#'
#' @export
plot_time_path <- function(
  x,
  change_point_id = NULL,
  change_point_col = "blue",
  change_point_pch = 4,
  change_point_cex = 1,
  nfacet_col = NULL,
  mfrow = c(2, 1),
  mar = c(2, 2, 2, 1),
  ...
) {
  # sort data by id and time
  if (is.null(attr(x, "id"))) {
    x <- x[order(time(x)), ]
  } else {
    x <- x[order(id(x), time(x)), ]
  }

  x_col <- attr(x, "easting")
  y_col <- attr(x, "northing")
  time_col <- attr(x, "time")
  id_col <- attr(x, "id")

  if (is.null(id_col)) {
    id_col <- "id_int"
    attr(x, "id") <- id_col
    x$id_int <- "id_1"
  }

  n_id <- length(unique(id(x)))

  if (n_id > 1) stop("Only implemented for a single ID.")

  nfacet_col <- nfacet_col %||% set_facet_ncol(n_id)

  if (n_id > 1) {
    form_x <- as.formula(paste(x_col, "~", time_col, "|", id_col))
    form_y <- as.formula(paste(y_col, "~", time_col, "|", id_col))
    default_options <- list(
      facet = "by",
      type = "l",
      facet.args = list("free" = TRUE, ncol = nfacet_col),
      grid = TRUE,
      main = "Paths"
    )
  } else {
    form_x <- as.formula(paste(x_col, "~", time_col))
    form_y <- as.formula(paste(y_col, "~", time_col))
    default_options <- list(type = "l", grid = TRUE, main = unique(id(x)))
  }

  # delete restricted elements
  restricted <- c("x", "y", "data")
  args <- list(...) # args = list()
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted and therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])
  par(mfrow = mfrow,
    mar = mar)
  plt_call_x <- c(list(tinyplot, form_x, data = x), control)
  eval_list(plt_call_x)
  # add change points
  if (!is.null(change_point_id)) {
    if (any(x[[change_point_id]] != 0)) {
      plot_add(plt_call_x, add = TRUE, data = x[x[[change_point_id]] != 0, ], type = "p",
        cex = change_point_cex, pch = change_point_pch, col = change_point_col)
      # NOTE: do we want to add cp numbers?
    }
  }

  plt_call_y <- c(list(tinyplot, form_y, data = x), control)
  eval_list(plt_call_y)
  # add change points
  if (!is.null(change_point_id)) {
    if (any(x[[change_point_id]] != 0)) {
      plot_add(plt_call_y, add = TRUE, data = x[x[[change_point_id]] != 0, ], type = "p",
        cex = change_point_cex, pch = change_point_pch, col = change_point_col)
      # NOTE: do we want to add cp numbers?
    }
  }
}
