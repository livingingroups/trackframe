# Calculates a reasonable number of columns for a facet plot based on number of individuals
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

# shortcut to pass args, list of args, and add a plot
# unlink tinyplot::tinyplot_add, this function
# does not inherit args from original plot command
add_plot <- function(..., more = list()) {
  do.call(
    tinyplot,
    c(
      list(...),
      more,
      list(add = TRUE)
    )
  )
}

qualitative_palette <- function(colors, ...) {
  function(n) {
    n_base_cols <- length(colors)
    cols <- if (n <= n_base_cols) {
      colors
    } else {
      n_inc <- (n %/% n_base_cols) + 1
      shuffle <- rep(seq_len(n_base_cols) - 1, lenght.out = n_inc) *
        n_inc +
        rep(seq_len(n_inc), each = n_base_cols)
      cols <- colorRampPalette(c(colors, colors[1]), ...)(
        n_inc * n_base_cols + 1
      )[
        shuffle
      ]
    }
    cols[seq_len(n)]
  }
}

apply_eas_theme_as_default <- function(theme_arg) {
  eas_theme <- list(
    theme = "minimal",
    family = "sans",
    grid = TRUE,
    palette.qualitative = qualitative_palette(
      c(
        "#7CCE7D",
        "#274F48",
        "#5ECDB9",
        "#3A8474",
        "#010101"
      )
    )(25),
    palette.sequential = c(
      "#7CCE7D",
      "#5ECDB9",
      "#3A8474",
      "#274F48",
      "#010101"
    )
  )
  # If the user has not passed in a theme arg
  if (is.null(theme_arg)) {
    # look for a theme set by tinyplot::tinytheme
    if (tpar("tinytheme") %||% "default" == "default") {
      # if no theme passed by arg or set by tinyplot::tinytheme
      # then use our eas defaults
      eas_theme
    } else {
      # if one exists, don't pass theme = .. to tinyplot() at all
      NULL
    }
    # If user set theme arg as a list with theme = "eas" included in the list
  } else if (is.list(theme_arg) && theme_arg[["theme"]] %||% "" == "eas") {
    # use eas_theme as a baseline, and modify it with user-provided list
    modifyList(eas_theme, theme_arg)
  } else {
    # otherwise, just pass through what the user provided
    theme_arg
  }
}

# Pulls the defaults of a given arg for the current function.
# Code pulled from base::match.args
defaults <- function(arg) {
  formal_args <- formals(sys.function(sys_p <- sys.parent()))
  arg <- eval(arg)
  eval(
    formal_args[[as.character(substitute(arg))]],
    envir = sys.frame(sys_p)
  )
}

#' Plot trackframes
#'
#' Plots coordinates of objects of class \code{\link[trackframe]{trackframe}} based on
#' \code{\link[tinyplot]{tinyplot}} functionality.
#'
#' @param x an object of class \code{trackframe}
#' @param lines logical should lines be plotted?
#' @param lines_style list of graphical parameters
#'  passed to [tinyplot::tinyplot()] when plotting lines
#' @param points logical should points be plotted?
#' @param points_style graphical parameters
#'  passed to [tinyplot::tinyplot()] when plotting points
#' @param start_indicator logical indicator if arrows indicating
#' start point of each track should be added
#' @param start_indicator_style list of graphical parameters for start indicator.
#'  See section 'x_indicator_style'.
#' @param end_indicator logical indicator if arrows indicating
#' endpoint of each track should be added
#' @param end_indicator_style list of graphical parameters for end indicator.
#'  See section 'x_indicator_style'.
#' @param marker character column name of logical column indicating which points to mark
#' @param marker_style list of graphical parameters passed to \code{\link[tinyplot]{tinyplot}}
#' as markers are being drawn
#' @param facet logical should the plot be facetted by track?
#' @param facet.args list of arguments controlling facet behavior.
#'  If `ncol` is unspecified, an attempt is made chose a visually pleasing value.
#'  See \code{\link[tinyplot]{tinyplot}}
#' @param theme character or list:
#'   1) `NULL` (default): Use currently set `tinyplot` theme if one is set.
#'     Otherwise use trackframe default.
#'   2) a string naming a built-in tinyplot theme `vignette("themes", package = "tinyplot")` or
#'   3) a list of graphical parameters defining a custom theme
#' @param asp numeric y/x aspect ratio
#' @param xlab a title for the x axis
#' @param ylab a title for the x axis
#'
#' @param ... additional graphical parameters passed to \code{\link[tinyplot]{tinyplot}}
#'
#' @details # x_style
#'
#' The arguments `lines_style`, `points_style`,
#' `start_indicator_style`, `end_indicator_style` and `marker_style` each take
#' list of argments passed to `tinyplot`to style the corresponding element.
#'
#' Possible arguments can come from:
#'
#' - \code{\link[tinyplot]{tinyplot}} (e.g. `facet.args`)
#' - \code{\link[graphics]{par}} (e.g. `fg`)
#'
#' Parameters in `...` can come from the same lists and are applied to
#' the entire plot.
#'
#' @details # x_indicator_style
#'
#' Start/end indicators are arrows that point from the start/endpoint of each track
#' toward the second/penultimate point.
#' In addition to standard arrow paramenters,
#' `start_indicator_style` and `end_indicator_style` can have a key: `arrowhead_loc`
#' which controls how far along the first/last segment the arrow extends.
#' `0` refering to putting the arrow having the head only.
#' and `1` referring to having the arrow extend to second/penultimate point.
#' .001 inch is added to the user-provided value to ensure arrow direction
#' can be determined.
#'
#' Note: Because end arrows go from the endpoint toward the penultimate point,
#' the [graphics::arrows()] default of `angle = 30, code = 2` will
#' result in arrows pointing against the direction of travel.
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
#' plot(data)
#'
#'
#' # with facets and no indicator arrows
#' plot(
#'   data,
#'   facet = TRUE,
#'   start_indicator = FALSE,
#'   end_indicator = FALSE
#' )
#'
#' # allow free y axis
#' plot(data, facet.args = list("free" = TRUE))
#'
#' track_1 <- select_id(data, "track_1")
#' plot(track_1)
#' plot(track_1, direction = TRUE)
#'
#' # Customize start/stop arrows
#' plot(track_1, start_indicator_style = list(col = "blue"))
#'
#' @return No return value, called for side effect of producing a plot.
#'
#' @export
plot.trackframe <- function(
  x,
  # TODO: Verify with Alie how this UI should be
  lines = TRUE,
  lines_style = list(alpha = .5),
  points = TRUE,
  points_style = list(pch = 19),
  start_indicator = TRUE,
  start_indicator_style = list(
    arrowhead_loc = .001,
    length = 0.1,
    lwd = 3
  ),
  end_indicator = TRUE,
  end_indicator_style = list(
    arrowhead_loc = .001,
    length = 0.1,
    code = 1,
    angle = 90,
    lwd = 3
  ),
  marker = NULL,
  marker_style = list(pch = 22, col = "black", cex = 2),
  facet = FALSE,
  facet.args = list(), # nolint: object_name_linter
  theme = NULL,
  xlab = sprintf("easting (%s)", easting_col(x)),
  ylab = sprintf("northing (%s)", northing_col(x)),
  asp = 1,
  ...
) {
  assert_class(x, "trackframe")
  assert_flag(facet)
  assert_flag(points)
  assert_list(points_style)
  assert_flag(lines)
  assert_list(lines_style)
  assert_flag(start_indicator)
  assert_list(start_indicator_style)
  assert_flag(end_indicator)
  assert_list(end_indicator_style)
  assert_choice(marker, colnames(x), null.ok = TRUE)
  assert_character(xlab, len = 1)
  assert_character(ylab, len = 1)
  if (!is.null(marker)) {
    assert_logical(x[[marker]])
  }
  assert_list(marker_style)
  assert_number(asp)
  # sort data by id and time
  x <- sort(x)

  x_col <- easting_col(x)
  y_col <- northing_col(x)
  i_col <- id_col(x)

  n_id <- length(unique(id(x)))
  nfacet_col <- facet.args[["ncol"]] %||% set_facet_ncol(n_id)

  if (n_id > 1) {
    form <- as.formula(sprintf("`%s` ~ `%s` | `%s`", y_col, x_col, i_col))
    default_options <- list()
    if (facet) {
      default_options <- c(
        default_options,
        list(
          facet = "by",
          facet.args = modifyList(list(ncol = nfacet_col), facet.args)
        )
      )
    }
  } else {
    form <- as.formula(sprintf("`%s` ~ `%s`", y_col, x_col))
    default_options <- list(main = "")
  }

  # set up args
  args <- list(asp = asp, xlab = xlab, ylab = ylab, ...)

  # delete restricted elements
  restricted <- c("y", "data")
  if (any(names(args) %in% restricted)) {
    warning(sprintf(
      "argument %s is restricted as formula and data are extracted from the trackframe itself and
      is therefore ignored",
      names(args)[names(args) %in% restricted]
    ))
  }
  control <- modifyList(default_options, args[!names(args) %in% restricted])

  # configure theme
  theme <- apply_eas_theme_as_default(theme)
  if (!is.null(theme)) {
    control[["theme"]] <- theme
  }
  if (!isTRUE(facet)) {
    # Workaround tinyplot#553
    control[["facet.args"]] <- NULL
  } else if (
    isTRUE((control[["facet.args"]] %||% list())[["free"]]) && !is.na(asp)
  ) {
    # Warning regarding tinyplot#555
    warning(paste(
      "facet.args[[\"free\"]] = TRUE is incompatible",
      "with fixed asp",
      "due to https://github.com/grantmcdermott/tinyplot/issues/555."
    ))
  }
  do.call(
    tinyplot,
    c(
      list(form, data = x, type = "b", empty = TRUE),
      control
    )
  )
  if (lines) {
    add_plot(
      form,
      data = x,
      type = "l",
      more = modifyList(
        control,
        modifyList(defaults("lines_style"), lines_style)
      )
    )
  }
  point_type <- ifelse(
    !duplicated(id(x)),
    "start",
    ifelse(
      !duplicated(id(x), fromLast = TRUE),
      "end",
      "mid"
    )
  )
  if (points) {
    # exclude start/end points if they will be plotted later
    which_points <- point_type %in%
      c(
        "mid",
        if (!start_indicator) "start" else NULL,
        if (!end_indicator) "end" else NULL
      )
    add_plot(
      form,
      data = x[which_points, ],
      type = "p",
      more = modifyList(
        control,
        modifyList(defaults("points_style"), points_style)
      )
    )
  }

  # add marker_point
  # for some reason, this fails if drawn after
  # arrows. Ideally shoud lbe on top of arrows
  if (!is.null(marker)) {
    if (any(x[[marker]] != 0)) {
      marker_style <- modifyList(
        defaults("marker_style"),
        marker_style
      )
      marker_style <- modifyList(control, marker_style)
      add_plot(
        form,
        data = x[x[[marker]] != 0, ],
        type = "p",
        more = marker_style
      )
    }
  }

  # add starting point
  indicator_configs <- list(
    "start" = list(
      enabled = start_indicator,
      style = start_indicator_style,
      default_style = defaults("start_indicator_style"),
      start = TRUE
    ),
    "end" = list(
      enabled = end_indicator,
      style = end_indicator_style,
      default_style = defaults("end_indicator_style"),
      start = FALSE
    )
  )
  for (config in indicator_configs) {
    if (isTRUE(config$enabled)) {
      # add arrow in path direction from (x1, y1) to (x2, y2)
      sep <- get_starting_ending_segments(x, starting_segments = config$start)
      starting_points <- sep[["startpoint"]]
      direction_points <- sep[["endpoint"]]
      style <- config$style
      style <- modifyList(
        config[["default_style"]],
        style
      )
      style <- modifyList(control, style)
      # TODO: add arg checking on pct
      arrow_coords <- list(
        xmin = easting(starting_points),
        ymin = northing(starting_points),
        xmax = easting(direction_points),
        ymax = northing(direction_points)
      )
      add_plot(
        type = type_arrows_scaled(
          length = style[["length"]] %||% .25,
          code = style[["code"]] %||% 2,
          angle = style[["angle"]] %||% 30,
          arrowhead_loc = style[["arrowhead_loc"]]
        ),
        by = id(starting_points),
        facet = if (facet) {
          id(starting_points)
        } else {
          "id"
        },
        # exclude params passed directly to type_arrows
        more = c(
          arrow_coords,
          style[
            !names(style) %in%
              c("length", "code", "arrowhead_loc", "facet")
          ]
        )
      )
    }
  }
}
