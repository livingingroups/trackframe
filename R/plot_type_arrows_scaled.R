scale_arrows <- function(coords = list(), pct = .5) {
  # arg checking not needed because this
  # is an internal and called from a function where inputs are checked
  # nolint start: object_usage_linter
  with(
    coords,
    {
      xdiff <- (xmax - xmin)
      ydiff <- (ymax - ymin)

      # Add .001in to each coord so that arrow will be long enough to have a defined direction.
      xeps <- diff(grconvertX(c(0, 1e-3), from = "inches", to = "user"))
      yeps <- diff(grconvertY(c(0, 1e-3), from = "inches", to = "user"))

      list(
        xmin = xmin,
        ymin = ymin,
        xmax = xmin + xdiff * pct + xeps * sign(xdiff),
        ymax = ymin + ydiff * pct + yeps * sign(ydiff)
      )
    }
  )
}
# nolint end

type_arrows_scaled <- function(arrowhead_loc, ...) {
  out <- type_arrows(...)
  out$data <- function(settings, ...) {
    settings$datapoints[,
      c(
        "xmin",
        "ymin",
        "xmax",
        "ymax"
      )
    ] <- do.call(
      data.frame,
      scale_arrows(
        as.list(settings$datapoints[, c("xmin", "ymin", "xmax", "ymax")]),
        arrowhead_loc
      )
    )
  }
  return(out)
}
