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
