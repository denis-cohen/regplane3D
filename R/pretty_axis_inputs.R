#' @title determine_axis_inputs
#'
#' @description Determine axis inputs for \code{regplane3D} plotting functions.
#' The function returns a rounded range for the axis (\code{range}),
#' (possibly) an adjusted number of lines that ensure pretty display
#' (\code{nlines}), and a value sequence for predictions that includes the values
#' at which the \code{z}-values for each line will be calculated and
#' (\code{linevals}) and, if \code{multiply > 1}), a sequence of
#' equally spaced value gradations for smoother prediction curves (\code{seq}).
#'
#' @param axis_range Value range of the variable displayed on either the
#' \code{x}-axis or \code{y}-axis of a \code{regplane3D} plot.
#' @param base Numerical base to which \code{axis_range} will be rounded for
#' pretty display.
#' @param nlines_suggest Suggested number of lines for the grid display of the
#' regression plane. May be adjusted by the function to ensure pretty display.
#' @param multiply Define the number of intermediate steps between line values.
#'
#' @return Returns a list with four entries: \code{nlines}, \code{range},
#' \code{linevals}, and \code{seq}.
#'
#' @example man/examples/pretty_axis_inputs.R
#'
#' @export
#'
#' @seealso \code{\link[base]{pretty}}
#'
#' @references
#' \url{https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/regplane3D/}

pretty_axis_inputs <- function(axis_range,
                               base = 1,
                               nlines_suggest = 5L,
                               multiply = 3) {

  ## Determine range
  rounded_range <-
    c(base * floor(min(axis_range, na.rm = TRUE) / base),
      base * ceiling(max(axis_range, na.rm = TRUE) / base))

  ## Determine value sequence for ticks
  pretty_seq_ticks <- pretty(rounded_range, n = nlines_suggest)

  ## Determine value sequence for line drawing
  if (multiply != 1) {
    seq_spacing <-
      (pretty_seq_ticks[2] - pretty_seq_ticks[1]) / multiply
    pretty_seq_lines <-
      seq(rounded_range[1], rounded_range[2], seq_spacing)
  } else {
    pretty_seq_lines <- pretty_seq_ticks
  }

  ## Return
  return(list(
    range = rounded_range,
    seq = pretty_seq_lines,
    linevals = pretty_seq_ticks,
    nlines = length(pretty_seq_ticks)
  ))
}
