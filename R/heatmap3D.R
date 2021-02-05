#' @title heatmap3D
#'
#' @description Add a three-dimensional histogram/heatmap
#'
#' @param z A two-dimensional matrix of the discretized density or frequency
#' of \code{x} and \code{y}.
#' @param x A numeric vector of length \code{nrow(z) + 1}.
#' @param y A numeric vector of length \code{ncol(z) + 1}.
#' @param nlines Suggested number of lines for the grid. May be overridden.
#' @param cols Color palette for the heatmap shadings.
#' @param cex.main The magnification to be used for main titles relative to the
#' current setting of cex as in \code{\link[graphics]{par}}.
#' @inheritParams graphics::persp
#' @inheritParams plot3D::perspbox
#' @inheritParams plot3D::hist3D
#' @inheritParams graphics::par
#'
#' @return Returns, as invisible, the viewing transformation matrix.
#'
#' @example man/examples/heatmap3D.R
#'
#' @export
#'
#' @seealso \code{\link{plot3D}}

heatmap3D <- function(z,
                      x,
                      y,
                      zlab = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      zlim = NULL,
                      xlim = NULL,
                      ylim = NULL,
                      nlines = 5,
                      cols = NULL,
                      border = NA,
                      main = NULL,
                      cex.main = 1,
                      nticks = NULL,
                      expand = 0.8,
                      theta = 45,
                      phi = 0,
                      r = sqrt(3),
                      d = 1.2,
                      add = FALSE) {

    ## Check inputs
  if (!(is.matrix(z))) {
    stop("z: Please supply a matrix for z.")
  }
  if (!(is.vector(x) &
        is.numeric(x) & length(x) == dim(z)[1] + 1)) {
    stop("x: Please supply a numeric vector of the length of the first
         dimension of z plus one.")
  }
  if (!(is.vector(y) &
        is.numeric(y) & length(y) == dim(z)[2] + 1)) {
    stop("y: Please supply a numeric vector of the length of the second
         dimension of z plus one.")
  }


  ## Extract meta information
  x <- x[-length(x)] + abs(x[1] - x[2]) / 2
  y <- y[-length(y)] + abs(y[1] - y[2]) / 2

  length.x <- length(x)
  length.y <- length(y)
  nlines <- nlines
  points.x <- x[round(seq(1, length(x), length.out = nlines))]
  points.y <- y[round(seq(1, length(y), length.out = nlines))]

  if (is.null(nticks)) {
    nticks <- nlines
  }
  if (is.null(zlim)) {
    zlim <- range(z)
  }
  if (is.null(xlim)) {
    xlim <- range(x)
  }
  if (is.null(ylim)) {
    ylim <- range(y)
  }
  if (any(is.null(cols))) {
    col <- plot3D::ramp.col(col = c("gray95", "gray20"),
                            n = 100,
                            alpha = .9)
  } else {
    col <- plot3D::ramp.col(col = cols, n = 100, alpha = .9)
  }
  if (is.na(border)) {
    border <- "white"
  }

  ## Initialize plot (if isFALSE(add))
  if (isFALSE(add)) {
    plot3D::perspbox(
      z = NULL,
      zlim = zlim,
      zlab = zlab,
      x = NULL,
      xlim = xlim,
      xlab = xlab,
      y = NULL,
      ylim = ylim,
      ylab = ylab,
      main = main,
      cex.main = cex.main,
      bty = 'u',
      col.grid = "white",
      col.panel = "gray95",
      col.axis = "gray80",
      lwd.grid = 2,
      lwd.panel = 2,
      ticktype = "detailed",
      nticks = nticks,
      expand = expand,
      theta = theta,
      phi = phi,
      r = r,
      d = d
    )
  }

  ## Draw histogram
  plot3D::hist3D (
    x = x,
    y = y,
    z = z,
    col = col,
    border = border,
    add = TRUE,
    plot = TRUE,
    colkey = FALSE,
    lwd = 2
  )
}
