#' @title plane3D
#'
#' @description Plot a three-dimensional regression prediction with confidence
#' intervals.
#'
#' @param z A two-dimensional matrix of point estimates or a three-dimensional
#' array of of point estimates and interval values.
#' @param x A vector of \code{x}-values, matching the first dimension of
#' \code{z}.
#' @param y A vector of \code{y}-values, matching the second dimension of
#' \code{z}.
#' @param zlim \code{z}-limits. These should be chosen to cover the range of
#' values of the surface: see \code{\link[graphics]{persp}}.
#' @param xlim \code{x}-limits. These should be chosen to cover the range of
#' values of the surface: see \code{\link[graphics]{persp}}.
#' @param ylim \code{y}-limits. These should be chosen to cover the range of
#' values of the surface: see \code{\link[graphics]{persp}}.
#' @param cis Plot confidence intervals? Defaults to \code{TRUE}.
#' @param xnlines Suggested number of lines for the grid display of the
#' regression plane along the \code{x}-dimension. See
#' \code{\link{pretty_axis_inputs}}.
#' @param ynlines Suggested number of lines for the grid display of the
#' regression plane along the \code{y}-dimension. See
#' \code{\link{pretty_axis_inputs}}.
#' @param heatmap A two-dimensional matrix indicating the joint frequency
#' distribution of \code{x} and \code{y}. Will displayed as a shaded histogram
#' below the plot.
#' @param heatmap_cols Override default shadings of \code{heatmap}.
#' @param heatmap_border Override default border color of \code{heatmap}.
#' @param plane0 Add a a horizontal red plane at \code{z == 0}.
#' Defaults to \code{FALSE}.
#' @param plane_aux Add an auxiliary reference place. Defaults to \code{FALSE}.
#' @param plane_aux_pos User-specified position of auxiliary reference plane if
#' \code{plane_aux == TRUE}.
#' @param plane_aux_col User-specified color of auxiliary reference plane if
#' \code{plane_aux == TRUE}.
#' @param cex.main The magnification to be used for main titles relative to the
#' current setting of cex as in \code{\link[graphics]{par}}.
#' @inheritParams graphics::persp
#' @inheritParams plot3D::perspbox
#' @inheritParams plot3D::image3D
#' @inheritParams plot3D::scatter3D
#' @inheritParams plot3D::persp3D
#' @inheritParams graphics::par
#'
#' @return Returns, as invisible, the viewing transformation matrix.
#'
#' @example man/examples/plane3D.R
#'
#' @export
#'
#' @seealso \code{\link{plot3D}}
#'
#' @references
#' \url{https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/regplane3D/}

plane3D <- function(z,
                    x,
                    y,
                    zlab = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    zlim = NULL,
                    xlim = NULL,
                    ylim = NULL,
                    xnlines = 5L,
                    ynlines = 5L,
                    cis = TRUE,
                    heatmap = NULL,
                    heatmap_cols = NULL,
                    heatmap_border = NA,
                    plane0 = FALSE,
                    plane_aux = FALSE,
                    plane_aux_pos = NULL,
                    plane_aux_col = NULL,
                    main = NULL,
                    cex.main = 1,
                    expand = 0.8,
                    theta = 45,
                    phi = 0,
                    r = sqrt(3),
                    d = 1.2) {
  ## Check inputs
  if (cis) {
    if (!(is.array(z) & length(dim(z)) == 3L & dim(z)[3] == 3L)) {
      stop(
        "z: Please supply a three-dimensional array where the third dimension
      provides (1) point estimate, (2) lower bound, and (3) upper bound."
      )
    }
  } else {
    if (!((is.array(z) & length(dim(z)) == 2L | is.matrix(z)))) {
      stop("z: Please supply a two-dimensional array or a matrix.")
    }
    z <- array(z, dim = c(dim(z), 1L))
  }
  if (!(is.vector(x) & is.numeric(x) & length(x) == dim(z)[1])) {
    stop("x: Please supply a numeric vector of equal length as the first
         dimension of z.")
  }
  if (!(is.vector(y) & is.numeric(y) & length(y) == dim(z)[2])) {
    stop("y: Please supply a numeric vector of equal length as the second
         dimension of z.")
  }
  if (!(is.null(heatmap)) & !(is.matrix(heatmap))) {
    stop("heatmap: Please supply a matrix.")
  }

  ## Extract meta information
  length.x <- length(x)
  length.y <- length(y)
  points.x <- x[round(seq(1, length(x), length.out = xnlines))]
  points.y <- y[round(seq(1, length(y), length.out = ynlines))]
  nticks <- max(xnlines, ynlines)

  if (is.null(zlim)) {
    zlim <- range(z)
  }
  if (!(is.null(heatmap))) {
    bottom.space <- abs(diff(zlim)) / 4
    zlim[1] <- zlim[1] - bottom.space
  }
  if (isTRUE(plane0) & prod(zlim) > 0) {
    if (sum(zlim) < 0) {
      zlim[2] <- 0
    } else {
      zlim[1] <- 0
    }
  }
  if (is.null(xlim)) {
    xlim <- range(x)
  }
  if (is.null(ylim)) {
    ylim <- range(y)
  }

  ## Initialize plot
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

  ## Bottom heatmap
  if (!(is.null(heatmap))) {
    length.heatmap_x <- dim(heatmap)[1] + 1
    length.heatmap_y <- dim(heatmap)[2] + 1
    heatmap_x <- seq(min(x), max(x), length.out = length.heatmap_x)
    heatmap_y <- seq(min(y), max(y), length.out = length.heatmap_y)
    heatmap3D(
      z = (heatmap / max(heatmap)) * bottom.space + zlim[1],
      x = heatmap_x,
      y = heatmap_y,
      cols = heatmap_cols,
      border = heatmap_border,
      add = TRUE
    )
  }


  ## Red 0-Plane
  if (isTRUE(plane0)) {
    p0.x <- seq(min(x), max(x), length.out = nticks)
    p0.y <- seq(min(y), max(y), length.out = nticks)
    plot3D::image3D(
      x = p0.x,
      y = p0.y,
      z = 0,
      col = grDevices::adjustcolor("rosybrown1", alpha.f = 0.1),
      border = grDevices::adjustcolor("white", alpha.f = 0.5),
      lwd = 2,
      add = T
    )
  }

  ## Horizontal auxiliary reference plane
  if (isTRUE(plane_aux)) {
    if (is.null(plane_aux_pos)) {
      stop("plane_aux_pos: Please supply a z-value for the vertical position.")
    }
    if (is.null(plane_aux_col)) {
      plane_aux_col <- grDevices::adjustcolor("rosybrown1", alpha.f = 0.1)
    } else {
      plane_aux_col <-
        grDevices::adjustcolor(plane_aux_col, alpha.f = 0.1)
    }
    p.aux.x <- seq(min(x), max(x), length.out = nticks)
    p.aux.y <- seq(min(y), max(y), length.out = nticks)
    plot3D::image3D(
      x = p.aux.x,
      y = p.aux.y,
      z = plane_aux_pos,
      col = plane_aux_col,
      border = grDevices::adjustcolor("white", alpha.f = 0.5),
      lwd = 2,
      add = T
    )
  }


  ## Draw lines along first dimension
  for (k in 1:ynlines) {
    xx <- rep(points.x[k], length.y)
    yy <- y
    zz <- z[which(x == points.x[k]), , 1]
    plot3D::scatter3D(
      z = zz,
      x = xx,
      y = yy,
      col = "gray40",
      lwd = 2,
      lty = 1,
      type = 'l',
      add = T
    )
    if (cis) {
      for (p in 2:3) {
        xx <- rep(min(x), length.y)
        zz <- z[1, , p]
        plot3D::scatter3D(
          z = zz,
          x = xx,
          y = yy,
          col = grDevices::adjustcolor("gray80", alpha.f = 0.04),
          lwd = 0.5,
          lty = 1,
          type = 'l',
          add = T
        )
        xx <- rep(max(x), length.y)
        zz <- z[length(x), , p]
        plot3D::scatter3D(
          z = zz,
          x = xx,
          y = yy,
          col = grDevices::adjustcolor("gray80", alpha.f = 0.04),
          lwd = 0.5,
          lty = 1,
          type = 'l',
          add = T
        )
      }
    }
  }

  ## Draw lines along second dimension
  for (k in 1:xnlines) {
    xx <- x
    yy <- rep(points.y[k], length.x)
    zz <- z[, which(y == points.y[k]), 1]
    plot3D::scatter3D(
      z = zz,
      x = xx,
      y = yy,
      col = "gray40",
      lwd = 2,
      lty = 1,
      type = 'l',
      add = T
    )
    if (cis) {
      for (p in 2:3) {
        yy <- rep(min(y), length.x)
        zz <- z[, 1, p]
        plot3D::scatter3D(
          z = zz,
          x = xx,
          y = yy,
          col = grDevices::adjustcolor("gray80", alpha.f = 0.04),
          lwd = 0.5,
          lty = 1,
          type = 'l',
          add = T
        )
        yy <- rep(max(y), length.x)
        zz <- z[, length(y), p]
        plot3D::scatter3D(
          z = zz,
          x = xx,
          y = yy,
          col = grDevices::adjustcolor("gray80", alpha.f = 0.04),
          lwd = 0.5,
          lty = 1,
          type = 'l',
          add = T
        )
      }
    }
  }
  if (cis) {
    plot3D::persp3D(x,
                    y,
                    z[, , 2],
                    col = grDevices::adjustcolor("gray40", alpha.f = 0.07),
                    add = T)
    plot3D::persp3D(x,
                    y,
                    z[, , 3],
                    col = grDevices::adjustcolor("gray40", alpha.f = 0.07),
                    add = T)
  }
}
