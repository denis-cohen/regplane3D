#' @title twoplanes3D
#'
#' @description Plot a three-dimensional regression prediction with two planes,
#' typically separated at a cut point in one of the two horizontal dimensions,
#' and their respective confidence intervals.
#'
#' @param z three-dimensional array of point estimates and interval values for
#' the first plane.
#' @param x A vector of \code{x}-values, matching the first dimension of
#' \code{z} for the first plane.
#' @param y A vector of \code{y}-values, matching the second dimension of
#' \code{z} for the first plane.
#' @param z2 three-dimensional array of point estimates and interval values for
#' the second plane.
#' @param x2 A vector of \code{x}-values, matching the first dimension of
#' \code{z} for the second plane.
#' @param y2 A vector of \code{y}-values, matching the second dimension of
#' \code{z} for the second plane.
#' @param cis Plot confidence intervals? Defaults to \code{TRUE}.
#' @param xnlines Suggested number of lines for the grid display of the
#' regression plane along the \code{x}-dimension. See
#' \code{\link{pretty_axis_inputs.}}.
#' @param ynlines Suggested number of lines for the grid display of the
#' regression plane along the \code{y}-dimension. See
#' \code{\link{pretty_axis_inputs.}}.
#' @param heatmap A two-dimensional matrix indicating the joint frequency
#' distribution of \code{x} and \code{y}. Will displayed as a shaded histogram
#' below the plot.
#' @param heatmap_cols Override default shadings of \code{heatmap}.
#' @param heatmap_border Override default border color of \code{heatmap}.
#' @param plane0 Add a a horizontal red plane at \code{z == 0}.
#' Defaults to \code{FALSE}.
#' @param zplane_aux Add an auxiliary horizontal reference place. Defaults to
#' \code{FALSE}.
#' @param zplane_aux_pos User-specified position of auxiliary horizontal
#' reference plane if \code{zplane_aux == TRUE}.
#' @param zplane_aux_col User-specified color of auxiliary horizontal reference
#' plane if \code{zplane_aux == TRUE}.
#' @param yplane_aux Add an auxiliary vertical reference place. Defaults to
#' \code{FALSE}.
#' @param yplane_aux_pos User-specified position of auxiliary vertical reference
#' plane if \code{yplane_aux == TRUE}.
#' @param yplane_aux_col User-specified color of auxiliary vertical reference
#' plane if \code{yplane_aux == TRUE}.
#' @param xplane_aux Add an auxiliary vertical reference place. Defaults to
#' \code{FALSE}.
#' @param xplane_aux_pos User-specified position of auxiliary vertical reference
#' plane if \code{xplane_aux == TRUE}.
#' @param xplane_aux_col User-specified color of auxiliary vertical reference
#' plane if \code{xplane_aux == TRUE}.
#' @param cex.main The magnification to be used for main titles relative to the
#' current setting of cex as in \code{\link[graphics]{par}}.
#' @inheritParams graphics::persp
#' @inheritParams plot3D::perspbox
#' @inheritParams plot3D::image3D
#' @inheritParams plot3D::scatter3D
#' @inheritParams plot3D::persp3D
#' @inheritParams graphics::par
#'
#' @return  Returns, as invisible, the viewing transformation matrix.
#'
#' @example man/examples/twoplanes3D.R
#'
#' @export
#'
#' @seealso \code{\link{plot3D}}
#'
#' @references
#' \url{https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/regplane3D/}

twoplanes3D <- function(z,
                        x,
                        y,
                        z2,
                        x2,
                        y2,
                        zlab = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        zlim = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        xnlines = 5,
                        ynlines = 5,
                        cis = TRUE,
                        heatmap = NULL,
                        heatmap_cols = NULL,
                        heatmap_border = NA,
                        plane0 = FALSE,
                        zplane_aux = FALSE,
                        zplane_aux_pos = NULL,
                        zplane_aux_col = NULL,
                        yplane_aux = FALSE,
                        yplane_aux_pos = NULL,
                        yplane_aux_col = NULL,
                        xplane_aux = FALSE,
                        xplane_aux_pos = NULL,
                        xplane_aux_col = NULL,
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
  if (cis) {
    if (!(is.array(z2) & length(dim(z2)) == 3L & dim(z2)[3] == 3L)) {
      stop(
        "z2: Please supply a three-dimensional array where the third dimension
      provides (1) point estimate, (2) lower bound, and (3) upper bound."
      )
    }
  } else {
    if (!((is.array(z2) & length(dim(z2)) == 2L | is.matrix(z2)))) {
      stop("z2: Please supply a two-dimensional array or a matrix.")
    }
    z2 <- array(z2, dim = c(dim(z2), 1L))
  }
  if (!(is.vector(x2) &
        is.numeric(x2) & length(x2) == dim(z2)[1])) {
    stop("x2: Please supply a numeric vector of equal length as the first
         dimension of z2.")
  }
  if (!(is.vector(y2) &
        is.numeric(y2) & length(y2) == dim(z2)[2])) {
    stop("y2: Please supply a numeric vector of equal length as the second
         dimension of z2.")
  }
  if (!(is.null(heatmap)) & !(is.matrix(heatmap))) {
    stop("heatmap: Please supply a matrix.")
  }

  ## Extract meta information
  length.x <- length(x)
  length.y <- length(y)
  length.x2 <- length(x2)
  length.y2 <- length(y2)
  points.x <- x[round(seq(1, length(x), length.out = xnlines))]
  points.y <- y[round(seq(1, length(y), length.out = ynlines))]
  points.x2 <- x2[round(seq(1, length(x2), length.out = xnlines))]
  points.y2 <- y2[round(seq(1, length(y2), length.out = ynlines))]
  nticks <- max(xnlines, ynlines)

  if (is.null(zlim)) {
    zlim <- c(min(c(min(z), min(z2))), max(c(max(z), max(z2))))
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
    xlim <- c(min(c(min(x), min(x2))), max(c(max(x), max(x2))))
  }
  if (is.null(ylim)) {
    ylim <- c(min(c(min(y), min(y2))), max(c(max(y), max(y2))))
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

  ## Horizontal auxiliaray reference plane
  if (isTRUE(zplane_aux)) {
    if (is.null(zplane_aux_pos)) {
      stop("zplane_aux_pos: Please supply a z-value for the vertical position.")
    }
    if (is.null(zplane_aux_col)) {
      zplane_aux_col <-
        grDevices::adjustcolor("rosybrown1", alpha.f = 0.1)
    } else {
      zplane_aux_col <-
        grDevices::adjustcolor(zplane_aux_col, alpha.f = 0.1)
    }
    p_aux.x <- seq(xlim[1], xlim[2], length.out = nticks)
    p_aux.y <- seq(ylim[1], ylim[2], length.out = nticks)
    plot3D::image3D(
      x = p_aux.x,
      y = p_aux.y,
      z = zplane_aux_pos,
      col = zplane_aux_col,
      border = grDevices::adjustcolor("white", alpha.f = 0.5),
      lwd = 2,
      add = T
    )
  }

  ## Vertical auxiliaray reference planes
  if (isTRUE(yplane_aux)) {
    if (is.null(yplane_aux_pos)) {
      stop("yplane_aux_pos: Please supply an x-value for the horizontal position.")
    }
    if (is.null(yplane_aux_col)) {
      yplane_aux_col <-
        grDevices::adjustcolor("rosybrown1", alpha.f = 0.1)
    } else {
      yplane_aux_col <-
        grDevices::adjustcolor(yplane_aux_col, alpha.f = 0.1)
    }
    p_aux.x <- seq(xlim[1], xlim[2], length.out = nticks)
    p_aux.z <- seq(zlim[1], zlim[2], length.out = nticks)
    plot3D::image3D(
      x = p_aux.x,
      y = yplane_aux_pos,
      z = p_aux.z,
      col = yplane_aux_col,
      border = grDevices::adjustcolor("white", alpha.f = 0.5),
      lwd = 2,
      add = T
    )
  }
  if (isTRUE(xplane_aux)) {
    if (is.null(xplane_aux_pos)) {
      stop("xplane_aux_pos: Please supply a y-value for the horizontal position.")
    }
    if (is.null(xplane_aux_col)) {
      xplane_aux_col <-
        grDevices::adjustcolor("rosybrown1", alpha.f = 0.1)
    } else {
      xplane_aux_col <-
        grDevices::adjustcolor(xplane_aux_col, alpha.f = 0.1)
    }
    p_aux.z <- seq(zlim[1], zlim[2], length.out = nticks)
    p_aux.y <- seq(ylim[1], ylim[2], length.out = nticks)
    plot3D::image3D(
      x = xplane_aux_pos,
      y = p_aux.y,
      z = p_aux.z,
      col = xplane_aux_col,
      border = grDevices::adjustcolor("white", alpha.f = 0.5),
      lwd = 2,
      add = T
    )
  }

  ## ---- First plane ----
  ## Draw lines along first dimension
  for (k in 1:xnlines) {
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
  for (k in 1:ynlines) {
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

  ## ---- Second plane ----
  ## Draw lines along first dimension
  for (k in 1:xnlines) {
    xx2 <- rep(points.x2[k], length.y2)
    yy2 <- y2
    zz2 <- z2[which(x2 == points.x2[k]), , 1]
    plot3D::scatter3D(
      z = zz2,
      x = xx2,
      y = yy2,
      col = "gray40",
      lwd = 2,
      lty = 1,
      type = 'l',
      add = T
    )
    if (cis) {
      for (p in 2:3) {
        xx2 <- rep(min(x2), length.y2)
        zz2 <- z2[1, , p]
        plot3D::scatter3D(
          z = zz2,
          x = xx2,
          y = yy2,
          col = grDevices::adjustcolor("gray80", alpha.f = 0.04),
          lwd = 0.5,
          lty = 1,
          type = 'l',
          add = T
        )
        xx2 <- rep(max(x2), length.y2)
        zz2 <- z2[length(x2), , p]
        plot3D::scatter3D(
          z = zz2,
          x = xx2,
          y = yy2,
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
  for (k in 1:ynlines) {
    xx2 <- x2
    yy2 <- rep(points.y2[k], length.x2)
    zz2 <- z2[, which(y2 == points.y2[k]), 1]
    plot3D::scatter3D(
      z = zz2,
      x = xx2,
      y = yy2,
      col = "gray40",
      lwd = 2,
      lty = 1,
      type = 'l',
      add = T
    )
    if (cis) {
      for (p in 2:3) {
        yy2 <- rep(min(y2), length.x2)
        zz2 <- z2[, 1, p]
        plot3D::scatter3D(
          z = zz2,
          x = xx2,
          y = yy2,
          col = grDevices::adjustcolor("gray80", alpha.f = 0.04),
          lwd = 0.5,
          lty = 1,
          type = 'l',
          add = T
        )
        yy2 <- rep(max(y2), length.x2)
        zz2 <- z2[, length(y2), p]
        plot3D::scatter3D(
          z = zz2,
          x = xx2,
          y = yy2,
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
    plot3D::persp3D(
      x2,
      y2,
      z2[, , 2],
      col = grDevices::adjustcolor("gray40", alpha.f = 0.07),
      add = T
    )
    plot3D::persp3D(
      x2,
      y2,
      z2[, , 3],
      col = grDevices::adjustcolor("gray40", alpha.f = 0.07),
      add = T
    )
  }
}
