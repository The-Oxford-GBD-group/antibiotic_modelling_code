#' @title Plots, in 3d, a s2-manifold or r2-manifold mesh
#'
#' @description This function allows 3d drawing and imaging of a mesh.inla
#' object. it can plot either a mesh constructed on the R2 or S2
#' manifold. NOTE that it requires the 'rgl' R package and it spawns
#' interactive graphics windows. it is untested on the cluster and is
#' meant for use on local machines
#'
#' @param mesh an inla.mesh object
#'
#' @param draw.edges Logical. Draw the edges between the vertices?
#'
#' @param draw.segments Logical. Draw the segments that bound the mesh
#'   object?
#'
#' @param draw.plane Logical. Draw a planar shape to aid in displaying
#'   curvature of mesh?
#'
#' @param node.cols Numeric vector with length equal to the number of
#'   mesh vertices (mesh$n). e.g. pass in the posterior mean of the
#'   spatial random effects heights to visualize the fitted GP.
#'
#' @param col.type String taking value of either 'bw' or 'col' and
#'   determining whether the color of the surface should be drawn in
#'   black and white or in color. Only used if a node.cols vector is
#'   passed in.
#'
#' @param window.dims 2d numeric vector describing the width and
#'   height of the plotting in pixels
#'
#' @param plot.mirror Logical. Should a mirror image of the mesh be
#'   added to the plot (could help visualize mesh if printing to a
#'   static image)
#'
#' @param returns nothing but spawns an interactive plot window
#'
#' @examples
#' \dontrun{
#' # plot a mesh. add color to the background that results from
#' # the linear interpolation of randomly generated nodal (basis
#' # height) values
#' draw.s2.mesh(mesh_s,
#'   draw.edges = T, draw.segments = T, col.type = "col",
#'   node.cols = rnorm(n = mesh_s$n), draw.plane = F
#' )
#' 
#' ## take a snapshot to save to file
#' fig.path <- "/path/to/outputdir/"
#' rgl::rgl.snapshot(file.path(fig.path, "mesh.png"), top = TRUE)
#' 
#' ## shut down the graphics window
#' rgl::rgl.close()
#' }
#' 
#' @importFrom grDevices colorRampPalette
#' @export
draw.mesh <- function(mesh, draw.edges = TRUE, draw.segments = TRUE,
                      draw.plane = F, node.cols = NULL,
                      col.type = "bw", window.dims = c(840, 400),
                      plot.mirror = FALSE) {

  window.dims <- c(50, 50, 50 + window.dims[1], 50 + window.dims[2])

  if (is.null(node.cols)) {
    node.cols <- rep(1, mesh$n)
  }

  if (col.type == "bw") {
    cp <- function(n, ...) {
      return(grey.colors(n, 0.95, 0.05, ...))
    }
  }
  if (col.type == "col") {
    cp <- grDevices::colorRampPalette(c(
      "darkblue", "blue", "cyan",
      "yellow", "red", "darkred"
    ))
  }

  mesh0 <- inla.mesh.create(
    loc = cbind(0, 0), extend = list(offset = 1.1, n = 4)
    )

  mesh01 <- mesh0
  mesh02 <- mesh0
  mesh1 <- mesh
  mesh2 <- mesh
  mesh02$loc[, 1] <- mesh02$loc[, 1] * (-1)
  mesh02$loc[, 3] <- mesh02$loc[, 3] * (-1)
  mesh2$loc[, 1] <- mesh2$loc[, 1] * (-1)
  mesh2$loc[, 3] <- mesh2$loc[, 3] * (-1)

  mesh01$loc[, 1] <- mesh01$loc[, 1] - 1.1
  mesh02$loc[, 1] <- mesh02$loc[, 1] + 1.1
  mesh1$loc[, 1] <- mesh1$loc[, 1] - 1.1
  mesh2$loc[, 1] <- mesh2$loc[, 1] + 1.1

  rgl::open3d(windowRect = window.dims)
  if (draw.plane) {
    plot(mesh01,
      rgl = TRUE, col = "white", color.palette = cp,
      draw.vertices = FALSE, draw.edges = FALSE, add = TRUE
    )
    if (plot.mirror) {
      plot(mesh02,
        rgl = TRUE, col = "white", color.palette = cp,
        draw.vertices = FALSE, draw.edges = FALSE, add = TRUE
      )
    }
  }
  plot(mesh1,
    rgl = TRUE, col = node.cols, color.palette = cp,
    draw.vertices = FALSE, draw.edges = draw.edges, add = TRUE,
    draw.segments = draw.segments
  )
  if (plot.mirror) {
    plot(mesh2,
      rgl = TRUE, col = node.cols, color.palette = cp,
      draw.vertices = FALSE, draw.edges = draw.edges, add = TRUE,
      draw.segments = draw.segments
    )
  }

  rgl::view3d(0, 0, fov = 0, zoom = 0.4)
  rgl::rgl.bringtotop()
}
