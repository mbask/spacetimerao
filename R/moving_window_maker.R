#' Return a function to perform the computatin of Rao's index on a moving window on a raster layer
#'
#' @description The function computes, serially or in parallel, the spatial Rao's index
#' on each layer of a raster stack on a moving window around a center pixel.
#'
#' The serial computation is carried out through the \code{raster::focal} function.
#' The parallel computation takes advantage of the \code{spatial.tools::rasterEngine} function.
#' The parallel cluster has to be initialized beforehand calling the returned
#' function by \code{spatial.tools::sfQuickInit}.
#'
#' @param raster_stack a raster stack of layers that will be used by the returned function to calculate Rao'index
#' @param window_dims_v a 2 element numeric vector of the count of x and y pixels for the moving window
#' @param parallel   whether or not to calculate the index in parallel
#'
#' @return a function accepting a numeric index relative to the layer of the \code{raster_stack} where Rao's index has to be calculated
#' @export
moving_window_maker <- function(raster_stack, window_dims_v, parallel = FALSE) {

  rao_on_vector <- function(window_a) {
    get_rao_index(as.vector(window_a))
  }

  is.even <- function(x) { x %% 2 == 0 }

  rao_on_window_par <- function(layer_index_n) {
    # raster_stack[[layer_index_n]] <- raster::setMinMax(raster_stack[[layer_index_n]])
    spatial.tools::rasterEngine(
      window_a    = raster_stack[[layer_index_n]],
      fun         = rao_on_vector,
      window_dims = window_dims_v)
  }

  rao_on_window_ser <- function(layer_index_n) {
    raster::focal(
      x   = raster_stack[[layer_index_n]],
      w   = window_weight_m,
      fun = rao_on_vector) * cells_in_win_n
  }

  if (!any(c("RasterLayer", "RasterStack", "RasterBrick") %in% class(raster_stack)))
    stop("Do not know what to do with a ", class(raster_stack), " object; expecting a 'raster' object to run parallel computation of Rao's index.")

  if (!requireNamespace("raster", quietly = TRUE))
    stop("Need 'raster' package to run Rao's index computation on rasters.")

  cells_in_win_n <- prod(window_dims_v)
  if (is.even(cells_in_win_n))
    stop("Moving window dimensions must be odd, instead given ", window_dims_v[1], "x", window_dims_v[2], ".")

  if (parallel) {
    if (!requireNamespace("spatial.tools", quietly = TRUE))
      stop("Need 'spatial.tools' package to run parallel spatial computation.")
    rao_on_window_par
  } else {
    window_weight_m <- matrix(
      data = 1 / cells_in_win_n,
      nrow = window_dims_v[1],
      ncol = window_dims_v[2])

    rao_on_window_ser
  }
}
