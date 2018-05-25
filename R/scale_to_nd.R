#' Center on 0 and/or scale raster layer to [-1, 1] range
#'
#' @description This is a helper function to scales values of a raster to the same range of
#' normalized difference indexes (NDVI, EVI, ecc). "Normalizing" rasters may be useful to enable
#' an optimal confrontation of Rao'index values resulting from get_rao_index function
#' among heterogenous rasters.
#'
#' @param raster_stack a Raster* object (RasterLayer, RasterStack, RasterBrick)
#' @param scale        a boolean indicating whether to scale values to [-1, 1] on
#'                     top of centering on 0, or not.
#'
#' @return a Raster* object scaled to [-1, 1] values
#' @export
scale_to_nd <- function(raster_stack, scale = TRUE) {
  if (!any(c("RasterLayer", "RasterStack", "RasterBrick") %in% class(raster_stack)))
    stop("Do not know what to do with a ", class(raster_stack), " object; expecting a 'raster' object to scale layers.")

  if (!requireNamespace("raster", quietly = TRUE))
    stop("Need 'raster' package to scale layers.")

  raster_stack <- raster::setMinMax(raster_stack)

  rs_mins_v <- raster::minValue(raster_stack)
  rs_maxs_v <- raster::maxValue(raster_stack)

  center_v <- (rs_maxs_v + rs_mins_v) / 2
  if (scale) {
    scale  <- (rs_maxs_v - rs_mins_v) / 2
  } else {
    scale <- scale
  }

  raster::calc(
    raster_stack,
    function(numeric_v) { scale(x = numeric_v, center = center_v, scale  = scale) },
    forceApply = TRUE)
}
