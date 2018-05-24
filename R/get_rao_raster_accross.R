#' Computation of Rao's index on each raster stack pixel accross its layers
#'
#' @description Temporal Rao's index is calculated through the \code{spacetimerao::get_rao_index}
#' on the values each pixel of the raster stack has accross the stack layers.
#'
#' Computation can be parallelized if a \code{cluster} object is passed to \code{cl_sock} argument.
#' The function uses the \code{parallel} package, the set of copies of R running in
#' has to be already in place.
#'
#' @param raster_stack the multi-layer raster stack
#' @param cl_sock a \code{c("SOCKcluster", "cluster")} object as given by \code{parallel::makeCluster} or \code{NULL} to perform serial computation
#'
#' @return a single layer raster with same extent as the \code{raster_stack} raster stack
#' @export
get_rao_raster_accross <- function(raster_stack, cl_sock = NULL) {
  if (!requireNamespace("raster", quietly = TRUE)) stop("Need 'raster' package to run Rao's index computation on rasters.")
  if (!"raster" %in% class(raster_stack)) stop("Do not know what to do with a ", class(raster_stack), " object; expecting a 'raster' object to run parallel computation of Rao's index.")

  if (is.null(cl_sock)) {
    rao_m <- apply(
      X      = raster::as.array(raster_stack),
      MARGIN = c(1, 2),
      FUN    = spacetimerao::get_rao_index)
  } else {
    if (!"cluster" %in% class(raster_stack))  stop("Do not know what to do with a ", class(cl_sock), " object; expecting a 'cluster' object to run parallel computation of Rao's index.")
    if (!requireNamespace("parallel", quietly = TRUE)) stop("Need 'parallel' package to run parallel temporal computation.")

    rao_m <- parallel::parApply(
      cl     = cl_sock,
      X      = raster::as.array(raster_stack),
      MARGIN = c(1, 2),
      FUN    = spacetimerao::get_rao_index)
  }

  if (!is.matrix(rao_m)) stop("An error has occurred in apply spacetimerao::get_rao_index so that the matrix of Rao's index values was not produced")

  raster::raster(
    x   = rao_m,
    crs = raster::crs(raster_stack))
}
