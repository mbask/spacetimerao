% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_to_nd.R
\name{scale_to_nd}
\alias{scale_to_nd}
\title{Center on 0 and/or scale raster layer to [-1, 1] range}
\usage{
scale_to_nd(raster_stack, scale = TRUE)
}
\arguments{
\item{raster_stack}{a Raster* object (RasterLayer, RasterStack, RasterBrick)}

\item{scale}{a boolean indicating whether to scale values to [-1, 1] on
top of centering on 0, or not.}
}
\value{
a Raster* object scaled to [-1, 1] values
}
\description{
This is a helper function to scales values of a raster to the same range of
normalized difference indexes (NDVI, EVI, ecc). "Normalizing" rasters may be useful to enable
an optimal confrontation of Rao'index values resulting from get_rao_index function
among heterogenous rasters.
}
