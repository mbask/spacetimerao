#' Rao's index of a numeric vector
#'
#' @description Calculates Rao's index.
#'
#' @details Rao's quadratic entropy is a measure of diversity of ecological communities
#' defined by Rao (1982) and is based on the proportion of the abundance of
#' species present in a community and some measure of distance (dissimilarity)
#' among them. Dissimilarity is defined as the euclidean distance.
#' \deqn{\rho = \deqn{\sum_{ij}d_{ij} p_i p_j }}
#'
#' The function accepts a numeric value vector (or matrix of numeric values) and returns Rao's index.
#' It is particularly useful in remote sensing applications as it can be used to calculate a temporal diversity index
#' between layers of raster stacks.
#'
#'
#' @param numeric_v numeric vector of 3 values at the very least
#'
#' @return a scalar value (the rao's index)
#' @export
#'
#' @importFrom RcppAlgos comboGeneral
#'
#' @references
#' Rao, C. R. (1982). Diversity and dissimilarity coefficients: a unified approach. Theoretical Population Biology, 21(1), 24–43
#' \url{https://doi.org/10.1016/0040-5809(82)90004-1}
#' @examples
#' i <- 1; j <- 200
#' m <- c(i, i, j, i, j, j, j, j, j)
#' get_rao_index(m)
#'
#' i <- 201; j <- 200
#' m <- c(i, i, j, i, j, j, j, j, j)
#' get_rao_index(m)
#'
#' # pass a matrix to get_rao_index, as a matrix is just a vector with an x and y dimensions
#' M <- matrix(m, byrow = TRUE, nrow = 3)
#' identical(get_rao_index(M), get_rao_index(m))
#' # [1] TRUE
get_rao_index <- function(numeric_v) {

  stopifnot(is.numeric(numeric_v))
  stopifnot(length(numeric_v) > 2)

  # Return NA or NaN if numeric_v vector/matrix is composed by only NAs or NaNs
  if (all(is.na(numeric_v)) | all(is.nan(numeric_v))) { numeric_v[1] } else {

    no_nas_numeric_v <- numeric_v[!is.na(numeric_v)]
    # Return 0 only 1 unique value (exept NAs, if present) in numeric_v vector/matrix
      if (length(unique(no_nas_numeric_v)) == 1) { 0 } else {

        # proportion table of values
        p <- table(no_nas_numeric_v) / length(no_nas_numeric_v)
        # character vector of values
        values_v <- as.numeric(rownames(p))
        # vector of euclidean distances of all pairwise combinations of values
        pairwise_dists_v <- apply(
          X      = RcppAlgos::comboGeneral(values_v, 2),
          MARGIN = 1,
          FUN    = function(pair_v) {abs(pair_v[1] - pair_v[2])})
        # vector of products of all pairwise combinations of proportions
        pairwise_product_v <- apply(
          X      = RcppAlgos::comboGeneral(p, 2),
          MARGIN = 1,
          FUN    = prod,
          na.rm  = TRUE)
        # summation of the pairwise products of distances and proportions
        sum(pairwise_dists_v * pairwise_product_v)
      }
    }
}

