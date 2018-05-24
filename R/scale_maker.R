#' Scale a numeric vector by a product factor and round the result
#'
#' @param prod_factor numeric vector of scaling factors
#' @param digits integer indicating the number of decimal places
#'
#' @return a function to scale by \code{prod_factor} and round to the \code{digits} digits. The function accepts a numeric vector
#' @export
#'
#' @examples
#' scale_by_ten <- scale_maker(10)
#' scale_by_ten(10)
#' # [1] 100
#' scale_by_ten(0.0046739)
#' # [1] 0
#' scale_by_ten <- scale_maker(10, 2)
#' scale_by_ten(0.0046739)
#' # [1] 0.05
scale_maker <- function(prod_factor, digits = 1) {
  scale_numeric_vector <- function(numeric_v) {
    stopifnot(is.numeric(numeric_v))
    round(numeric_v * prod_factor, digits)
  }
  stopifnot(is.numeric(prod_factor))
  stopifnot(is.numeric(digits))
  stopifnot(length(digits) == 1)

  scale_numeric_vector
}
