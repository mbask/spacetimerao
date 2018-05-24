context("test-rao_result_1.R")

test_that(
  "Rao result identical to Rocchini, D., Marcantonio, M., & Ricotta, C. (2017). Measuring Rao’s Q diversity index from remote sensing: An open source solution. Ecological Indicators, 72, 234–238 page 235 example 1",
  {
    i <- 1; j <- 200
    M <- matrix(c(i, i, j, i, j, j, j, j, j), byrow = TRUE, nrow = 3)
    x <- 88.44444 / 2
    expect_equal(
      object    = get_rao_index(M),
      expected  = x,
      tolerance = 0.002)
})

test_that(
  "Rao result identical to Rocchini, D., Marcantonio, M., & Ricotta, C. (2017). Measuring Rao’s Q diversity index from remote sensing: An open source solution. Ecological Indicators, 72, 234–238 page 235 example 2",
  {
    i <- 201; j <- 200
    M <- matrix(c(i, i, j, i, j, j, j, j, j), byrow = TRUE, nrow = 3)
    x <- 0.4444444 / 2
    expect_equal(
      object    = get_rao_index(M),
      expected  = x,
      tolerance = 0.002)
  })

