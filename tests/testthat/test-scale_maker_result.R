context("test-scale_maker.R")

test_that("scale_maker works", {
  scale_by_ten <- scale_maker(10)
  x <- 100
  expect_equal(
    object   = scale_by_ten(10),
    expected = x)
})

test_that("scale_maker works 2", {
  scale_by_ten <- scale_maker(10, 2)
  x <- 0.05
  expect_equal(
    object   = scale_by_ten(0.0046739),
    expected = x)
})
