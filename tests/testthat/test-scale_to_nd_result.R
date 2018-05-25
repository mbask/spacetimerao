context("test-scale_to_nd.R")

test_that(
  "scale_to_nd scales raster layers to [-1, 1] range. Minimum is -1",
  {
    r   <- raster::raster(ncols = 36, nrows = 18)
    r[] <- 1:raster::ncell(r)
    rs  <- raster::stack(r, r*2, sqrt(r))
    s   <- scale_to_nd(rs)
    s_min <- raster::minValue(s)
    expected_s_min <- rep(-1, raster::nlayers(rs))
    expect_equal(
      object    = s_min,
      expected  = expected_s_min)
  })

test_that(
  "scale_to_nd scales raster layers to [-1, 1] range. Max is 1",
  {
    r   <- raster::raster(ncols = 36, nrows = 18)
    r[] <- 1:raster::ncell(r)
    rs  <- raster::stack(r, r*2, sqrt(r))
    s   <- scale_to_nd(rs)
    s_max <- raster::maxValue(s)
    expected_s_max <- rep(1, raster::nlayers(rs))
    expect_equal(
      object    = s_max,
      expected  = expected_s_max)
  })

