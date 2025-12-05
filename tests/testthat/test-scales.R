test_that("scale_fill_rdataviz returns a ggplot2 scale", {
  s <- scale_fill_rdataviz()
  expect_s3_class(s, "Scale")
  expect_s3_class(s, "ggproto")
})

test_that("scale_color_rdataviz returns a ggplot2 scale", {
  s <- scale_color_rdataviz()
  expect_s3_class(s, "Scale")
  expect_s3_class(s, "ggproto")
})

test_that("scale_colour_rdataviz is alias for scale_color_rdataviz", {
  expect_identical(scale_colour_rdataviz, scale_color_rdataviz)
})

test_that("scale functions accept different palettes", {
  expect_no_error(scale_fill_rdataviz("default"))
  expect_no_error(scale_fill_rdataviz("chart"))
  expect_no_error(scale_fill_rdataviz("diabetes"))
  expect_no_error(scale_fill_rdataviz("health"))
  expect_no_error(scale_fill_rdataviz("subgroup"))
})

test_that("scale functions error on unknown palette",
{
  expect_error(scale_fill_rdataviz("unknown"), class = "rdataviz_error")
})

test_that("scale_fill_risk returns continuous scale", {
  s <- scale_fill_risk()
  expect_s3_class(s, "Scale")
  expect_s3_class(s, "ScaleContinuous")
})

test_that("scale_fill_risk reverse parameter works", {
  s_normal <- scale_fill_risk(reverse = FALSE)
  s_reverse <- scale_fill_risk(reverse = TRUE)
  expect_s3_class(s_normal, "Scale")
  expect_s3_class(s_reverse, "Scale")
})
