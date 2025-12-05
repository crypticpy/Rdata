test_that("theme_worldclass returns a ggplot2 theme object", {
  th <- theme_worldclass()
  expect_s3_class(th, "theme")
  expect_s3_class(th, "gg")
})

test_that("theme_worldclass dark mode has transparent background", {
  th <- theme_worldclass(dark_mode = TRUE)
  expect_equal(th$plot.background$fill, "transparent")
})

test_that("theme_worldclass light mode has white background", {
  th <- theme_worldclass(dark_mode = FALSE)
  expect_equal(th$plot.background$fill, "white")
})

test_that("theme_worldclass respects base_size parameter", {
  th_small <- theme_worldclass(base_size = 10)
  th_large <- theme_worldclass(base_size = 20)
  expect_true(th_small$text$size < th_large$text$size)
})

test_that("theme aliases work correctly", {
  expect_identical(theme_wc, theme_worldclass)
  expect_identical(theme_dashboard, theme_worldclass)
})
