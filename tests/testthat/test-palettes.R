test_that("chart_colors has 10 colors", {
  expect_length(chart_colors, 10)
})

test_that("chart_colors are valid hex codes", {
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", chart_colors)))
})

test_that("diabetes_colors has correct names", {
  expect_named(diabetes_colors, c("No Diabetes", "Prediabetes", "Diabetes"))
})

test_that("diabetes_colors are valid hex codes", {
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", diabetes_colors)))
})

test_that("risk_gradient has 6 colors", {
  expect_length(risk_gradient, 6)
})

test_that("subgroup_colors has correct names", {
  expected_names <- c("Resilient", "Vulnerable", "Expected Healthy",
                      "Expected Diabetic", "Typical")
  expect_named(subgroup_colors, expected_names)
})

test_that("get_theme_colors returns list with expected elements", {
  colors_dark <- get_theme_colors(dark_mode = TRUE)
  colors_light <- get_theme_colors(dark_mode = FALSE)

  expected_names <- c("text", "text_secondary", "background", "surface",
                      "grid", "border", "accent", "primary", "success",
                      "warning", "danger", "info")

  expect_type(colors_dark, "list")
  expect_type(colors_light, "list")
  expect_true(all(expected_names %in% names(colors_dark)))
  expect_true(all(expected_names %in% names(colors_light)))
})

test_that("dark and light mode colors differ", {
  colors_dark <- get_theme_colors(dark_mode = TRUE)
  colors_light <- get_theme_colors(dark_mode = FALSE)

  expect_false(colors_dark$text == colors_light$text)
  expect_false(colors_dark$background == colors_light$background)
})
