test_that("application UI exposes the current result tabs", {
  html <- htmltools::renderTags(app_test_env$app_ui())$html

  expect_match(html, "Model Fit", fixed = TRUE)
  expect_match(html, "Parameter Estimates", fixed = TRUE)
  expect_match(html, "Download", fixed = TRUE)
  expect_match(html, "download_model_fit", fixed = TRUE)
  expect_match(html, "download_parameter_estimates", fixed = TRUE)
  expect_false(grepl("R2 Debug", html, fixed = TRUE))
})

test_that("application server is a Shiny server function", {
  expect_true(is.function(app_test_env$app_server))
  expect_true(all(c("input", "output", "session") %in% names(formals(app_test_env$app_server))))
})
