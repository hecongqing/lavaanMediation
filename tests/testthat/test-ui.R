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

test_that("changing an analysis input invalidates the fitted result", {
  shiny::testServer(app_test_env$app_server, {
    session$setInputs(
      use_demo = TRUE,
      model_type = "B1",
      xvar = "X",
      mvar = character(0),
      yvar = "Y",
      nboot = 100,
      seed = NA_real_
    )
    session$setInputs(run = 1)
    session$flushReact()

    expect_false(is.null(fit_result()))
    expect_identical(fit_result()$xvar, "X")
    expect_identical(fit_result()$yvar, "Y")
    expect_identical(fit_result()$model_string, "Y ~ c*X\ntotal := c")

    session$setInputs(yvar = "M1")
    session$flushReact()

    expect_null(fit_result())
  })
})

test_that("concept model uses the two packaged PNG assets", {
  expect_true(file.exists(file.path(app_test_dir, "www", "concept_b1.png")))
  expect_true(file.exists(file.path(app_test_dir, "www", "concept_c2.png")))
  expect_false(file.exists(file.path(app_test_dir, "www", "concept_b1.pdf")))
  expect_false(file.exists(file.path(app_test_dir, "www", "concept_c2.pdf")))
})
