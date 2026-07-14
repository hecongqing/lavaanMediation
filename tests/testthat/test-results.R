make_test_fit <- function(model_type = "B1") {
  set.seed(7)
  data <- data.frame(X = rnorm(120), Z = rnorm(120))
  data$M <- 0.45 * data$X + 0.20 * data$Z + rnorm(120)
  data$Y <- 0.25 * data$X + 0.55 * data$M + 0.15 * data$Z + rnorm(120)
  zvars <- if (identical(model_type, "C2")) "Z" else character(0)
  model <- app_test_env$generate_mediation_model("X", "M", "Y", zvars, model_type)

  list(
    data = data,
    fit = lavaan::sem(model, data = data),
    zvars = zvars
  )
}

test_that("model-fit download contains displayed fit metrics", {
  fixture <- make_test_fit()
  result <- app_test_env$build_model_fit_download_data(
    fixture$fit,
    nboot_input = 100
  )

  expect_named(result, c("Section", "Metric", "Value"))
  expect_true(all(c("CFI", "TLI", "RMSEA", "SRMR") %in% result$Metric))
  expect_equal(result$Value[result$Metric == "Number of bootstrap samples"], "100")
})

test_that("parameter download includes main, mediation, and R2 sections", {
  fixture <- make_test_fit("C2")
  main_fit <- lavaan::sem("Y ~ X + Z", data = fixture$data)
  main_r2 <- lavaan::lavInspect(main_fit, "r2")[["Y"]]

  result <- app_test_env$build_parameter_estimates_download_data(
    fit = fixture$fit,
    data = fixture$data,
    model_type = "C2",
    xvar = "X",
    yvar = "Y",
    mvars = "M",
    zvars = fixture$zvars,
    main_effect_r2 = main_r2
  )

  expect_true(all(c("Main effect model", "Mediation model", "R2 values") %in% result$Model))
  expect_true(all(c("R2_uncorrected", "R2_corrected") %in% names(result)))
  expect_true(any(result$op == ":=" & result$lhs == "indirect"))
  expect_equal(result$R2_corrected[result$Model == "R2 values" & result$lhs == "Y"], main_r2)
})
