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
    boot = list(n_samples = 100L, successful_samples = 96L)
  )

  expect_named(result, c("Section", "Metric", "Value"))
  expect_true(all(c("CFI", "TLI", "RMSEA", "SRMR") %in% result$Metric))
  expect_equal(result$Value[result$Metric == "Number of bootstrap samples"], "100")
  expect_equal(result$Value[result$Metric == "Successful bootstrap samples"], "96")
})

test_that("model-fit download does not report Bootstrap input when none ran", {
  fixture <- make_test_fit()
  result <- app_test_env$build_model_fit_download_data(fixture$fit)

  expect_true(is.na(result$Value[result$Metric == "Number of bootstrap samples"]))
  expect_true(is.na(result$Value[result$Metric == "Successful bootstrap samples"]))
})

test_that("parameter download includes main, mediation, and R2 sections", {
  fixture <- make_test_fit("C2")
  main_fit <- lavaan::sem("Y ~ X + Z", data = fixture$data)
  main_r2 <- lavaan::lavInspect(main_fit, "r2")[["Y"]]

  result <- app_test_env$build_parameter_estimates_download_data(
    fit = fixture$fit,
    main_effect_fit = main_fit,
    xvar = "X",
    yvar = "Y",
    mvars = "M",
    zvars = fixture$zvars,
    boot = NULL
  )

  expect_true(all(c("Main effect model", "Mediation model", "R2 values") %in% result$Model))
  expect_true(all(c("R2_uncorrected", "R2_corrected") %in% names(result)))
  expect_true(any(result$op == ":=" & result$lhs == "indirect"))
  expect_equal(result$R2_corrected[result$Model == "R2 values" & result$lhs == "Y"], main_r2)
})

test_that("R2 comparison uses main-effect R2 only for the outcome", {
  fixture <- make_test_fit("C2")
  main_fit <- lavaan::sem("Y ~ X + Z", data = fixture$data)

  result <- app_test_env$build_r2_comparison_data(
    fixture$fit,
    main_fit,
    yvar = "Y"
  )

  expect_true(all(c("M", "Y") %in% result$Variable))
  expect_equal(
    result$R2_corrected[result$Variable == "Y"],
    lavaan::lavInspect(main_fit, "r2")[["Y"]]
  )
  expect_equal(
    result$R2_corrected[result$Variable == "M"],
    result$R2_uncorrected[result$Variable == "M"]
  )
})
