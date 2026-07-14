test_that("single-mediator B1 syntax contains direct, indirect, and total effects", {
  model <- app_test_env$generate_mediation_model(
    xvar = "X",
    mvars = "M",
    yvar = "Y",
    model_type = "B1"
  )

  expect_match(model, "M ~ a1[*]X", fixed = FALSE)
  expect_match(model, "Y ~ cprime[*]X", fixed = FALSE)
  expect_match(model, "Y ~ b1[*]M", fixed = FALSE)
  expect_match(model, "indirect := a1[*]b1", fixed = FALSE)
  expect_match(model, "total := cprime [+] [(]a1[*]b1[)]", fixed = FALSE)
})

test_that("multiple mediators receive distinct path labels", {
  model <- app_test_env$generate_mediation_model(
    xvar = "X",
    mvars = c("M1", "M2"),
    yvar = "Y",
    model_type = "B1"
  )

  expect_match(model, "M1 ~ a1[*]X", fixed = FALSE)
  expect_match(model, "M2 ~ a2[*]X", fixed = FALSE)
  expect_match(model, "indirect1 := a1[*]b1", fixed = FALSE)
  expect_match(model, "indirect2 := a2[*]b2", fixed = FALSE)
  expect_match(model, "total_indirect", fixed = TRUE)
})

test_that("C2 syntax adds covariate paths to mediators and outcome", {
  model <- app_test_env$generate_mediation_model(
    xvar = "X",
    mvars = "M",
    yvar = "Y",
    zvars = c("Z1", "Z2"),
    model_type = "C2"
  )

  expect_match(model, "M ~ zm1_1[*]Z1", fixed = FALSE)
  expect_match(model, "M ~ zm1_2[*]Z2", fixed = FALSE)
  expect_match(model, "Y ~ zy1[*]Z1", fixed = FALSE)
  expect_match(model, "Y ~ zy2[*]Z2", fixed = FALSE)
})

test_that("bootstrap helper returns named indirect-effect samples", {
  set.seed(42)
  data <- data.frame(X = rnorm(60))
  data$M <- 0.4 * data$X + rnorm(60)
  data$Y <- 0.2 * data$X + 0.5 * data$M + rnorm(60)
  model <- app_test_env$generate_mediation_model("X", "M", "Y", model_type = "B1")

  result <- app_test_env$boot_indirect_ci(model, data, R = 10, seed = 123)

  expect_equal(nrow(result$samples), 10)
  expect_identical(colnames(result$samples), "indirect")
  expect_equal(dim(result$cis), c(1L, 2L))
  expect_equal(result$successful_samples, 10)
})

test_that("analysis selections reject overlapping and non-numeric variables", {
  data <- data.frame(X = 1:5, M = 2:6, Y = 3:7, group = letters[1:5])

  expect_null(app_test_env$validate_analysis_selection(data, "B1", "X", "Y", "M"))
  expect_match(
    app_test_env$validate_analysis_selection(data, "B1", "X", "Y", "X"),
    "different variables",
    fixed = TRUE
  )
  expect_match(
    app_test_env$validate_analysis_selection(data, "C2", "X", "Y", "M"),
    "requires at least one covariate",
    fixed = TRUE
  )
  expect_match(
    app_test_env$validate_analysis_selection(data, "C2", "X", "Y", "M", "group"),
    "must be numeric",
    fixed = TRUE
  )
})
