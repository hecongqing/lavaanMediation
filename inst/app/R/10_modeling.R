# Model specification and bootstrap helpers.

require_converged_fit <- function(fit, label = "Model") {
  converged <- tryCatch(
    isTRUE(lavInspect(fit, "converged")),
    error = function(e) FALSE
  )
  if (!converged) {
    stop(paste0(label, " did not converge."), call. = FALSE)
  }
  fit
}

boot_indirect_ci <- function(
  model_string,
  data,
  R = 500,
  progress_cb = NULL,
  seed = NULL
) {
  if (length(R) != 1 || !is.numeric(R) || is.na(R) || !is.finite(R) ||
      R < 1 || R != as.integer(R)) {
    stop("R must be a positive integer.", call. = FALSE)
  }
  R <- as.integer(R)

  if (!is.data.frame(data) || nrow(data) < 3) {
    stop("Bootstrap data must contain at least three rows.", call. = FALSE)
  }

  if (!is.null(seed) && length(seed) == 1 && !is.na(seed)) {
    if (!is.numeric(seed) || !is.finite(seed) || seed < 0 ||
        seed != as.integer(seed)) {
      stop("seed must be a non-negative integer or NA.", call. = FALSE)
    }
    set.seed(as.integer(seed))
  } else if (!is.null(seed) && (length(seed) != 1 || length(seed) == 0)) {
    stop("seed must be a non-negative integer or NA.", call. = FALSE)
  }

  n <- nrow(data)
  reference_fit <- sem(model_string, data = data, se = "none")
  require_converged_fit(reference_fit, "Reference model")
  reference_pe <- parameterEstimates(reference_fit, standardized = TRUE)
  effect_names <- reference_pe$lhs[
    reference_pe$op == ":=" & grepl("indirect", reference_pe$lhs)
  ]
  out <- matrix(
    NA_real_,
    nrow = R,
    ncol = length(effect_names),
    dimnames = list(NULL, effect_names)
  )

  for (i in seq_len(R)) {
    idx   <- sample.int(n, n, replace = TRUE)
    pe_b <- tryCatch({
      fit_b <- sem(model_string, data = data[idx, , drop = FALSE], se = "none")
      require_converged_fit(fit_b, "Bootstrap model")
      parameterEstimates(fit_b, standardized = TRUE)
    }, error = function(e) NULL)

    if (!is.null(pe_b) && length(effect_names) > 0) {
      ind <- subset(pe_b, op == ":=" & lhs %in% effect_names)
      matched <- match(effect_names, ind$lhs)
      valid <- !is.na(matched)
      out[i, valid] <- ind$est[matched[valid]]
    }
    if (!is.null(progress_cb)) progress_cb(i, R)
  }

  cis <- NULL
  if (ncol(out) > 0) {
    cis <- t(apply(out, 2, function(x) quantile(x, probs = c(.025, .975), na.rm = TRUE)))
    cis <- round(cis, 3)
  }

  successful_samples <- if (ncol(out) == 0) {
    0L
  } else {
    sum(rowSums(!is.na(out)) == ncol(out))
  }

  list(
    samples = out,
    cis = cis,
    n_samples = R,
    successful_samples = successful_samples
  )
}

build_main_effect_model <- function(yvar, xvar, zvars = character(0)) {
  parts <- c(paste0(yvar, " ~ ", xvar))
  if (length(zvars) > 0) {
    parts <- c(parts, paste0(yvar, " ~ ", zvars))
  }
  paste(parts, collapse = "\n")
}

# --------- 生成中介模型字符串 ----------
generate_mediation_model <- function(xvar, mvars, yvar, zvars = NULL, model_type = "B1") {
  n_mediators <- length(mvars)
  n_zvars <- if (is.null(zvars) || length(zvars) == 0) 0 else length(zvars)

  if (n_mediators == 0) {
    model_parts <- sprintf('%s ~ c*%s', yvar, xvar)
    if (n_zvars > 0 && model_type == "C2") {
      for (i in seq_along(zvars)) {
        model_parts <- c(model_parts, sprintf('%s ~ zy%s*%s', yvar, i, zvars[i]))
      }
    }
    model_parts <- c(model_parts, "total := c")
    return(paste(model_parts, collapse = "\n"))
  }

  mediator_paths <- character(0)
  for (i in seq_along(mvars)) {
    mediator_paths <- c(mediator_paths, sprintf('%s ~ a%s*%s', mvars[i], i, xvar))
  }

  # Z -> M only for C2 model type
  if (n_zvars > 0 && model_type %in% c("C2")) {
    for (i in seq_along(mvars)) {
      for (j in seq_along(zvars)) {
        mediator_paths <- c(mediator_paths, sprintf('%s ~ zm%s_%s*%s', mvars[i], i, j, zvars[j]))
      }
    }
  }

  y_paths <- character(0)
  y_paths <- c(y_paths, sprintf('%s ~ cprime*%s', yvar, xvar))
  for (i in seq_along(mvars)) {
    y_paths <- c(y_paths, sprintf('%s ~ b%s*%s', yvar, i, mvars[i]))
  }

  # Z -> Y for C2
  if (n_zvars > 0 && model_type == "C2") {
    for (i in seq_along(zvars)) {
      y_paths <- c(y_paths, sprintf('%s ~ zy%s*%s', yvar, i, zvars[i]))
    }
  }

  indirect_defs <- character(0)
  if (n_mediators == 1) {
    indirect_defs <- c(indirect_defs, "indirect := a1*b1")
  } else {
    for (i in seq_along(mvars)) {
      indirect_defs <- c(indirect_defs, sprintf("indirect%s := a%s*b%s", i, i, i))
    }
    indirect_terms <- sapply(seq_along(mvars), function(i) sprintf("a%s*b%s", i, i))
    indirect_defs <- c(indirect_defs, paste("total_indirect := ", paste(indirect_terms, collapse = " + ")))
  }

  if (n_mediators == 1) {
    total_defs <- "total := cprime + (a1*b1)"
  } else {
    indirect_terms <- sapply(seq_along(mvars), function(i) sprintf("a%s*b%s", i, i))
    total_defs <- paste("total := cprime + (", paste(indirect_terms, collapse = " + "), ")")
  }


  paste(c(mediator_paths, y_paths, indirect_defs, total_defs), collapse = "\n")
}

validate_analysis_selection <- function(
  data,
  model_type,
  xvar,
  yvar,
  mvars = character(0),
  zvars = character(0)
) {
  if (!is.data.frame(data) || nrow(data) < 3 || ncol(data) < 2) {
    return("The analysis data must contain at least 3 rows and 2 columns.")
  }

  if (is.null(xvar) || is.null(yvar) || !nzchar(xvar) || !nzchar(yvar)) {
    return("Select both an independent variable (X) and a dependent variable (Y).")
  }

  mvars <- if (is.null(mvars)) character(0) else as.character(mvars)
  zvars <- if (is.null(zvars)) character(0) else as.character(zvars)

  if (identical(model_type, "C2") && length(zvars) == 0) {
    return("The C2 model requires at least one covariate (Z).")
  }

  selected <- c(xvar, mvars, yvar, zvars)
  if (anyDuplicated(selected)) {
    return("X, M, Y, and Z must be assigned to different variables.")
  }

  missing_columns <- setdiff(selected, names(data))
  if (length(missing_columns) > 0) {
    return(paste(
      "Selected columns are missing from the data:",
      paste(missing_columns, collapse = ", ")
    ))
  }

  non_numeric <- selected[!vapply(data[selected], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    return(paste(
      "All selected variables must be numeric:",
      paste(non_numeric, collapse = ", ")
    ))
  }

  NULL
}
