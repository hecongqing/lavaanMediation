# Result tables, fit statistics, and downloadable data.

build_indirect_label <- function(effect_name, xvar, yvar, mvars) {
  if (identical(effect_name, "indirect")) {
    mediator_label <- if (length(mvars) >= 1) mvars[1] else "M"
    return(paste("Indirect:", xvar, "→", mediator_label, "→", yvar))
  }

  if (grepl("^indirect\\d+$", effect_name)) {
    idx <- suppressWarnings(as.integer(sub("^indirect", "", effect_name)))
    mediator_label <- if (!is.na(idx) && idx >= 1 && idx <= length(mvars)) mvars[idx] else effect_name
    label_name <- paste0("Indirect", sub("^indirect", "", effect_name))
    return(paste0(label_name, ": ", xvar, " → ", mediator_label, " → ", yvar))
  }

  if (identical(effect_name, "total_indirect")) {
    mediator_label <- if (length(mvars) > 0) paste(mvars, collapse = ", ") else "M"
    return(paste0("Total_indirect: ", xvar, " → ", mediator_label, " → ", yvar))
  }

  effect_name
}

build_mediation_table_html <- function(pe, xvar, yvar, mvars, zvars = character(0)) {
  mvars <- if (is.null(mvars)) character(0) else mvars
  zvars <- if (is.null(zvars)) character(0) else zvars

  mediation_rows <- pe[pe$op == "~" &
                         ((pe$lhs %in% mvars & pe$rhs %in% c(xvar, zvars)) |
                            (pe$lhs %in% yvar & pe$rhs %in% c(xvar, mvars, zvars))), ]
  indirect_rows <- pe[pe$op == ":=" & grepl("indirect", pe$lhs), , drop = FALSE]
  total_rows <- pe[pe$op == ":=" & pe$lhs == "total", , drop = FALSE]

  if (nrow(mediation_rows) == 0 && nrow(indirect_rows) == 0 && nrow(total_rows) == 0) {
    return("<h4>Mediation model</h4><p>No mediation effects found</p>")
  }

  mediation_data <- data.frame(
    Path = character(),
    b = character(),
    β = character(),
    SE = character(),
    z = character(),
    p = character(),
    ci.lower = character(),
    ci.upper = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(mediation_rows))) {
    row <- mediation_rows[i, ]
    path_label <- if (row$lhs %in% mvars && row$rhs %in% xvar) {
      paste(xvar, "→", row$lhs)
    } else if (row$lhs %in% mvars && row$rhs %in% zvars) {
      paste(row$rhs, "→", row$lhs)
    } else if (row$lhs %in% yvar && row$rhs %in% mvars) {
      paste(row$rhs, "→", yvar)
    } else if (row$lhs %in% yvar && row$rhs %in% xvar) {
      paste0(xvar, " → ", yvar, " (Direct)")
    } else if (row$lhs %in% yvar && row$rhs %in% zvars) {
      paste(row$rhs, "→", yvar)
    } else {
      paste(row$rhs, "→", row$lhs)
    }

    mediation_data <- rbind(mediation_data, data.frame(
      Path = path_label,
      b = format_estimate_cell(row$est),
      β = format_estimate_cell(row$std.all),
      SE = format_estimate_cell(row$se),
      z = format_estimate_cell(row$z),
      p = format_pvalue(row$pvalue),
      ci.lower = format_estimate_cell(row$ci.lower),
      ci.upper = format_estimate_cell(row$ci.upper),
      stringsAsFactors = FALSE
    ))
  }

  if (nrow(mediation_data) > 0) {
    is_xy <- mediation_data$Path == paste0(xvar, " → ", yvar, " (Direct)")
    if (any(is_xy)) {
      mediation_data <- rbind(mediation_data[!is_xy, , drop = FALSE],
                              mediation_data[is_xy, , drop = FALSE])
    }
  }

  if (nrow(indirect_rows) > 0) {
    indirect_order <- c(
      paste0("indirect", seq_along(mvars)),
      "indirect",
      "total_indirect"
    )
    indirect_rows$order_idx <- match(indirect_rows$lhs, indirect_order)
    indirect_rows$order_idx[is.na(indirect_rows$order_idx)] <- length(indirect_order) + seq_len(sum(is.na(indirect_rows$order_idx)))
    indirect_rows <- indirect_rows[order(indirect_rows$order_idx), , drop = FALSE]

    for (i in seq_len(nrow(indirect_rows))) {
      row <- indirect_rows[i, ]
      mediation_data <- rbind(mediation_data, data.frame(
        Path = build_indirect_label(row$lhs, xvar, yvar, mvars),
        b = format_estimate_cell(row$est),
        β = format_estimate_cell(row$std.all),
        SE = format_estimate_cell(row$se),
        z = format_estimate_cell(row$z),
        p = format_pvalue(row$pvalue),
        ci.lower = format_estimate_cell(row$ci.lower),
        ci.upper = format_estimate_cell(row$ci.upper),
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(total_rows) > 0) {
    mediation_data <- rbind(mediation_data, data.frame(
      Path = paste0("Total: ", xvar, " → ", yvar),
      b = format_estimate_cell(total_rows$est[1]),
      β = format_estimate_cell(total_rows$std.all[1]),
      SE = format_estimate_cell(total_rows$se[1]),
      z = format_estimate_cell(total_rows$z[1]),
      p = format_pvalue(total_rows$pvalue[1]),
      ci.lower = format_estimate_cell(total_rows$ci.lower[1]),
      ci.upper = format_estimate_cell(total_rows$ci.upper[1]),
      stringsAsFactors = FALSE
    ))
  }

  headers <- c("Path", "b", "β", "SE", "z", "p", "ci.lower", "ci.upper")
  build_html_table("Mediation model", mediation_data, headers, "No mediation effects found")
}

build_main_effect_table_html <- function(pe, xvar, yvar, zvars = character(0)) {
  zvars <- if (is.null(zvars)) character(0) else zvars
  rhs_order <- c(xvar, zvars)
  main_rows <- pe[pe$op == "~" & pe$lhs == yvar & pe$rhs %in% rhs_order, ]

  if (nrow(main_rows) == 0) {
    return("<h4>Main effect model</h4><p>No main effects found</p>")
  }

  # Keep display order: X -> Y first, then Z -> Y
  main_rows$order_idx <- match(main_rows$rhs, rhs_order)
  main_rows <- main_rows[order(main_rows$order_idx), , drop = FALSE]

  main_data <- data.frame(
    Path = character(),
    b = character(),
    β = character(),
    SE = character(),
    z = character(),
    p = character(),
    ci.lower = character(),
    ci.upper = character(),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(main_rows))) {
    row <- main_rows[i, ]
    main_data <- rbind(main_data, data.frame(
      Path = paste0(row$rhs, " → ", row$lhs),
      b = sprintf("%.3f", row$est),
      β = sprintf("%.3f", row$std.all),
      SE = sprintf("%.3f", row$se),
      z = sprintf("%.3f", row$z),
      p = format_pvalue(row$pvalue),
      ci.lower = sprintf("%.3f", row$ci.lower),
      ci.upper = sprintf("%.3f", row$ci.upper),
      stringsAsFactors = FALSE
    ))
  }

  headers <- c("Path", "b", "β", "SE", "z", "p", "ci.lower", "ci.upper")
  build_html_table("Main effect model", main_data, headers, "No main effects found")
}
extract_r_squared <- function(fit) {
  tryCatch({
    values <- lavInspect(fit, "r2")
    result <- as.numeric(values)
    names(result) <- names(values)
    result
  }, error = function(e) numeric(0))
}

build_r2_comparison_data <- function(fit, main_effect_fit, yvar) {
  full_r2 <- extract_r_squared(fit)
  if (length(full_r2) == 0 || is.null(names(full_r2))) {
    return(data.frame(
      Variable = character(0),
      R2_uncorrected = numeric(0),
      R2_corrected = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  main_r2 <- extract_r_squared(main_effect_fit)
  corrected <- full_r2
  if (yvar %in% names(full_r2) && yvar %in% names(main_r2) &&
      !is.na(main_r2[[yvar]])) {
    corrected[[yvar]] <- main_r2[[yvar]]
  }

  data.frame(
    Variable = names(full_r2),
    R2_uncorrected = as.numeric(full_r2),
    R2_corrected = as.numeric(corrected),
    stringsAsFactors = FALSE
  )
}

# --------- Model fit HTML table ----------
build_model_fit_html <- function(fit, digits = 3, boot = NULL) {
  fm  <- fitMeasures(fit)
  est <- toupper(lavInspect(fit, "options")$estimator %||% "ML")

  npar <- get1(fm,"npar")
  nobs <- get1(fm,"ntotal")
  if (is.na(nobs)) {
    nobs_raw <- tryCatch(lavInspect(fit, "nobs"), error = function(e) NA_real_)
    if (length(nobs_raw) > 0 && !all(is.na(nobs_raw))) {
      nobs <- sum(as.numeric(nobs_raw), na.rm = TRUE)
    }
  }
  chisq <- get1(fm,"chisq")
  df <- get1(fm,"df")
  cfi <- get1(fm,"cfi")
  tli <- get1(fm,"tli")
  aic <- get1(fm,"aic")
  bic <- get1(fm,"bic")
  bic2 <- get1(fm,"bic2")
  rmsea <- get1(fm,"rmsea")
  lo <- get1(fm,"rmsea.ci.lower")
  hi <- get1(fm,"rmsea.ci.upper")
  srmr <- get1(fm,"srmr")

  # 格式化RMSEA置信区间
  rmsea_ci <- if (is.na(rmsea)) NA_character_ else sprintf("%.3f (%.3f, %.3f)", rmsea, lo, hi)

  boot_n <- NA_integer_
  boot_successful <- NA_integer_
  if (!is.null(boot) && !is.null(boot$n_samples)) {
    boot_n <- boot$n_samples
  } else if (!is.null(boot) && !is.null(boot$samples)) {
    boot_n <- if (is.matrix(boot$samples)) nrow(boot$samples) else length(boot$samples)
  }
  if (!is.null(boot) && !is.null(boot$successful_samples)) {
    boot_successful <- boot$successful_samples
  }

  # 创建基本信息表格
  info_table <- tags$table(class="table table-sm table-bordered",
    tags$tbody(
      tags$tr(
        tags$td(style="font-weight:bold; width:200px;", "Estimator"),
        tags$td(est)
      ),
      tags$tr(
        tags$td(style="font-weight:bold;", "Number of model parameters"),
        tags$td(if (is.na(npar)) "N/A" else as.character(npar))
      ),
      tags$tr(
        tags$td(style="font-weight:bold;", "Number of observations"),
        tags$td(if (is.na(nobs)) "N/A" else as.character(nobs))
      ),
      tags$tr(
        tags$td(style="font-weight:bold;", "Number of bootstrap samples"),
        tags$td(if (is.na(boot_n)) "N/A" else as.character(boot_n))
      ),
      tags$tr(
        tags$td(style="font-weight:bold;", "Successful bootstrap samples"),
        tags$td(if (is.na(boot_successful)) "N/A" else as.character(boot_successful))
      )
    )
  )

  # 创建模型拟合指标表格
  fit_table <- tags$table(class="table table-striped table-bordered",
    tags$thead(
      tags$tr(
        tags$th(style="text-align:right;", "chi-square (χ²)"),
        tags$th(style="text-align:right;", "df"),
        tags$th(style="text-align:right;", "CFI"),
        tags$th(style="text-align:right;", "TLI"),
        tags$th(style="text-align:right;", "AIC"),
        tags$th(style="text-align:right;", "BIC"),
        tags$th(style="text-align:right;", "n-adjusted BIC"),
        tags$th(style="text-align:center;", "RMSEA (90% CI)"),
        tags$th(style="text-align:right;", "SRMR")
      )
    ),
    tags$tbody(
      tags$tr(
        tags$td(style="text-align:right; font-weight:bold;", num(chisq, digits)),
        tags$td(style="text-align:right; font-weight:bold;", df),
        tags$td(style="text-align:right; font-weight:bold;", num(cfi, digits)),
        tags$td(style="text-align:right; font-weight:bold;", num(tli, digits)),
        tags$td(style="text-align:right; font-weight:bold;", num(aic, digits)),
        tags$td(style="text-align:right; font-weight:bold;", num(bic, digits)),
        tags$td(style="text-align:right; font-weight:bold;", num(bic2, digits)),
        tags$td(style="text-align:center; font-weight:bold;", rmsea_ci),
        tags$td(style="text-align:right; font-weight:bold;", num(srmr, digits))
      )
    )
  )

  # 返回两个表格
  tags$div(
    tags$h5("Model Information"),
    info_table,
    tags$br(),
    tags$h5("Model Fit Indices"),
    fit_table
  )
}

# --------- Model Fit download data ----------
build_model_fit_download_data <- function(fit, boot = NULL) {
  fm <- fitMeasures(fit)
  estimator <- toupper(lavInspect(fit, "options")$estimator %||% "ML")

  nobs <- get1(fm, "ntotal")
  if (is.na(nobs)) {
    nobs_raw <- tryCatch(lavInspect(fit, "nobs"), error = function(e) NA_real_)
    if (length(nobs_raw) > 0 && !all(is.na(nobs_raw))) {
      nobs <- sum(as.numeric(nobs_raw), na.rm = TRUE)
    }
  }

  boot_n <- NA_integer_
  boot_successful <- NA_integer_
  if (!is.null(boot) && !is.null(boot$n_samples)) {
    boot_n <- boot$n_samples
  } else if (!is.null(boot) && !is.null(boot$samples)) {
    boot_n <- if (is.matrix(boot$samples)) nrow(boot$samples) else length(boot$samples)
  }
  if (!is.null(boot) && !is.null(boot$successful_samples)) {
    boot_successful <- boot$successful_samples
  }

  data.frame(
    Section = c(
      rep("Model Information", 5),
      rep("Model Fit Indices", 11)
    ),
    Metric = c(
      "Estimator",
      "Number of model parameters",
      "Number of observations",
      "Number of bootstrap samples",
      "Successful bootstrap samples",
      "chi-square",
      "df",
      "CFI",
      "TLI",
      "AIC",
      "BIC",
      "n-adjusted BIC",
      "RMSEA",
      "RMSEA 90% CI lower",
      "RMSEA 90% CI upper",
      "SRMR"
    ),
    Value = as.character(c(
      estimator,
      get1(fm, "npar"),
      nobs,
      boot_n,
      boot_successful,
      get1(fm, "chisq"),
      get1(fm, "df"),
      get1(fm, "cfi"),
      get1(fm, "tli"),
      get1(fm, "aic"),
      get1(fm, "bic"),
      get1(fm, "bic2"),
      get1(fm, "rmsea"),
      get1(fm, "rmsea.ci.lower"),
      get1(fm, "rmsea.ci.upper"),
      get1(fm, "srmr")
    )),
    stringsAsFactors = FALSE
  )
}

# --------- Parameter Estimates 下载表 ----------
build_parameter_estimates_download_data <- function(
  fit,
  main_effect_fit,
  xvar,
  yvar,
  mvars = character(0),
  zvars = character(0),
  boot = NULL
) {
  mediation_pe <- parameterEstimates(
    fit,
    standardized = TRUE,
    ci = TRUE,
    level = 0.95
  )

  if (!is.null(boot) && !is.null(boot$cis) && nrow(boot$cis) > 0) {
    for (effect_name in rownames(boot$cis)) {
      effect_idx <- which(mediation_pe$op == ":=" & mediation_pe$lhs == effect_name)
      if (length(effect_idx) > 0) {
        mediation_pe$ci.lower[effect_idx] <- boot$cis[effect_name, 1]
        mediation_pe$ci.upper[effect_idx] <- boot$cis[effect_name, 2]
      }
    }
  }

  mediation_keep <-
    (mediation_pe$op == "~" &
       ((mediation_pe$lhs %in% mvars & mediation_pe$rhs %in% c(xvar, zvars)) |
          (mediation_pe$lhs == yvar & mediation_pe$rhs %in% c(xvar, mvars, zvars)))) |
    (mediation_pe$op == ":=" & mediation_pe$lhs %in%
       c("indirect", paste0("indirect", seq_along(mvars)), "total_indirect", "total"))
  mediation_pe <- mediation_pe[mediation_keep, , drop = FALSE]
  mediation_pe$Model <- "Mediation model"
  mediation_pe$Path <- ifelse(
    mediation_pe$op == "~",
    paste(mediation_pe$rhs, "->", mediation_pe$lhs),
    ifelse(
      mediation_pe$lhs == "total",
      paste0("Total: ", xvar, " -> ", yvar),
      vapply(
        mediation_pe$lhs,
        build_indirect_label,
        character(1),
        xvar = xvar,
        yvar = yvar,
        mvars = mvars
      )
    )
  )

  main_effect_pe <- parameterEstimates(
    main_effect_fit,
    standardized = TRUE,
    ci = TRUE,
    level = 0.95
  )
  main_effect_pe <- main_effect_pe[
    main_effect_pe$op == "~" &
      main_effect_pe$lhs == yvar &
      main_effect_pe$rhs %in% c(xvar, zvars),
    ,
    drop = FALSE
  ]
  main_effect_pe$Model <- "Main effect model"
  main_effect_pe$Path <- paste(main_effect_pe$rhs, "->", main_effect_pe$lhs)

  main_effect_pe$R2_uncorrected <- NA_real_
  main_effect_pe$R2_corrected <- NA_real_
  mediation_pe$R2_uncorrected <- NA_real_
  mediation_pe$R2_corrected <- NA_real_

  export_columns <- c(
    "Model", "Path", "lhs", "op", "rhs", "label", "est", "se", "z",
    "pvalue", "ci.lower", "ci.upper", "std.lv", "std.all",
    "R2_uncorrected", "R2_corrected"
  )

  ensure_export_columns <- function(x) {
    character_columns <- c("Model", "Path", "lhs", "op", "rhs", "label")
    for (column_name in setdiff(export_columns, names(x))) {
      x[[column_name]] <- if (column_name %in% character_columns) "" else NA_real_
    }
    x
  }
  main_effect_pe <- ensure_export_columns(main_effect_pe)
  mediation_pe <- ensure_export_columns(mediation_pe)

  r2_comparison <- build_r2_comparison_data(fit, main_effect_fit, yvar)
  r2_data <- data.frame()
  if (nrow(r2_comparison) > 0) {
    r2_data <- do.call(rbind, lapply(seq_len(nrow(r2_comparison)), function(i) {
      r2_row <- r2_comparison[i, ]
      data.frame(
        Model = "R2 values",
        Path = r2_row$Variable,
        lhs = r2_row$Variable,
        op = "r2",
        rhs = "",
        label = "",
        est = NA_real_,
        se = NA_real_,
        z = NA_real_,
        pvalue = NA_real_,
        ci.lower = NA_real_,
        ci.upper = NA_real_,
        std.lv = NA_real_,
        std.all = NA_real_,
        R2_uncorrected = r2_row$R2_uncorrected,
        R2_corrected = r2_row$R2_corrected,
        stringsAsFactors = FALSE
      )
    }))
  }

  parameter_parts <- list(
    main_effect_pe[, export_columns, drop = FALSE],
    mediation_pe[, export_columns, drop = FALSE]
  )
  if (nrow(r2_data) > 0) {
    parameter_parts <- c(parameter_parts, list(r2_data[, export_columns, drop = FALSE]))
  }
  do.call(rbind, parameter_parts)
}
