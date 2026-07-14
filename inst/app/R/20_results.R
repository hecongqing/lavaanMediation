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
calculate_r_squared <- function(fit) {
  tryCatch({
    # 获取R²值
    r_squared <- lavInspect(fit, "r2")

    # 获取标准化参数估计
    pe_std <- parameterEstimates(fit, standardized = TRUE)

    # 计算每个内生变量的R²
    endogenous_vars <- unique(pe_std$lhs[pe_std$op == "~"])

    r2_results <- list()

    for (var in endogenous_vars) {
      if (var %in% names(r_squared)) {
        # 未校正的R²
        r2_uncorrected <- r_squared[[var]]

        # 计算校正效应保留的R²
        # 这通常涉及考虑测量误差和模型复杂性
        # 这里使用一个简化的校正方法
        n_obs <- lavInspect(fit, "nobs")
        n_params <- sum(pe_std$op == "~" & pe_std$lhs == var)

        # 校正公式：R²_adj = 1 - (1 - R²) * (n-1)/(n-p-1)
        if (n_obs > n_params + 1) {
          r2_corrected <- 1 - (1 - r2_uncorrected) * (n_obs - 1) / (n_obs - n_params - 1)
        } else {
          r2_corrected <- r2_uncorrected
        }

        r2_results[[var]] <- list(
          uncorrected = r2_uncorrected,
          corrected = r2_corrected
        )
      }
    }

    return(r2_results)
  }, error = function(e) {
    return(list())
  })
}

# --------- 组装"按红色标注"的 Model fit HTML 表 ----------
build_model_fit_html <- function(fit, digits = 3, boot = NULL, nboot_input = NULL) {
  fm  <- fitMeasures(fit)
  est <- toupper(lavInspect(fit, "options")$estimator %||% "ML")
  optm <- tryCatch({
    opt <- lavTech(fit, "optim")
    as.character(opt$optimizer %||% opt$optim.method %||% NA_character_)
  }, error = function(e) NA_character_)

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
  base_chisq <- get1(fm,"baseline.chisq")
  base_df <- get1(fm,"baseline.df")
  base_p <- get1(fm,"baseline.pvalue")
  cfi <- get1(fm,"cfi")
  tli <- get1(fm,"tli")
  aic <- get1(fm,"aic")
  bic <- get1(fm,"bic")
  bic2 <- get1(fm,"bic2")
  rmsea <- get1(fm,"rmsea")
  lo <- get1(fm,"rmsea.ci.lower")
  hi <- get1(fm,"rmsea.ci.upper")
  pclose <- get1(fm,"rmsea.pclose")
  srmr <- get1(fm,"srmr")

  # 格式化RMSEA置信区间
  rmsea_ci <- if (is.na(rmsea)) NA_character_ else sprintf("%.3f (%.3f, %.3f)", rmsea, lo, hi)

  boot_n <- NA_integer_
  if (!is.null(boot) && !is.null(boot$n_samples)) {
    boot_n <- boot$n_samples
  } else if (!is.null(boot) && !is.null(boot$samples)) {
    boot_n <- if (is.matrix(boot$samples)) nrow(boot$samples) else length(boot$samples)
  } else if (!is.null(nboot_input) && !is.na(nboot_input)) {
    boot_n <- as.integer(nboot_input)
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

# --------- Model Fit 下载表 ----------
build_model_fit_download_data <- function(fit, boot = NULL, nboot_input = NULL) {
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
  if (!is.null(boot) && !is.null(boot$n_samples)) {
    boot_n <- boot$n_samples
  } else if (!is.null(boot) && !is.null(boot$samples)) {
    boot_n <- if (is.matrix(boot$samples)) nrow(boot$samples) else length(boot$samples)
  } else if (!is.null(nboot_input) && !is.na(nboot_input)) {
    boot_n <- as.integer(nboot_input)
  }

  data.frame(
    Section = c(
      rep("Model Information", 4),
      rep("Model Fit Indices", 11)
    ),
    Metric = c(
      "Estimator",
      "Number of model parameters",
      "Number of observations",
      "Number of bootstrap samples",
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
  data,
  model_type,
  xvar,
  yvar,
  mvars = character(0),
  zvars = character(0),
  main_effect_r2 = NA_real_,
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

  main_effect_parts <- c(paste0(yvar, " ~ ", xvar))
  if (length(zvars) > 0) {
    main_effect_parts <- c(main_effect_parts, paste0(yvar, " ~ ", zvars))
  }
  main_effect_fit <- sem(
    paste(main_effect_parts, collapse = "\n"),
    data = data,
    se = "standard"
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

  r2_values <- calculate_r_squared(fit)
  r2_data <- data.frame()
  if (length(r2_values) > 0) {
    r2_data <- do.call(rbind, lapply(names(r2_values), function(var_name) {
      r2_corrected <- if (identical(var_name, yvar) && !is.na(main_effect_r2)) {
        main_effect_r2
      } else {
        r2_values[[var_name]]$uncorrected
      }
      data.frame(
        Model = "R2 values",
        Path = var_name,
        lhs = var_name,
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
        R2_uncorrected = r2_values[[var_name]]$uncorrected,
        R2_corrected = r2_corrected,
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
