library(shiny)
library(lavaan)
library(semPlot)
library(htmltools)
library(semptools)

`%||%` <- function(a,b) if (is.null(a)) b else a

# --------- 安全取值 + 数字格式 ----------
num <- function(x, d=3) {
  if (is.null(x) || length(x)==0 || is.na(x)) return(NA_character_)
  sprintf(paste0("%.", d, "f"), as.numeric(x))
}
get1 <- function(fm_all, name) if (name %in% names(fm_all)) fm_all[[name]] else NA_real_

# --------- 计算R²值（未校正和校正效应保留） ----------
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
build_model_fit_html <- function(fit, digits = 3) {
  fm  <- fitMeasures(fit)
  est <- toupper(lavInspect(fit, "options")$estimator %||% "ML")
  optm <- tryCatch({
    opt <- lavTech(fit, "optim")
    as.character(opt$optimizer %||% opt$optim.method %||% NA_character_)
  }, error = function(e) NA_character_)

  npar <- get1(fm,"npar")
  nobs <- get1(fm,"nobs")
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

  # 创建基本信息表格
  info_table <- tags$table(class="table table-sm table-bordered",
    tags$tbody(
      tags$tr(
        tags$td(style="font-weight:bold; width:200px;", "Estimator"),
        tags$td(est)
      ),
      tags$tr(
        tags$td(style="font-weight:bold;", "Number of model parameters"),
        tags$td(npar)
      ),
      tags$tr(
        tags$td(style="font-weight:bold;", "Number of observations"),
        tags$td(nobs)
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

# --------- 手动 bootstrap 间接效应（显示迭代进度） ----------
boot_indirect_ci <- function(model_string, data, R = 500, progress_cb = NULL, session = shiny::getDefaultReactiveDomain()) {
  n <- nrow(data); out <- numeric(R)
  for (i in seq_len(R)) {
    idx   <- sample.int(n, n, replace = TRUE)
    fit_b <- sem(model_string, data = data[idx, , drop = FALSE], se = "none")
    pe_b  <- parameterEstimates(fit_b, standardized = TRUE)
    ind   <- subset(pe_b, op == ":=" & (label == "indirect" | lhs == "indirect"))
    out[i] <- if (nrow(ind) >= 1) ind$est[1] else NA_real_
    if (!is.null(progress_cb)) progress_cb(i, R)
    # 轻微让出 UI 刷新机会
    if (i %% 5 == 0) Sys.sleep(0.001)
  }
  qs <- quantile(out, probs = c(.025, .975), na.rm = TRUE)
  list(samples = out, ci = round(unname(qs), 3))
}

# --------- 生成中介模型字符串 ----------
generate_mediation_model <- function(xvar, mvars, yvar, zvars = NULL, model_type = "B1") {
  n_mediators <- length(mvars)
  n_zvars <- if (is.null(zvars) || length(zvars) == 0) 0 else length(zvars)

  if (n_mediators == 0) {
    model_parts <- sprintf('%s ~ c*%s', yvar, xvar)
    if (n_zvars > 0 && model_type %in% c("A1", "B1_Z", "C2")) {
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

  # Z -> Y for A1, B1_Z, C2
  if (n_zvars > 0 && model_type %in% c("A1", "B1_Z", "C2")) {
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

# 修改UI部分，使用标准Shiny语法
ui <- fluidPage(
  titlePanel("Multiple Mediation Models: X → M1...Mn → Y"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # 数据设置
      h4("Data Settings"),
      fileInput("file", "Upload CSV (optional)", accept = ".csv"),
      checkboxInput("use_demo", "Use built-in demo data", TRUE),

      # 数据预览设置
      conditionalPanel(
        condition = "input.use_demo || input.file",
        wellPanel(
          h5("Data Preview"),
          numericInput("n_preview", "Show top N rows",
                       value = 5, min = 1, max = 50, step = 1)
        )
      ),

      # 模型配置
      wellPanel(
        h4("Model Configuration"),
        selectInput("model_type", "Model Type",
                    choices = list(
                      "Direct (X→Y)" = "direct",
                      "Mediation (X→M→Y)" = "B1",
                      "Direct (X→Y) + Z→Y" = "A1",
                      "(X→M→Y) + (Z→Y)" = "B1_Z",
                      "(X→M→Y) + (Z→Y) + (Z→M)" = "C2"
                    ),
                    selected = "B1"),

        numericInput("n_mediators", "Number of Mediators (M)",
                     value = 1, min = 0, max = 10, step = 1),

        conditionalPanel(
          condition = "input.model_type != 'direct'",
          checkboxInput("include_errors", "Include Error Terms", value = TRUE)
        ),

        conditionalPanel(
          condition = "input.model_type == 'A1' || input.model_type == 'B1_Z' || input.model_type == 'C2'",
          checkboxInput("use_covariate", "Include Covariate Z", value = TRUE)
        )
      ),

      # 变量选择
      wellPanel(
        h4("Variable Selection"),
        uiOutput("var_selectors")
      ),

      # 分析设置
      wellPanel(
        h4("Analysis Settings"),
        numericInput("nboot", "Bootstrap reps (CI for indirect effect)",
                     value = 100, min = 100, step = 100),
        actionButton("run", "Run Analysis", class = "btn-primary btn-lg", style = "width: 100%;")
      )
    ),

    mainPanel(
      width = 9,

      # 模型说明
      wellPanel(
        h4("Model Description"),
        uiOutput("model_description")
      ),

      # 主要结果
      wellPanel(
        h4("Analysis Results"),
        tabsetPanel(
          tabPanel("Data Preview", tableOutput("data_preview")),
          tabPanel("Model Specification", verbatimTextOutput("model_txt")),
          tabPanel("Model Fit", uiOutput("fit_html")),
          tabPanel("Parameter Estimates", uiOutput("pe")),
          tabPanel("Path Diagram", uiOutput("path_plot_ui"))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # 数据：上传优先，否则内置示例
  dat <- reactive({
    req(input$use_demo || !is.null(input$file))
    if (!is.null(input$file)) {
      df <- tryCatch(read.csv(input$file$datapath), error = function(e) NULL)
      validate(need(!is.null(df), "Failed to read CSV. Please check the file format."))
      return(df)
    }

    # 生成示例数据，支持多个中介变量
    set.seed(123)
    n <- 100

    # 生成X
    X <- rnorm(n)

    # 生成多个Z（协变量）
    Z1 <- rnorm(n)
    Z2 <- rnorm(n)
    Z3 <- rnorm(n)

    # 生成中介变量M1-M10（示例：受X与部分Z影响）
    M1 <- 0.5*X + 0.3*Z1 + 0.2*Z2 + rnorm(n, 0, 0.8)
    M2 <- 0.4*X + 0.2*Z1 + 0.1*Z2 + rnorm(n, 0, 0.8)
    M3 <- 0.3*X + 0.1*Z1 + 0.1*Z3 + rnorm(n, 0, 0.8)
    M4 <- 0.2*X + 0.1*Z2 + 0.05*Z3 + rnorm(n, 0, 0.8)
    M5 <- 0.1*X + 0.05*Z1 + 0.05*Z2 + rnorm(n, 0, 0.8)
    M6 <- 0.1*X + 0.05*Z2 + 0.05*Z3 + rnorm(n, 0, 0.8)
    M7 <- 0.1*X + 0.05*Z1 + 0.05*Z3 + rnorm(n, 0, 0.8)
    M8 <- 0.1*X + 0.05*Z1 + 0.05*Z2 + rnorm(n, 0, 0.8)
    M9 <- 0.1*X + 0.05*Z2 + 0.05*Z3 + rnorm(n, 0, 0.8)
    M10 <- 0.1*X + 0.05*Z1 + 0.05*Z2 + rnorm(n, 0, 0.8)

    # 生成Y（受M、X与Z影响）
    Y <- 0.6*M1 + 0.4*M2 + 0.3*M3 + 0.2*M4 + 0.1*M5 +
      0.1*M6 + 0.1*M7 + 0.1*M8 + 0.1*M9 + 0.1*M10 +
      0.2*X + 0.3*Z1 + 0.2*Z2 + 0.1*Z3 + rnorm(n, 0, 0.8)

    data.frame(X=X, M1=M1, M2=M2, M3=M3, M4=M4, M5=M5,
               M6=M6, M7=M7, M8=M8, M9=M9, M10=M10, Z1=Z1, Z2=Z2, Z3=Z3, Y=Y)
  })

  # 观察模型类型变化，自动调整中介变量数量
  observeEvent(input$model_type, {
    if (input$model_type == "direct" || input$model_type == "A1") {
      updateNumericInput(session, "n_mediators", value = 0)
    } else if (input$n_mediators == 0) {
      updateNumericInput(session, "n_mediators", value = 1)
    }
  })

  # 动态变量选择
  output$var_selectors <- renderUI({
    cols <- names(dat())
    n_mediators <- input$n_mediators

    # 过滤出可用的中介变量
    available_mediators <- cols[grepl("^M[0-9]+$", cols)]
    if (length(available_mediators) == 0) {
      available_mediators <- cols[grepl("^M", cols)]
    }

    # 限制选择的中介变量数量
    if (n_mediators > 0 && n_mediators <= length(available_mediators)) {
      selected_mediators <- available_mediators[1:n_mediators]
    } else if (n_mediators > length(available_mediators)) {
      selected_mediators <- available_mediators
    } else {
      selected_mediators <- character(0)
    }

    tagList(
      selectInput("xvar", "X (Independent Variable)",
                  choices = cols[!grepl("^M[0-9]+$", cols)],
                  selected = "X"),

      if (n_mediators > 0 && input$model_type != 'A1') {
        selectInput("mvar", "M (Mediators)",
                    choices = available_mediators,
                    selected = selected_mediators,
                    multiple = TRUE)
      },

      selectInput("yvar", "Y (Dependent Variable)",
                  choices = cols[!grepl("^M[0-9]+$", cols)],
                  selected = "Y"),

      if (input$model_type %in% c("A1", "B1_Z", "C2") && input$use_covariate) {
        cols_no_m <- cols[!grepl("^M[0-9]+$", cols)]
        available_zvars <- cols_no_m[grepl("^Z(\\d+)?$", cols_no_m)]
        if (length(available_zvars) == 0) available_zvars <- cols_no_m
        selectInput("zvar", "Z (Covariates)",
                    choices = available_zvars,
                    selected = head(available_zvars, 1),
                    multiple = TRUE)
      }
    )
  })

  # 模型描述
  output$model_description <- renderUI({
    model_type <- input$model_type
    n_mediators <- input$n_mediators

    descriptions <- list(
      "direct" = "Simple direct effect model: X → Y",
      "B1" = "Basic mediation model: X → M → Y + X → Y (direct effect)",
      "A1" = "Model with X and Z both predicting Y (no mediators)",
      "B1_Z" = "Mediation model with covariate Z predicting Y",
      "C2" = "Full model: Z influences both M and Y, plus mediation paths"
    )

    desc <- descriptions[[model_type]] %||% "Custom mediation model"

    if (n_mediators > 1 && model_type != 'A1') {
      desc <- paste0(desc, " with ", n_mediators, " mediators")
    }

    tags$div(
      tags$h5("Model Type: ", tags$code(model_type)),
      tags$p(desc),
      if (n_mediators > 0 && model_type != 'A1') {
        tags$p("Number of mediators: ", tags$strong(n_mediators))
      }
    )
  })

  # 模型字符串
  model_string <- reactive({
    req(input$xvar, input$yvar)

    mvars <- if (input$model_type == 'A1') {
      character(0)
    } else if (input$n_mediators > 0 && !is.null(input$mvar)) {
      if (is.character(input$mvar)) input$mvar else character(0)
    } else {
      character(0)
    }

    zvars <- if (input$model_type %in% c("A1", "B1_Z", "C2") &&
                 input$use_covariate && !is.null(input$zvar)) {
      input$zvar
    } else {
      NULL
    }

    generate_mediation_model(input$xvar, mvars, input$yvar, zvars, input$model_type)
  })

  output$model_txt <- renderText(model_string())

  # 创建一个reactive值来存储拟合结果
  fit_result <- reactiveVal(NULL)

  # 观察Run Analysis按钮点击
  observeEvent(input$run, {
    df <- dat()
    mdl <- model_string()

    # 检查模型是否为空
    if (trimws(mdl) == "") {
      showNotification("Model specification is empty. Please check your variable selection.",
                       type = "error")
      return(NULL)
    }

    # 使用withProgress显示进度
    withProgress(message = 'Fitting model...', value = 0, {
      tryCatch({
        # 1) 常规拟合
        incProgress(0.3, detail = "Fitting main model...")
        fit0 <- sem(mdl, data = df, se = "standard")

        # 2) Bootstrap（如果模型包含间接效应）
        boot <- NULL
        if (grepl("indirect", mdl)) {
          R <- input$nboot
          incProgress(0.1, detail = paste("Running bootstrap with", R, "replications..."))

          out <- numeric(R)
          for (i in seq_len(R)) {
            idx <- sample.int(nrow(df), nrow(df), replace = TRUE)
            fit_b <- sem(mdl, data = df[idx, , drop = FALSE], se = "none")
            pe_b <- parameterEstimates(fit_b, standardized = TRUE)

            # 查找间接效应
            indirect_params <- pe_b[pe_b$op == ":=" & grepl("indirect", pe_b$lhs), ]
            if (nrow(indirect_params) > 0) {
              out[i] <- indirect_params$est[1]
            } else {
              out[i] <- NA_real_
            }

            # 更新进度
            incProgress(0.6/R, detail = paste("Bootstrap progress:", i, "/", R))
          }

          qs <- quantile(out, probs = c(.025, .975), na.rm = TRUE)
          boot <- list(samples = out, ci = round(unname(qs), 3))
        }

        incProgress(1, detail = "Complete!")

        # 存储结果
        fit_result(list(fit = fit0, boot = boot))

        # 显示完成通知
        showNotification("Analysis completed successfully!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Model fitting error:", e$message), type = "error")
        fit_result(NULL)
      })
    })
  })



  # 所有输出都使用fit_result()
  output$fit_html <- renderUI({
    req(fit_result())
    build_model_fit_html(fit_result()$fit, digits = 3)
  })

  # 数据预览
  output$data_preview <- renderTable({
    req(dat())
    df <- dat()
    n_rows <- if (!is.null(input$n_preview)) input$n_preview else 5
    head(df, n_rows)
  }, rownames = TRUE)

  # 参数估计表
  output$pe <- renderUI({
    req(fit_result())
    fit <- fit_result()$fit
    boot <- fit_result()$boot

    pe <- parameterEstimates(fit, standardized = TRUE, ci = TRUE, level = 0.95)

    # 计算R²值
    r2_values <- calculate_r_squared(fit)

    # 如果有bootstrap结果，更新间接效应的置信区间
    if (!is.null(boot) && !is.null(boot$ci)) {
      indirect_params <- pe$op == ":=" & grepl("indirect", pe$lhs)
      if (any(indirect_params)) {
        pe$ci.lower[indirect_params][1] <- boot$ci[1]
        pe$ci.upper[indirect_params][1] <- boot$ci[2]
      }
    }

    # 创建主效应模型表格
    main_effect_rows <- pe[pe$op == "~" & grepl("^Y", pe$lhs) & grepl("^X", pe$rhs), ]

    main_effect_table <- if (nrow(main_effect_rows) > 0) {
      main_data <- data.frame(
        Path = "X → Y",
        b = sprintf("%.2f", main_effect_rows$est[1]),
        β = sprintf("%.2f", main_effect_rows$std.all[1]),
        SE = sprintf("%.2f", main_effect_rows$se[1]),
        z = sprintf("%.2f", main_effect_rows$z[1]),
        p = ifelse(main_effect_rows$pvalue[1] < 0.001, "< .001", sprintf("%.3f", main_effect_rows$pvalue[1])),
        ci.lower = sprintf("%.2f", main_effect_rows$ci.lower[1]),
        ci.upper = sprintf("%.2f", main_effect_rows$ci.upper[1]),
        stringsAsFactors = FALSE
      )

      # 创建HTML表格
      main_html <- paste0(
        "<h4>Main-effect model</h4>",
        "<table class='table table-striped table-bordered'>",
        "<thead><tr><th>Path</th><th>b</th><th>β</th><th>SE</th><th>z</th><th>p</th><th>ci.lower</th><th>ci.upper</th></tr></thead>",
        "<tbody>",
        "<tr><td>", main_data$Path, "</td><td>", main_data$b, "</td><td>", main_data$β, "</td><td>", main_data$SE, "</td><td>", main_data$z, "</td><td>", main_data$p, "</td><td>", main_data$ci.lower, "</td><td>", main_data$ci.upper, "</td></tr>",
        "</tbody></table>"
      )
      main_html
    } else {
      "<h4>Main-effect model</h4><p>No direct effect found</p>"
    }

    # 创建中介效应模型表格
    mediation_rows <- pe[pe$op == "~" & (grepl("^M", pe$lhs) | (grepl("^Y", pe$lhs) & grepl("^M", pe$rhs))), ]
    indirect_rows <- pe[pe$op == ":=" & grepl("indirect", pe$lhs), ]
    total_rows <- pe[pe$op == ":=" & grepl("total", pe$lhs), ]

    mediation_table <- if (nrow(mediation_rows) > 0 || nrow(indirect_rows) > 0 || nrow(total_rows) > 0) {
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

      # 添加路径系数
      for (i in seq_len(nrow(mediation_rows))) {
        row <- mediation_rows[i, ]
        path_label <- if (grepl("^M", row$lhs) && grepl("^X", row$rhs)) {
          # 显示具体的M变量名，如M1, M2等
          paste("X →", row$lhs)
        } else if (grepl("^Y", row$lhs) && grepl("^M", row$rhs)) {
          # 显示具体的M变量名，如M1, M2等
          paste(row$rhs, "→ Y")
        } else if (grepl("^Y", row$lhs) && grepl("^X", row$rhs)) {
          "c′: X → Y | M"
        } else {
          paste(row$lhs, "→", row$rhs)
        }

        mediation_data <- rbind(mediation_data, data.frame(
          Path = path_label,
          b = sprintf("%.2f", row$est),
          β = sprintf("%.2f", row$std.all),
          SE = sprintf("%.2f", row$se),
          z = sprintf("%.2f", row$z),
          p = ifelse(row$pvalue < 0.001, "< .001", sprintf("%.3f", row$pvalue)),
          ci.lower = sprintf("%.2f", row$ci.lower),
          ci.upper = sprintf("%.2f", row$ci.upper),
          stringsAsFactors = FALSE
        ))
      }

      # 添加间接效应
      if (nrow(indirect_rows) > 0) {
        mediation_data <- rbind(mediation_data, data.frame(
          Path = "Indirect (a×b)",
          b = sprintf("%.2f", indirect_rows$est[1]),
          β = sprintf("%.2f", indirect_rows$std.all[1]),
          SE = sprintf("%.2f", indirect_rows$se[1]),
          z = sprintf("%.2f", indirect_rows$z[1]),
          p = ifelse(indirect_rows$pvalue[1] < 0.001, "< .001", sprintf("%.3f", indirect_rows$pvalue[1])),
          ci.lower = sprintf("%.2f", indirect_rows$ci.lower[1]),
          ci.upper = sprintf("%.2f", indirect_rows$ci.upper[1]),
          stringsAsFactors = FALSE
        ))
      }

      # 添加总效应
      if (nrow(total_rows) > 0) {
        mediation_data <- rbind(mediation_data, data.frame(
          Path = "Total (c′+ab)",
          b = sprintf("%.2f", total_rows$est[1]),
          β = sprintf("%.2f", total_rows$std.all[1]),
          SE = sprintf("%.2f", total_rows$se[1]),
          z = sprintf("%.2f", total_rows$z[1]),
          p = ifelse(total_rows$pvalue[1] < 0.001, "< .001", sprintf("%.3f", total_rows$pvalue[1])),
          ci.lower = sprintf("%.2f", total_rows$ci.lower[1]),
          ci.upper = sprintf("%.2f", total_rows$ci.upper[1]),
          stringsAsFactors = FALSE
        ))
      }

      # 创建HTML表格
      mediation_html <- paste0(
        "<h4>Mediation model</h4>",
        "<table class='table table-striped table-bordered'>",
        "<thead><tr><th>Path</th><th>b</th><th>β</th><th>SE</th><th>z</th><th>p</th><th>ci.lower</th><th>ci.upper</th></tr></thead>",
        "<tbody>"
      )

      for (i in seq_len(nrow(mediation_data))) {
        row <- mediation_data[i, ]
        mediation_html <- paste0(mediation_html,
          "<tr><td>", row$Path, "</td><td>", row$b, "</td><td>", row$β, "</td><td>", row$SE, "</td><td>", row$z, "</td><td>", row$p, "</td><td>", row$ci.lower, "</td><td>", row$ci.upper, "</td></tr>"
        )
      }

      mediation_html <- paste0(mediation_html, "</tbody></table>")
      mediation_html
    } else {
      "<h4>Mediation model</h4><p>No mediation effects found</p>"
    }

    # 创建R²表格
    r2_table <- if (length(r2_values) > 0) {
      r2_data <- data.frame(
        Variable = character(),
        R2_uncorrected = character(),
        R2_corrected = character(),
        stringsAsFactors = FALSE
      )

      for (var_name in names(r2_values)) {
        r2_data <- rbind(r2_data, data.frame(
          Variable = var_name,
          R2_uncorrected = sprintf("%.3f", r2_values[[var_name]]$uncorrected),
          R2_corrected = sprintf("%.3f", r2_values[[var_name]]$corrected),
          stringsAsFactors = FALSE
        ))
      }

      r2_html <- paste0(
        "<h4>R² Values</h4>",
        "<table class='table table-striped table-bordered'>",
        "<thead><tr><th>Variable</th><th>R² uncorrected for effect conservation</th><th>R² corrected for effect conservation</th></tr></thead>",
        "<tbody>"
      )

      for (i in seq_len(nrow(r2_data))) {
        row <- r2_data[i, ]
        r2_html <- paste0(r2_html,
          "<tr><td>", row$Variable, "</td><td>", row$R2_uncorrected, "</td><td>", row$R2_corrected, "</td></tr>"
        )
      }

      r2_html <- paste0(r2_html, "</tbody></table>")
      r2_html
    } else {
      "<h4>R² Values</h4><p>Unable to calculate R² values</p>"
    }

    # 合并所有表格
    HTML(paste(main_effect_table, "<br>", mediation_table, "<br>", r2_table))
  })




######################################################################################################################################################

  # —— 自动布局（M 在上排；X/Y 在中下；Z 贴着 Y 的左上，仅用于 Y）——
  make_layout_mediation_multi <- function(
    fit,
    patt_x="^X(\\d+)?$", patt_m="^M(\\d+)?$", patt_y="^Y(\\d+)?$", patt_z="^Z(\\d+)?$",
    y_m=0.72, y_xy=0.40, z_above_y_offset=0.30,   # Z 相对 Y 的竖直偏移
    z_left_dx=c(0.18, 0.06),                      # Z 相对 Y 的水平范围：左移 [0.18, 0.06]
    wiggle_m=TRUE
  ){
    m <- semPlot::semPlotModel(fit)
    labs <- tryCatch(m@Vars$name, error=function(e) m$Vars$name)
    lay <- matrix(NA_real_, nrow=length(labs), ncol=2,
                  dimnames=list(labs, c("x","y")))
    clip01 <- function(v) pmin(pmax(v, 0.02), 0.98)

    Xs <- grep(patt_x, labs, value=TRUE)
    Ms <- grep(patt_m, labs, value=TRUE)
    Ys <- grep(patt_y, labs, value=TRUE)
    Zs <- grep(patt_z, labs, value=TRUE)

    # X：左列（中下）
    if (length(Xs)) {
      yx <- if (length(Xs)==1) y_xy else seq(y_xy-0.10, y_xy+0.10, length.out=length(Xs))
      lay[Xs,] <- cbind(rep(0.08, length(Xs)), clip01(yx))
    }

    # M*：上排（垂直分布，M1在上，M2在M1下面，M3在M2下面...）
    if (length(Ms)) {
      if (length(Ms) == 1) {
        xm <- 0.50
        ym <- y_m
      } else {
        # 多个M变量，按数字顺序从上到下分布
        xm <- rep(0.50, length(Ms))  # 水平位置居中

        # 垂直位置：M1在最上面，M2在M1下面，M3在M2下面...，间隔更大
        if (length(Ms) == 2) {
          # 2个M：上下分布，间隔更大
          ym <- c(y_m + 0.15, y_m - 0.15)
        } else if (length(Ms) == 3) {
          # 3个M：上中下分布，间隔更大
          ym <- c(y_m + 0.20, y_m, y_m - 0.20)
        } else if (length(Ms) == 4) {
          # 4个M：上下分布，每行2个，间隔更大
          ym <- c(y_m + 0.25, y_m + 0.08, y_m - 0.08, y_m - 0.25)
        } else if (length(Ms) == 5) {
          # 5个M：从上到下依次排列，间隔更大
          ym <- c(y_m + 0.30, y_m + 0.15, y_m, y_m - 0.15, y_m - 0.20)
        } else if (length(Ms) == 6) {
          # 6个M：从上到下依次排列，间隔更大
          ym <- c(y_m + 0.35, y_m + 0.20, y_m + 0.05, y_m - 0.05, y_m - 0.20, y_m - 0.35)
        } else {
          # 7个以上：使用更大的垂直范围，间隔更大
          ym <- seq(y_m + 0.40, y_m - 0.40, length.out=length(Ms))
        }
      }

      lay[Ms,] <- cbind(clip01(xm), clip01(ym))
    }

    # Y：右列（中下）
    if (length(Ys)) {
      yy <- if (length(Ys)==1) y_xy else seq(y_xy-0.10, y_xy+0.10, length.out=length(Ys))
      lay[Ys,] <- cbind(rep(0.92, length(Ys)), clip01(yy))
    }

    # Z*：垂直分布，放在Y的右上角，稍微高一点
    if (length(Zs) && length(Ys)) {
      # 获取Y的位置作为参考
      yx <- lay[Ys[1], 1]; yy <- lay[Ys[1], 2]

      if (length(Zs) == 1) {
        # 单个Z：放在Y的右上角，稍微高一点
        lay[Zs,] <- cbind(clip01(yx + 0.15), clip01(yy + 0.35))
      } else {
        # 多个Z：垂直分布，在Y的右上角，稍微高一点
        zx <- rep(yx + 0.15, length(Zs))  # 水平位置在Y的右边

        # 垂直位置：根据数量上下分布，整体调高
        if (length(Zs) == 2) {
          # 2个Z：上下分布
          zy <- c(yy + 0.40, yy + 0.30)
        } else if (length(Zs) == 3) {
          # 3个Z：上中下分布
          zy <- c(yy + 0.45, yy + 0.35, yy + 0.25)
        } else if (length(Zs) == 4) {
          # 4个Z：上下分布
          zy <- c(yy + 0.50, yy + 0.40, yy + 0.30, yy + 0.20)
        } else if (length(Zs) == 5) {
          # 5个Z：上中下分布
          zy <- c(yy + 0.55, yy + 0.45, yy + 0.35, yy + 0.25, yy + 0.15)
        } else {
          # 6个以上：使用更大的垂直范围，整体调高
          zy <- seq(yy + 0.60, yy + 0.15, length.out=length(Zs))
        }

        lay[Zs,] <- cbind(clip01(zx), clip01(zy))
      }
    }

    # 残差节点放到对应变量上方，增加垂直间距
    err_nodes <- grep("^(e\\.|ε\\.|E\\.)", rownames(lay), value=TRUE)
    if (length(err_nodes)) {
      for (e in err_nodes) {
        base <- sub("^(e\\.|ε\\.|E\\.)", "", e)
        if (base %in% rownames(lay)) {
          base_y <- lay[base, 2]
          base_x <- lay[base, 1]

          # 检查是否有Z变量在附近
          z_nearby <- FALSE
          if (length(Zs) > 0) {
            for (z in Zs) {
              if (z %in% rownames(lay)) {
                z_pos <- lay[z,]
                if (abs(z_pos[1] - base_x) < 0.20 && abs(z_pos[2] - base_y) < 0.20) {
                  z_nearby <- TRUE
                  break
                }
              }
            }
          }

          # 根据是否有Z变量调整偏移
          y_offset <- if (z_nearby) 0.20 else 0.15
          lay[e,] <- clip01(c(base_x, base_y + y_offset))
        }
      }
    }

    # 其余未定位的节点放在最下方
    miss <- which(is.na(lay[,1]))
    if (length(miss)) {
      # 检查Z变量的最低位置
      z_bottom <- if (length(Zs) > 0) min(lay[Zs, 2], na.rm=TRUE) else 0.60
      safe_y <- max(0.10, z_bottom - 0.15)  # 确保在Z变量下方
      lay[miss,] <- cbind(seq(0.10, 0.90, length.out=length(miss)), rep(safe_y, length(miss)))
    }

    lay
  }

  output$path_plot_ui <- renderUI({
    base <- 520
    kM <- as.integer(input$n_mediators %||% 0)
    extra <- max(0, kM - 3) * 200
    plotOutput("path_plot", height = paste0(base + extra, "px"))
  })


  output$path_plot <- renderPlot({
    req(fit_result())
    tryCatch({
      fit <- fit_result()$fit
      my_layout <- make_layout_mediation_multi(fit)

      # 获取模型信息，用于过滤不需要的边
      m <- semPlot::semPlotModel(fit)

      # 首先创建基础的semPaths对象，所有边都是黑色
      p <- semPlot::semPaths(
        fit,
        layout       = my_layout,
        fixed        = TRUE,
        whatLabels   = "std",
        residuals    = isTRUE(input$include_errors),  # 勾选：显示误差箭头
        intercepts   = FALSE,
        nCharNodes   = 0,
        sizeMan      = 10,
        sizeLat      = 10,
        label.cex    = 1.2,
        style        = "lisrel",
        shapeMan     = "rectangle",
        shapeLat     = "rectangle",
        node.width   = 0.3,
        node.height  = 0.2,
        edge.label.cex = 0.6,
        edge.label.color = "#C62828",
        edge.label.position = 0.6,
        edge.label.bg = 'white',
        edge.color   = "black",
        edge.label.margin = 0.02,
        edge.width   = 1.2,
        asize        = 1.5,         # 箭头整体更小（包含误差箭头）
        curve        = 0.2,
        curvePivot   = TRUE,
        residScale   = 3,           # 误差箭头更短更小
        color        = list(man = "white", lat = "white"),
        border.width = 2,
        label.color  = "blue",
        thresholds   = FALSE,
        exoVar       = FALSE,
        exoCov       = FALSE,
        what         = "paths",
        DoNotPlot    = TRUE
      )

      # 获取参数估计信息来识别Z->M的边
      pe <- lavaan::parameterEstimates(fit)

      # 更精确地识别Z->M的边：匹配zm{i}_{j}模式的参数标签
      # 这些边表示从Z变量到M变量的路径
      zm_edges <- pe[grepl("^zm\\d+_\\d+$", pe$label), ]

      if (nrow(zm_edges) > 0) {
        # 用现有颜色作为基准，避免覆盖 residCol
        edge_colors <- p$graphAttributes$Edges$color

        for (i in seq_len(nrow(zm_edges))) {
          from_var <- zm_edges$rhs[i]
          to_var   <- zm_edges$lhs[i]

          if (!is.null(p$graphAttributes$Nodes$names)) {
            node_names <- p$graphAttributes$Nodes$names
            from_idx <- which(node_names == from_var)
            to_idx   <- which(node_names == to_var)

            if (length(from_idx) > 0 && length(to_idx) > 0) {
              edge_idx <- which(p$Edgelist$from == from_idx & p$Edgelist$to == to_idx)
              if (length(edge_idx) > 0) {
                edge_colors[edge_idx] <- "red"
              }
            }
          }
        }

        # 仅更新被标记的边；其余（包括 residual）保留原色（如 residCol="blue"）
        p$graphAttributes$Edges$color <- edge_colors
      }

      # —— Residual（误差项）着色（qgraph 不支持 residCol，用后处理）——
      node_names <- p$graphAttributes$Nodes$names
      from_names <- node_names[p$Edgelist$from]
      to_names   <- node_names[p$Edgelist$to]
      vars <- unique(na.omit(node_names))

      # 残差边：指向真实变量，但来自非真实变量（幽灵误差节点）
      resid_idx <- which(!is.na(to_names) & to_names %in% vars &
                         (!from_names %in% vars | is.na(from_names)))

      if (length(resid_idx) > 0) {
        edge_colors <- p$graphAttributes$Edges$color  # 以现有颜色为基准
        edge_colors[resid_idx] <- "blue"              # 改成你想要的颜色
        p$graphAttributes$Edges$color <- edge_colors
        # 若要连同残差边标签也同色（字段可能不存在）
        if (!is.null(p$graphAttributes$Edges$label.color)) {
          p$graphAttributes$Edges$label.color[resid_idx] <- "blue"
        }
      }

      # 绘制图形
      plot(p)
      title("Mediation Model Path Diagram", cex.main = 1.5, font.main = 2)
    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error plotting path diagram:", e$message), cex = 1.2, col = "red")
    })
  })



}

shinyApp(ui, server)
