# Application server.

app_server <- function(input, output, session) {

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

  # No fixed mediator count; number inferred from selected M variables

  # 动态变量选择
  output$var_selectors <- renderUI({
    numeric_columns <- vapply(dat(), is.numeric, logical(1))
    cols <- names(dat())[numeric_columns]
    validate(need(
      length(cols) >= 2,
      "The data must contain at least two numeric columns."
    ))

    # 默认选择：优先沿用用户已有选择；否则基于名称给出建议
    suggested_x <- if ("X" %in% cols) "X" else cols[1]
    suggested_y <- if ("Y" %in% cols) "Y" else cols[length(cols)]
    suggested_m <- cols[grepl("^M", cols)]
    suggested_z <- cols[grepl("^Z", cols)]
    if (length(suggested_m) > 1) suggested_m <- suggested_m[1]
    if (length(suggested_z) > 1) suggested_z <- suggested_z[1]
    fallback_mz <- setdiff(cols, c(suggested_x, suggested_y))
    if (length(suggested_m) == 0 && length(fallback_mz) > 0) {
      suggested_m <- fallback_mz[1]
    }
    if (length(suggested_z) == 0 && length(fallback_mz) > 0) {
      suggested_z <- fallback_mz[2]
    }

    selected_x <- if (!is.null(input$xvar) && input$xvar %in% cols) input$xvar else suggested_x
    selected_y <- if (!is.null(input$yvar) && input$yvar %in% cols) input$yvar else suggested_y
    selected_mediators <- if (!is.null(input$mvar) && length(input$mvar) > 0) input$mvar else suggested_m
    selected_z <- if (!is.null(input$zvar) && length(input$zvar) > 0) input$zvar else suggested_z

    tagList(
      selectInput("xvar", "X (Independent Variable)",
                  choices = cols,
                  selected = selected_x),

        selectizeInput("mvar", "M (Mediators)",
                    choices = cols,
                    selected = selected_mediators,
                    multiple = TRUE,
                    options = list(plugins = list("remove_button"),
                                   closeAfterSelect = FALSE)),

      selectInput("yvar", "Y (Dependent Variable)",
                  choices = cols,
                  selected = selected_y),

      if (!is.null(input$model_type) && input$model_type == "C2") {
        selectizeInput("zvar", "Z (Covariates)",
                    choices = cols,
                    selected = selected_z,
                    multiple = TRUE,
                    options = list(plugins = list("remove_button"),
                                   closeAfterSelect = TRUE))
      }
    )
  })

  # 模型描述
  output$model_description <- renderUI({
    model_type <- input$model_type
    n_mediators <- if (!is.null(input$mvar)) length(input$mvar) else 0

    descriptions <- list(
      "B1" = "Basic mediation model: X → M → Y [direct effect included]",
      "C2" = "Mediation model with covariates: X → M → Y ← (Z) [direct effect included]"
    )

    desc <- descriptions[[model_type]] %||% "Custom mediation model"

    if (n_mediators > 1) {
      desc <- paste0(desc, " with ", n_mediators, " mediators")
    }

    tags$div(
      tags$p(desc),
      if (n_mediators > 0) {
        tags$p("Number of mediators: ", tags$strong(n_mediators))
      }
    )
  })



  # —— 左侧：两张PDF示意图的模型选择 ——
  output$concept_picker <- renderUI({
    tags$div(
      fluidRow(
        column(6,
          uiOutput("concept_left"),
          actionButton("concept_pick_left", "Choose X→M→Y", width = '100%', class = 'btn-primary')
        ),
        column(6,
          uiOutput("concept_right"),
          actionButton("concept_pick_right", "Choose X→M→Y←Z", width = '100%', class = 'btn-primary')
        )
      )
    )
  })


  output$concept_left <- renderUI({
    concept_img_or_pdf("concept_b1")
  })

  output$concept_right <- renderUI({
    concept_img_or_pdf("concept_c2")
  })

  observeEvent(input$concept_pick_left, {
    updateSelectInput(session, "model_type", selected = "B1")
    updateCheckboxInput(session, "include_errors", value = FALSE)
  })
  observeEvent(input$concept_pick_right, {
    updateSelectInput(session, "model_type", selected = "C2")
    updateCheckboxInput(session, "include_errors", value = FALSE)
    pick_default_z()
  })

  # 设置默认Z（若存在）
  pick_default_z <- function() {
    cols <- names(dat())
    available_zvars <- cols
    suggested_z <- cols[grepl("^Z", cols)]
    if (length(suggested_z) == 0) suggested_z <- available_zvars
    if (!is.null(input$zvar) && length(input$zvar) > 0) return(invisible(NULL))
    updateSelectInput(session, "zvar", choices = available_zvars, selected = head(suggested_z, 1))
  }

  # 模型字符串
  model_string <- reactive({
    req(input$xvar, input$yvar)

    mvars <- if (!is.null(input$mvar) && length(input$mvar) > 0) as.character(input$mvar) else character(0)

    zvars <- if (input$model_type == "C2" && !is.null(input$zvar)) {
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

    mvars <- if (!is.null(input$mvar)) input$mvar else character(0)
    zvars <- if (identical(input$model_type, "C2") && !is.null(input$zvar)) {
      input$zvar
    } else {
      character(0)
    }
    validation_error <- validate_analysis_selection(
      data = df,
      model_type = input$model_type,
      xvar = input$xvar,
      yvar = input$yvar,
      mvars = mvars,
      zvars = zvars
    )
    if (!is.null(validation_error)) {
      showNotification(validation_error, type = "error", duration = 6)
      return(NULL)
    }

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
          boot <- boot_indirect_ci(
            model_string = mdl,
            data = df,
            R = R,
            seed = input$seed,
            progress_cb = function(i, total) {
              incProgress(0.6 / total, detail = paste("Bootstrap progress:", i, "/", total))
            }
          )
          showNotification(
            paste0(
              "Bootstrap completed: ", boot$successful_samples,
              " of ", boot$n_samples, " samples succeeded."
            ),
            type = "message",
            duration = 3
          )
        } else {
          showNotification("No bootstrap run (model has no indirect effect).",
                           type = "message",
                           duration = 3)
        }

        # 3) 计算主效应模型的R²
        incProgress(0.9, detail = "Calculating main-effect model R²...")
        main_effect_r2 <- tryCatch({
          # 创建主效应模型
          if (input$model_type == "C2" && !is.null(input$zvar) && length(input$zvar) > 0) {
            main_effect_parts <- paste0(input$yvar, " ~ ", input$xvar)
            for (z in input$zvar) {
              main_effect_parts <- c(main_effect_parts, paste0(input$yvar, " ~ ", z))
            }
            main_effect_model <- paste(main_effect_parts, collapse = "\n")
          } else {
            main_effect_model <- paste0(input$yvar, " ~ ", input$xvar)
          }

          # 拟合主效应模型并获取R²
          fit_main <- sem(main_effect_model, data = df)
          main_r2 <- lavInspect(fit_main, "r2")
          if (input$yvar %in% names(main_r2)) {
            main_r2[[input$yvar]]
          } else {
            NA_real_
          }
        }, error = function(e) {
          NA_real_
        })

        incProgress(1, detail = "Complete!")

        # 存储结果
        fit_result(list(fit = fit0, boot = boot, main_effect_r2 = main_effect_r2))

        # 显示完成通知
        showNotification("Analysis completed successfully!", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Model fitting error:", e$message), type = "error")
        fit_result(NULL)
      })
    })
  })



  # 所有输出都使用fit_result()
  # 标记：是否已有拟合结果，用于UI条件显示
  output$hasFit <- reactive({
    !is.null(fit_result())
  })
  outputOptions(output, "hasFit", suspendWhenHidden = FALSE)
  output$fit_html <- renderUI({
    req(fit_result())
    build_model_fit_html(
      fit_result()$fit,
      digits = 3,
      boot = fit_result()$boot,
      nboot_input = input$nboot
    )
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
    model_type <- input$model_type
    xvar <- input$xvar
    yvar <- input$yvar
    mvars <- if (!is.null(input$mvar) && length(input$mvar) > 0) input$mvar else character(0)
    zvars <- if (model_type == "C2" &&
                 !is.null(input$zvar) && length(input$zvar) > 0) {
      input$zvar
    } else {
      character(0)
    }

    pe <- parameterEstimates(fit, standardized = TRUE, ci = TRUE, level = 0.95)

    # 这里调用 calculate_r_squared() 取得 R² 结果，
    # 并在下方把它们组装成页面里显示的 "R² Values" 表格。
    r2_values <- calculate_r_squared(fit)

    # 生成 R² Values 表格 HTML（供两个模型类型共用）
    r2_table_html <- {
      if (length(r2_values) > 0) {
        r2_data <- data.frame(
          Variable = character(),
          R2_uncorrected = character(),
          R2_corrected = character(),
          stringsAsFactors = FALSE
        )

        for (var_name in names(r2_values)) {
          # R² corrected for effect conservation = R² of Y from main-effect model
          r2_corrected <- if (var_name == input$yvar) {
            # For Y variable, use R² from main-effect model
            main_effect_r2 <- fit_result()$main_effect_r2
            if (!is.na(main_effect_r2)) {
              main_effect_r2
            } else {
              r2_values[[var_name]]$uncorrected
            }
          } else {
            # For other variables (M), use the same as uncorrected
            r2_values[[var_name]]$uncorrected
          }

          r2_data <- rbind(r2_data, data.frame(
            Variable = var_name,
            R2_uncorrected = sprintf("%.3f", r2_values[[var_name]]$uncorrected),
            R2_corrected = sprintf("%.3f", r2_corrected),
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
        r2_uncorrected_label <- if (identical(row$Variable, input$yvar)) {
          paste0("<strong>", row$R2_uncorrected, "</strong>")
        } else {
          row$R2_uncorrected
        }
        r2_corrected_label <- if (identical(row$Variable, input$yvar)) {
          paste0("<strong>", row$R2_corrected, "</strong>")
        } else {
          row$R2_corrected
        }
        r2_html <- paste0(r2_html,
          "<tr><td>", row$Variable, "</td><td>", r2_uncorrected_label, "</td><td>", r2_corrected_label, "</td></tr>"
        )
      }

        r2_html <- paste0(r2_html, "</tbody></table>")
        r2_html
      } else {
        "<h4>R² Values</h4><p>Unable to calculate R² values</p>"
      }
    }

    # 如果有 bootstrap 结果，按每个间接效应分别更新置信区间
    if (!is.null(boot) && !is.null(boot$cis) && nrow(boot$cis) > 0) {
      ci_names <- rownames(boot$cis)
      for (effect_name in ci_names) {
        effect_idx <- which(pe$op == ":=" & pe$lhs == effect_name)
        if (length(effect_idx) > 0) {
          pe$ci.lower[effect_idx] <- boot$cis[effect_name, 1]
          pe$ci.upper[effect_idx] <- boot$cis[effect_name, 2]
        }
      }
    }

    main_effect_table <- tryCatch({
      main_effect_parts <- c(paste0(yvar, " ~ ", xvar))
      if (length(zvars) > 0) {
        main_effect_parts <- c(main_effect_parts, paste0(yvar, " ~ ", zvars))
      }
      main_effect_model <- paste(main_effect_parts, collapse = "\n")
      fit_main <- sem(main_effect_model, data = dat(), se = "standard")
      pe_main <- parameterEstimates(fit_main, standardized = TRUE, ci = TRUE, level = 0.95)
      build_main_effect_table_html(pe_main, xvar, yvar, zvars)
    }, error = function(e) {
      "<h4>Main effect model</h4><p>Unable to estimate main effect model</p>"
    })

    if (model_type == "B1") {
      # B1模型：显示X->Y (main effect) 和 X->M->Y (mediation)


      # 2. Mediation model (X->M->Y)
      mediation_table <- build_mediation_table_html(pe, xvar, yvar, mvars)

      # 合并B1模型的表格 + R²
      HTML(paste(main_effect_table, "<br>", mediation_table, "<br>", r2_table_html))

    } else if (model_type == "C2") {
      # C2模型：显示X->Y + Z->Y 和 (X→M→Y) + (Z→Y) + (Z→M)

      # 2. Mediation model ((X→M→Y) + (Z→Y) + (Z→M))
      mediation_table <- build_mediation_table_html(pe, xvar, yvar, mvars, zvars)

      # 合并C2模型的表格 + R²
      HTML(paste(main_effect_table, "<br>", mediation_table, "<br>", r2_table_html))
    }
  })

  output$download_model_fit <- downloadHandler(
    filename = function() {
      paste0("model_fit_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      req(fit_result())
      model_fit_data <- build_model_fit_download_data(
        fit_result()$fit,
        boot = fit_result()$boot,
        nboot_input = input$nboot
      )
      write.csv(model_fit_data, file, row.names = FALSE, na = "")
    }
  )

  output$download_parameter_estimates <- downloadHandler(
    filename = function() {
      paste0("parameter_estimates_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      req(fit_result())

      model_type <- input$model_type
      xvar <- input$xvar
      yvar <- input$yvar
      mvars <- if (!is.null(input$mvar) && length(input$mvar) > 0) input$mvar else character(0)
      zvars <- if (model_type == "C2" &&
                   !is.null(input$zvar) && length(input$zvar) > 0) {
        input$zvar
      } else {
        character(0)
      }

      parameter_data <- build_parameter_estimates_download_data(
        fit_result()$fit,
        data = dat(),
        model_type = model_type,
        xvar = xvar,
        yvar = yvar,
        mvars = mvars,
        zvars = zvars,
        main_effect_r2 = fit_result()$main_effect_r2,
        boot = fit_result()$boot
      )
      write.csv(parameter_data, file, row.names = FALSE, na = "")
    }
  )


  output$path_plot_ui <- renderUI({
    base <- 520
    kM <- tryCatch(length(input$mvar), error=function(e) 0)
    extra <- max(0, kM - 3) * 200
    # 垂直堆叠两个图，高度加倍
    plotOutput("path_plot", height = paste0((base + extra) * 2, "px"))
  })


  output$path_plot <- renderPlot({
    req(fit_result())
    tryCatch({
      fit <- fit_result()$fit
      model_type <- input$model_type
      include_errors <- isTRUE(input$include_errors)
      zvars <- if (model_type == "C2" &&
                   !is.null(input$zvar) && length(input$zvar) > 0) input$zvar else character(0)

      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mfrow = c(2, 1), oma = c(3, 1, 1, 1))

      if (model_type == "B1") {
        # 图1: X->Y (Main effect)
        main_effect_model <- build_main_effect_model(input$yvar, input$xvar)
        fit_main <- sem(main_effect_model, data = dat())
        layout_main <- make_layout_mediation_multi(
          fit_main,
          xvars = input$xvar,
          yvars = input$yvar
        )
        p1 <- build_sem_paths_plot(fit_main, layout_main, include_errors)
        draw_path_panel(p1, "Main-effect model: X → Y")

        # 图2: X->M->Y (Mediation)
        my_layout <- make_layout_mediation_multi(
          fit,
          xvars = input$xvar,
          yvars = input$yvar,
          mvars = input$mvar
        )
        p2 <- build_sem_paths_plot(fit, my_layout, include_errors)
        p2 <- mark_residual_edges(p2)
        draw_path_panel(p2, "Mediation model: X → M → Y")

      } else if (model_type == "C2") {
        # 图1: X->Y + Z->Y (Main effect)
        main_effect_model <- build_main_effect_model(input$yvar, input$xvar, zvars)
        fit_main <- sem(main_effect_model, data = dat())
        layout_main <- make_layout_mediation_multi(
          fit_main,
          xvars = input$xvar,
          yvars = input$yvar,
          zvars = zvars
        )
        p1 <- build_sem_paths_plot(fit_main, layout_main, include_errors)
        draw_path_panel(p1, "Main-effect model: X→Y←(Z)")

        # 图2: (X→M→Y) + (Z→Y) + (Z→M) (Mediation)
        my_layout <- make_layout_mediation_multi(
          fit,
          xvars = input$xvar,
          yvars = input$yvar,
          mvars = input$mvar,
          zvars = zvars
        )
        p2 <- build_sem_paths_plot(fit, my_layout, include_errors)
        p2 <- mark_zm_edges(p2, fit)
        p2 <- mark_residual_edges(p2)
        draw_path_panel(p2, "Mediation model: X→M→Y←(Z)")
      }

      mtext("Standardized coefficients are reported in the diagram.",
            side = 1, line = 1, outer = TRUE, cex = 1.1, col = "black")

    }, error = function(e) {
      plot(1, 1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, paste("Error plotting path diagram:", e$message), cex = 1.2, col = "red")
    })
  })



}
