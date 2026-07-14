# Application user interface.

app_ui <- function() {
  fluidPage(
    titlePanel("Parallel Mediation Analysis with Effect Conservation"),

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

        # 左侧：模型选择 + 配置（合并）
        wellPanel(
          h4("Model Selection"),
          selectInput(
            "model_type", "Model Type",
                      choices = list(
              "X → M → Y" = "B1",
              "X → M → Y ← (Z)" = "C2"
            ),
            selected = "B1"
          ),

          # Number of Mediators removed; infer from selected M variables

          checkboxInput("include_errors", "Include Error Terms", value = TRUE),

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
          numericInput("seed", "Bootstrap seed (optional)",
                       value = NA, min = 0, step = 1),
          actionButton("run", "Run Analysis", class = "btn-primary btn-lg", style = "width: 100%;")
        )
      ),

      mainPanel(
        width = 9,

        # 右侧：概念模型选择（两张示意图，启动显示；运行后隐藏）
        conditionalPanel(
          condition = "!output.hasFit",
          wellPanel(
            h4("Model Selection"),
            uiOutput("concept_picker")
          )
        ),

        # 模型说明（运行后显示）
        conditionalPanel(
          condition = "output.hasFit",
        wellPanel(
          h4("Model Description"),
          uiOutput("model_description")
          )
        ),



        # 主要结果（运行后显示）
        conditionalPanel(
          condition = "output.hasFit",
        wellPanel(
          h4("Analysis Results"),
          tabsetPanel(
            tabPanel("Data Preview", tableOutput("data_preview")),
            tabPanel("Model Specification", verbatimTextOutput("model_txt")),
            tabPanel("Model Fit", uiOutput("fit_html")),
            tabPanel("Parameter Estimates", uiOutput("pe")),
            tabPanel("Path Diagram", uiOutput("path_plot_ui")),
            tabPanel(
              "Download",
              tags$div(
                style = "padding: 15px 0;",
                h4("Download Results"),
                p("Download the current analysis results as CSV files."),
                fluidRow(
                  column(
                    width = 6,
                    wellPanel(
                      h4("Model Fit"),
                      p("Model information and fit indices."),
                      downloadButton(
                        "download_model_fit",
                        "Download Model Fit (.csv)",
                        class = "btn-primary"
                      )
                    )
                  ),
                  column(
                    width = 6,
                    wellPanel(
                      h4("Parameter Estimates"),
                      p("Main-effect and mediation-model parameter estimates."),
                      downloadButton(
                        "download_parameter_estimates",
                        "Download Parameter Estimates (.csv)",
                        class = "btn-primary"
                      )
                    )
                  )
                )
              )
            )
            )
          )
        )
      )
    )
  )
}
