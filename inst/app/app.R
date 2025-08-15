library(shiny)
library(lavaan)
library(semPlot)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Mediation Demo: X → M → Y (lavaan)"),
  layout_sidebar(
    sidebar = sidebar(
      fileInput("file", "上传 CSV（可选）", accept = ".csv"),
      checkboxInput("use_demo", "使用内置演示数据（若不上传文件）", TRUE),
      uiOutput("var_selectors"),
      numericInput("nboot", "Bootstrap 次数（间接效应置信区间）", value = 500, min = 100, step = 100),
      actionButton("run", "运行分析", class = "btn-primary")
    ),
    card(
      card_header("模型设定"),
      verbatimTextOutput("model_txt")
    ),
    card(
      card_header("关键结果"),
      navset_pill(
        nav_panel("概览", verbatimTextOutput("summary")),
        nav_panel("参数表", tableOutput("pe")),
        nav_panel("路径图", plotOutput("path_plot", height = "520px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # 读取/准备数据：优先用户上传，否则使用内置模拟数据
  dat <- reactive({
    req(input$use_demo || !is.null(input$file))
    
    if (!is.null(input$file)) {
      df <- tryCatch(read.csv(input$file$datapath), error = function(e) NULL)
      validate(need(!is.null(df), "无法读取 CSV，请检查文件格式。"))
      return(df)
    }
    
    # 简单可复现实验数据：X→M→Y
    set.seed(123)
    n <- 300
    X <- rnorm(n, 0, 1)
    M <- 0.5*X + rnorm(n, 0, 1)
    Y <- 0.6*M + 0.2*X + rnorm(n, 0, 1)
    data.frame(X = X, M = M, Y = Y)
  })
  
  # 动态生成变量选择框
  output$var_selectors <- renderUI({
    cols <- names(dat())
    tagList(
      selectInput("xvar", "X（自变量）", choices = cols, selected = cols[1]),
      selectInput("mvar", "M（中介）",   choices = cols, selected = cols[2]),
      selectInput("yvar", "Y（因变量）", choices = cols, selected = cols[3])
    )
  })
  
  # 根据选择自动生成 lavaan 模型语句
  model_string <- reactive({
    req(input$xvar, input$mvar, input$yvar)
    sprintf('
      # 中介路径
      %s ~ a*%s
      %s ~ b*%s + cprime*%s

      # 间接效应与总效应
      indirect := a*b
      total := cprime + (a*b)
    ', input$mvar, input$xvar, input$yvar, input$mvar, input$xvar)
  })
  
  output$model_txt <- renderText(model_string())
  
  # 运行 lavaan（click 触发）
  fit_obj <- eventReactive(input$run, {
    df <- dat()
    mdl <- model_string()
    # 使用 bootstrap 估计置信区间
    sem(mdl, data = df, se = "bootstrap", bootstrap = input$nboot)
  }, ignoreInit = TRUE)
  
  # 概览（带标准化）
  output$summary <- renderText({
    req(fit_obj())
    txt <- capture.output(summary(fit_obj(), standardized = TRUE, fit.measures = TRUE))
    paste(txt, collapse = "\n")
  })
  
  # 参数表（包含间接效应/总效应、标准化、置信区间）
  output$pe <- renderTable({
    req(fit_obj())
    parameterEstimates(
      fit_obj(), standardized = TRUE, ci = TRUE, level = 0.95, boot.ci.type = "perc"
    )[, c("lhs","op","rhs","label","est","se","z","pvalue","ci.lower","ci.upper","std.all")]
  })
  
  # 路径图（标准化路径系数）
  output$path_plot <- renderPlot({
    req(fit_obj())
    semPaths(
      fit_obj(),
      whatLabels = "std", edge.label.cex = 1.1,
      residuals = FALSE, intercepts = FALSE,
      layout = "tree", nCharNodes = 0, sizeMan = 9
    )
  })
}

shinyApp(ui, server)
