library(shiny)
library(lavaan)
library(semPlot)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Mediation Demo: X → M → Y (lavaan)"),
  layout_sidebar(
    sidebar = sidebar(
      fileInput("file", "Upload CSV (optional)", accept = ".csv"),
      checkboxInput("use_demo", "Use built-in demo data (if no file is uploaded)", TRUE),
      uiOutput("var_selectors"),
      numericInput("nboot", "Bootstrap reps (CI for indirect effect)", value = 500, min = 100, step = 100),
      actionButton("run", "Run analysis", class = "btn-primary")
    ),
    card(
      card_header("Model specification"),
      verbatimTextOutput("model_txt")
    ),
    card(
      card_header("Key results"),
      navset_pill(
        nav_panel("Overview", verbatimTextOutput("summary")),
        nav_panel("Parameter table", tableOutput("pe")),
        nav_panel("Path diagram", plotOutput("path_plot", height = "520px"))
      )
    )
  )
)

server <- function(input, output, session) {

  # Read/prepare data: prefer uploaded file, otherwise use built-in simulated data
  dat <- reactive({
    req(input$use_demo || !is.null(input$file))

    if (!is.null(input$file)) {
      df <- tryCatch(read.csv(input$file$datapath), error = function(e) NULL)
      validate(need(!is.null(df), "Failed to read CSV. Please check the file format."))
      return(df)
    }

    # Simple reproducible toy data: X → M → Y
    set.seed(123)
    n <- 300
    X <- rnorm(n, 0, 1)
    M <- 0.5*X + rnorm(n, 0, 1)
    Y <- 0.6*M + 0.2*X + rnorm(n, 0, 1)
    data.frame(X = X, M = M, Y = Y)
  })

  # Dynamic variable selectors
  output$var_selectors <- renderUI({
    cols <- names(dat())
    tagList(
      selectInput("xvar", "X (independent variable)", choices = cols, selected = cols[1]),
      selectInput("mvar", "M (mediator)",            choices = cols, selected = cols[2]),
      selectInput("yvar", "Y (dependent variable)",  choices = cols, selected = cols[3])
    )
  })

  # Auto-generate lavaan model string from selections
  model_string <- reactive({
    req(input$xvar, input$mvar, input$yvar)
    sprintf('
      # Mediation paths
      %s ~ a*%s
      %s ~ b*%s + cprime*%s

      # Indirect & total effects
      indirect := a*b
      total := cprime + (a*b)
    ', input$mvar, input$xvar, input@yvar, input$mvar, input$xvar)
  })

  output$model_txt <- renderText(model_string())

  # Fit lavaan model (on click)
  fit_obj <- eventReactive(input$run, {
    df <- dat()
    mdl <- model_string()
    # Use bootstrap for CIs
    sem(mdl, data = df, se = "bootstrap", bootstrap = input$nboot)
  }, ignoreInit = TRUE)

  # Overview (with standardized estimates)
  output$summary <- renderText({
    req(fit_obj())
    txt <- capture.output(summary(fit_obj(), standardized = TRUE, fit.measures = TRUE))
    paste(txt, collapse = "\n")
  })

  # Parameter table (includes indirect/total effects, standardized, CIs)
  output$pe <- renderTable({
    req(fit_obj())
    parameterEstimates(
      fit_obj(), standardized = TRUE, ci = TRUE, level = 0.95, boot.ci.type = "perc"
    )[, c("lhs","op","rhs","label","est","se","z","pvalue","ci.lower","ci.upper","std.all")]
  })

  # Path diagram (standardized path coefficients)
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
