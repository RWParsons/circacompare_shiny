library(shiny)
library(circacompare)
library(tidyverse)
library(kableExtra)

ui <- fluidPage(
  titlePanel("CircaCompare"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      tags$br(),
      uiOutput("time_selecter"),
      uiOutput("group_selecter"),
      uiOutput("outcome_selecter"),
      tags$br(),
      uiOutput("ui.action")
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("contents")
    )
  )
)

server <- function(input, output, session) {
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })

  cols <- reactive({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    } else {
      return(names(df))
    }
  })

  output$time_selecter <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    selectInput("time", "Select the TIME (INDEPENDENT) variable from:", cols())
  })

  output$group_selecter <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    selectInput("group", "Select the GROUPING variable from:", cols())
  })

  output$outcome_selecter <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    selectInput("outcome", "Select the OUTCOME (DEPENDENT) variable from:", cols())
  })

  output$ui.action <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    actionButton("action", "Run")
  })

  observeEvent(input$action, {
    isolate({
      df <- filedata()
      if (is.null(df)) {
        return(NULL)
      }
      set.seed(42)
      cc_obj <- circacompare(
        df,
        col_time = input$time,
        col_group = input$group,
        col_outcome = input$outcome
      )
    })

    output$contents <- renderText({
      if (class(cc_obj) != "list") {
        return(cc_obj)
      }
      cc_obj$summary %>%
        mutate(
          value = as.character(value),
          value = ifelse(value == 1 & parameter == "Both groups were rhythmic", TRUE, value)
        ) %>%
        kable(format = "html", escape = F) %>%
        kable_styling("striped", full_width = F)
    })

    output$plot <- renderPlot({
      if (class(cc_obj) != "list") {
        return(NULL)
      }
      cc_obj$plot
    })
  })
}

shinyApp(ui = ui, server = server)
