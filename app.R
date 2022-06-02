library(shiny)
library(kableExtra)
library(knitr)
library(tidyverse)
library(circacompare)

ui <-
  shinyUI(fluidPage(
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
        tags$hr(),
        uiOutput("time"),
        uiOutput("group"),
        uiOutput("outcome"),
        tags$hr(),
        uiOutput("ui.action")
      ),
      mainPanel(
        plotOutput("plot"),
        tableOutput("contents")
      )
    )
  ))

server <- function(input, output, session) {
  filedata <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })

  output$time <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }
    items <- names(df)
    names(items) <- items
    selectInput("time", "Select the TIME (INDEPENDENT) variable from:", items)
  })

  output$group <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }
    items <- names(df)
    names(items) <- items
    selectInput("group", "Select the GROUPING variable from:", items)
  })

  output$outcome <- renderUI({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    }
    items <- names(df)
    names(items) <- items
    selectInput("outcome", "Select the OUTCOME (DEPENDENT) variable from:", items)
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

    output$contents <- renderText({ # works with renderPrint()
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


  output$ui.action <- renderUI({
    if (is.null(input$file1)) {
      return()
    }
    actionButton("action", "Run")
  })
}

shinyApp(ui = ui, server = server)
