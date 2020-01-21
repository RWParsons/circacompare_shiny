# rsconnect::deployApp("C:/Users/Rex Parsons/Documents/R_projects/misc/proc_nlin/circacompare")

# remove.packages("circacompare")
# .rs.restartR()
# devtools::install_github("RWParsons/circacompare")
library(shiny)
library(ggplot2)
library(kableExtra)
library(knitr)
library(tidyverse)

circacompare <- function(x,
                         col_time,
                         col_group,
                         col_outcome,
                         period = 24,
                         alpha_threshold = 0.05,
                         timeout_n = 10000){
  
  if(!"ggplot2" %in% installed.packages()[, "Package"]){
    return(paste("Please install 'ggplot2'"))
  }
  
  library(ggplot2)
  colnames(x)[grep(col_group, colnames(x))] <- "group"
  
  if(length(levels(as.factor(x$group))) != 2){
    return(paste("Your grouping variable had more or less than 2 levels! \nThis function is used to compare two groups of data. \nTo avoid me having to guess, please send data with only two possible values in your grouping variable to this function."))
  }
  
  group_1_text <- levels(as.factor(x$group))[1]
  group_2_text <- levels(as.factor(x$group))[2]
  colnames(x)[grep(col_time, colnames(x))] <- "time"
  
  if(!class(x$time) %in% c("numeric", "integer")){
    return(paste(paste("The time variable which you gave was a '",
                       class(x$time),
                       "' \nThis function expects time to be given as hours and be of class 'integer' or 'numeric'.",
                       "\nPlease convert the time variable in your dataframe to be of one of these classes",
                       sep = "")))
  }
  
  colnames(x)[grep(col_outcome, colnames(x))] <- "measure"
  
  if(!class(x$measure) %in% c("numeric", "integer")){
    return(paste(paste("The measure variable which you gave was a '",
                       class(x$measure),
                       "' \nThis function expects measure to be number and be of class 'integer' or 'numeric'.",
                       "\nPlease convert the measure variable in your dataframe to be of one of these classes",
                       sep = "")))
  }
  
  x$time_r <- (x$time/24)*2*pi*(24/period)
  x$x_group <- ifelse(x$group == group_1_text, 0, 1)
  
  comparison_model_success <- 0
  comparison_model_timeout <- FALSE
  g1_success <- 0
  g2_success <- 0
  g1_alpha_p <- NA
  g2_alpha_p <- NA
  n <- 0
  dat_group_1 <- x[x$group == group_1_text,]
  dat_group_2 <- x[x$group == group_2_text,]
  
  while(g1_success !=1){
    g1_alpha_start <- (max(dat_group_1$measure, na.rm = TRUE) - min(dat_group_1$measure, na.rm = TRUE)) * runif(1)
    g1_phi_start <- runif(1)*6.15 - 3.15
    g1_k_start <- mean(dat_group_1$measure, na.rm = TRUE)*2*runif(1)
    
    fit.nls_group_1 <- try({nls(measure~k + alpha*cos(time_r-phi),
                                data = dat_group_1,
                                start = list(k=g1_k_start,alpha=g1_alpha_start,phi=g1_phi_start))},
                           silent = TRUE)
    if(class(fit.nls_group_1) == "try-error"){
      n <- n + 1
    }
    else{
      g1_k_out <- summary(fit.nls_group_1)$coef[1,1]
      g1_alpha_out <- summary(fit.nls_group_1)$coef[2,1]
      g1_alpha_p <- summary(fit.nls_group_1)$coef[2,4]
      g1_phi_out <- summary(fit.nls_group_1)$coef[3,1]
      g1_success <- ifelse(g1_alpha_out > 0,1,0)
      n <- n + 1
    }
    if(n >= timeout_n){
      return(paste("Failed to converge group 1 data prior to timeout. \nYou may try to increase the allowed attempts before timeout by increasing the value of the 'timeout_n' argument or setting a new seed before this function.\nIf you have repeated difficulties, please contact me (via github) or Oliver Rawashdeh (contact details in manuscript)."))
    }
  }
  n <- 0
  while(g2_success !=1){
    g2_alpha_start <- (max(dat_group_2$measure, na.rm = TRUE) - min(dat_group_2$measure, na.rm = TRUE)) * runif(1)
    g2_phi_start <- runif(1)*6.15 - 3.15
    g2_k_start <- mean(dat_group_2$measure, na.rm = TRUE)*2*runif(1)
    
    fit.nls_group_2 <- try({nls(measure~k + alpha*cos(time_r-phi),
                                data = dat_group_2,
                                start = list(k=g2_k_start,alpha=g2_alpha_start,phi=g2_phi_start))},
                           silent = TRUE)
    if(class(fit.nls_group_2) == "try-error"){
      n <- n + 1
    }
    else{
      g2_k_out <- summary(fit.nls_group_2)$coef[1,1]
      g2_alpha_out <- summary(fit.nls_group_2)$coef[2,1]
      g2_alpha_p <- summary(fit.nls_group_2)$coef[2,4]
      g2_phi_out <- summary(fit.nls_group_2)$coef[3,1]
      g2_success <- ifelse(g2_alpha_out > 0,1,0)
      n <- n + 1
    }
    if(n >= timeout_n){
      return(paste("Failed to converge group 2 data prior to timeout. \nYou may try to increase the allowed attempts before timeout by increasing the value of the 'timeout_n' argument or setting a new seed before this function.\nIf you have repeated difficulties, please contact me (via github) or Oliver Rawashdeh (contact details in manuscript)."))
    }
  }
  
  g1_rhythmic <- ifelse(g1_alpha_p < alpha_threshold, TRUE, FALSE)
  g2_rhythmic <- ifelse(g2_alpha_p < alpha_threshold, TRUE, FALSE)
  both_groups_rhythmic <- ifelse(g1_rhythmic ==TRUE & g2_rhythmic==TRUE, TRUE, FALSE)
  
  if(both_groups_rhythmic==FALSE){
    if(g1_rhythmic == FALSE & g2_rhythmic == FALSE){
      return(paste("Both groups of data were arrhythmic (to the power specified by the argument 'alpha_threshold').\nThe data was, therefore, not used for a comparison between the two groups."))
    }
    if(g1_rhythmic == FALSE){
      return(paste(group_1_text, " was arrhythmic (to the power specified by the argument 'alpha_threshold').\nThe data was, therefore, not used for a comparison between the two groups."))
    }else{
      return(paste(group_2_text, " was arrhythmic (to the power specified by the argument 'alpha_threshold').\nThe data was, therefore, not used for a comparison between the two groups."))
    }
  }
  n <- 0
  if(both_groups_rhythmic == TRUE){
    while(comparison_model_success == 0 & comparison_model_timeout == FALSE){
      alpha_in <- g1_alpha_out*2*runif(1)
      alpha1_in <- (g2_alpha_out - g1_alpha_out)*2*runif(1)
      phi_in <- g1_phi_out*2*runif(1)
      phi1_in <- runif(1)*2*pi - pi
      k_in <- g1_k_out*2*runif(1)
      k1_in <- (g2_k_out - g1_k_out)*2*runif(1)
      
      fit.nls <- try({nls(measure~k+k1*x_group+(alpha+alpha1*x_group)*cos(time_r-(phi+phi1*x_group)),
                          data = x,
                          start = list(k=k_in, k1=k1_in, alpha=alpha_in, alpha1=alpha1_in, phi=phi_in, phi1=phi1_in),
                          nls.control(maxiter = 100, minFactor = 1/10000#, warnOnly = TRUE
                          ))},
                     silent = TRUE)
      
      if (class(fit.nls) == "try-error") {
        n <- n + 1
      }
      else{
        k_out <- coef(fit.nls)[1]
        k1_out <- coef(fit.nls)[2]
        k_out_p <- (summary(fit.nls)$coef)[1,4]
        k1_out_p <- (summary(fit.nls)$coef)[2,4]
        
        alpha_out <- coef(fit.nls)[3]
        alpha1_out <- coef(fit.nls)[4]
        alpha1_out_p <- (summary(fit.nls)$coef)[4,4]
        phi_out <- coef(fit.nls)[5]
        phi1_out <- coef(fit.nls)[6]
        phi1_out_p <- (summary(fit.nls)$coef)[6,4]
        
        comparison_model_success <- ifelse(alpha_out>0 & (alpha_out + alpha1_out) > 0 & phi1_out <pi & phi1_out >-pi, 1, 0)
        comparison_model_timeout <- ifelse(n>timeout_n, TRUE, FALSE)
        n <- n + 1
      }
    }
    
    if(comparison_model_timeout == TRUE){
      return(paste("Both groups of data were rhythmic but the curve fitting procedure failed due to timing out. \nYou may try to increase the allowed attempts before timeout by increasing the value of the 'timeout_n' argument or setting a new seed before this function.\nIf you have repeated difficulties, please contact me (via github) or Oliver Rawashdeh (contact details in manuscript)."))
    }
    #loop curve fitting process (all data) until outputs are appropriate, or until looped more times than timeout_n
    if(comparison_model_timeout == FALSE){
      eq_1 <- function(time){k_out + alpha_out*cos((2*pi/period)*time - phi_out)}
      eq_2 <- function(time){k_out + k1_out + (alpha_out + alpha1_out)*cos((2*pi/period)*time - (phi_out + phi1_out))}
      
      fig_out <- ggplot2::ggplot(x, aes(time, measure)) +
        stat_function(fun = eq_1, colour = "blue", size=1) +
        stat_function(fun = eq_2, colour = "red", size=1) +
        geom_point(aes(colour = group)) +
        scale_colour_manual(breaks = c(group_1_text, group_2_text),
                            values = c("blue", "red")) +
        xlab("time (hours)") +
        xlim(min(floor(x$time/period) * period),
             max(ceiling(x$time/period) * period))
      
    }#if the nls was successful, create a graph to plot the data as well as curves of best fit, 'fig_out'
  }
  if(both_groups_rhythmic==TRUE & comparison_model_success==1){
    if(phi_out > pi){
      while(phi_out > pi){
        phi_out <- phi_out - 2*pi
      }
    }
    if(phi_out < -pi){
      while(phi_out < -pi){
        phi_out <- phi_out + 2*pi
      }
    }#adjust phi_out so that -pi < phi_out < pi
    baseline_diff_abs <- k1_out
    baseline_diff_pc <- ((k_out + k1_out)/k_out)*100 - 100
    amplitude_diff_abs <- alpha1_out
    amplitude_diff_pc <-  ((alpha_out+alpha1_out)/alpha_out)*100 - 100
    g1_peak_time <- phi_out*24/(2*pi)
    g2_peak_time <- (phi_out+phi1_out)*24/(2*pi)
    while(g1_peak_time >24 | g1_peak_time < 0){
      if(g1_peak_time >24){
        g1_peak_time <- g1_peak_time - 24
      }
      if(g1_peak_time<0){
        g1_peak_time <- g1_peak_time + 24
      }
    }
    while(g2_peak_time >24| g2_peak_time <0){
      if(g2_peak_time>24){
        g2_peak_time <- g2_peak_time - 24
      }
      if(g2_peak_time<0){
        g2_peak_time <- g2_peak_time + 24
      }
    }
    peak_time_diff <- phi1_out*24/(2*pi)
  }
  
  output_parms <- data.frame(parameter = c("Both groups were rhythmic",
                                           paste("Presence of rhythmicity (p-value) for ", group_1_text, sep = ""),
                                           paste("Presence of rhythmicity (p-value) for ", group_2_text, sep = ""),
                                           paste(group_1_text, " mesor estimate", sep = ""),
                                           paste(group_2_text, " mesor estimate", sep = ""),
                                           "Mesor difference estimate",
                                           "P-value for mesor difference",
                                           paste(group_1_text, " amplitude estimate", sep = ""),
                                           paste(group_2_text, " amplitude estimate", sep = ""),
                                           "Amplitude difference estimate",
                                           "P-value for amplitude difference",
                                           paste(group_1_text, " peak time", sep = ""),
                                           paste(group_2_text, " peak time", sep = ""),
                                           "Phase difference estimate",
                                           "P-value for difference in phase"),
                             value = c(both_groups_rhythmic, g1_alpha_p, g2_alpha_p, k_out, (k_out + k1_out), k1_out,
                                       k1_out_p, alpha_out, alpha_out + alpha1_out, alpha1_out, alpha1_out_p,
                                       g1_peak_time, g2_peak_time, peak_time_diff, phi1_out_p))
  
  
  if(exists("fig_out")){
    return(list(fig_out, output_parms, fit.nls))
  }
}

# ui
ui <-
  shinyUI(fluidPage(
    titlePanel("CircaCompare"),
    sidebarLayout(
      sidebarPanel(
        fileInput('file1', 'Choose CSV File',
                  accept=c('text/csv',
                           'text/comma-separated-values,text/plain',
                           '.csv')),

        tags$hr(),
        uiOutput("time"),
        uiOutput("group"),
        uiOutput("outcome"),
        tags$hr(),
        uiOutput('ui.action') # instead of conditionalPanel
      ),
      mainPanel(
        tableOutput('contents'),
        plotOutput("plot")
      )
    )
  ))

# server
server <-
  shinyServer(function(input, output) {

    filedata <- reactive({
      infile <- input$file1
      if (is.null(infile)){
        return(NULL)
      }
      read.csv(infile$datapath)
    })

    output$time <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("time","Select the TIME (INDEPENDENT) variable from:",items)
    })

    output$group <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("group","Select the GROUPING variable from:",items)
    })

    output$outcome <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("outcome","Select the OUTCOME (DEPENDENT) variable from:",items)
    })

    observeEvent(input$action, {
      output$contents <- renderText({  # works with renderPrint()
      input$action
      isolate({
        df <- filedata()
        if (is.null(df)) return(NULL)
        if (class(circacompare(df,
                               col_time = input$time,
                               col_group = input$group,
                               col_outcome = input$outcome))
            != 'list') return(circacompare(df,
                                           col_time = input$time,
                                           col_group = input$group,
                                           col_outcome = input$outcome))
        circacompare(df,
                     col_time = input$time,
                     col_group = input$group,
                     col_outcome = input$outcome)[[2]] %>%
          mutate(value = as.character(value),
                 value = ifelse(value == 1 & parameter == "Both groups were rhythmic", TRUE, value)) %>%
          knitr::kable(format = "html", escape = F) %>%
          kable_styling("striped", full_width = F)
        })
      })
    })

    observeEvent(input$action, {
      output$plot <- renderPlot({
        input$action
        isolate({
          df <- filedata()
          if (is.null(df)) return(NULL)
          circacompare(df,
                       col_time = input$time,
                       col_group = input$group,
                       col_outcome = input$outcome)[[1]]
        })
      })
    })


    output$ui.action <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("action", "Run")
    })

  })

# run app
shinyApp(ui = ui, server = server)
