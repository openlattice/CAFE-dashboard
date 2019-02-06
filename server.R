library(shiny)
library(shinydashboard)

source("pipelines/tud_load_data.R")
source("pipelines/tud_transform_data.R")
source("pipelines/tud_summarise_data.R")
source("servercomponents/plots.R")

latticecols <- c("#cdd1db","#6124e2", "#44beff", "#bc0000")

shinyServer(function(input, output, session) {

  ###########################################
  # loading data and observing column names #
  ###########################################
  
  # load data
  rawdata <- reactive({load_data(input$jwt, local=TRUE)})
  activitydata <- reactive({
    print("processing")
    process_activities(rawdata())
  })
  
  subset_activitydata <- reactive({
    activitydata()
    if (rawdata()$auth){
      dur_by_child <- activitydata() %>%
        group_by(child_id) %>%
        summarise(duration = sum(duration) / 60) %>%
        filter(duration == 24) %>%
        select("child_id")
    activitydata() %>% filter(child_id %in% as_vector(dur_by_child))
    }
  })
  
  summarydata <- reactive({
    print("summarising")
    summarise_data(subset_activitydata())
  })
  
  
  
  activity_coltypes <- reactive({
    if (rawdata()$auth){
      list(
        numeric = activitydata() %>% select(which(sapply(., is.numeric))) %>% names,
        factorial = activitydata() %>% select(which(sapply(., is.factor))) %>% names,
        boolean = activitydata() %>% select(which(sapply(., is.logical))) %>% names
      )
    } else {
      list()
    }
  })
  
  summary_coltypes <- reactive({
    if (rawdata()$auth){
      list(
        numeric = summarydata() %>% select(which(sapply(., is.numeric))) %>% names,
        factorial = summarydata() %>% select(which(sapply(., is.factor))) %>% names,
        boolean = summarydata() %>% select(which(sapply(., is.logical))) %>% names
      )
    } else {
      list()
    }
  })
  
  observe({
    updateSelectInput(session, "barchart_columns", choices = c(activity_coltypes()$factorial, activity_coltypes()$boolean))
    updateSelectInput(session, "barchart_grouper_columns", choices = c(activity_coltypes()$factorial, activity_coltypes()$boolean))
    updateSelectInput(session, "activity_columns", choices = c(activity_coltypes()$factorial, activity_coltypes()$boolean))
  })
  
  observe({
    updateRadioButtons(session, "hist_column", choices = summary_coltypes()$numeric)
    updateCheckboxGroupInput(session, "cross_columns", choices = c(summary_coltypes()$numeric, summary_coltypes()$factorial, summary_coltypes()$boolean))
  })
  
  # authenticated
  output$auth <- reactive({rawdata()$auth})
  
  #############################
  # loading information boxes #
  #############################
  
  output$activityCounterBox <- renderInfoBox({
    valueBox(rawdata()$n_act, "activity blocks", icon = icon("heartbeat"))
  })
  output$kidsCounterBox <- renderInfoBox({
    valueBox(rawdata()$n_child, "children", icon = icon("child"))
  })
  
  output$datasetCounterBox <- renderInfoBox({
    valueBox(length(rawdata()$nodes), "entities", icon = icon("pie-chart"))
  })
  
  
  #######################
  # showing data tables #
  #######################
  
  output$preprocessed <- renderDataTable({
    activitydata()
  },
  options = list(scrollX = TRUE))
  
  output$summarised <- renderDataTable({
    summarydata()
  },
  options = list(scrollX = TRUE))
  
  #################
  # showing plots #
  #################
  
  output$A_hours_by_activity <- renderPlot({plot_hours_by_activity(subset_activitydata())})
  output$A_hours_by_activity_grouped <- renderPlot({plot_hours_by_activity(subset_activitydata(), input$activity_columns)})
  output$A_hours_total <- renderPlot({plot_total_hour_distribution(activitydata())})
  output$A_activities_cross <- renderPlot({plot_barchart_activities(subset_activitydata(), input$barchart_columns, input$barchart_grouper_columns)})
  output$histogram <- renderPlot({plot_summary_histogram(summarydata(), input$hist_column)})
  output$crossplot <- renderPlot({plot_crossplot(summarydata(), input$cross_columns)})
  
  # output$download_preprocessed <- downloadHandler(
  #   filename = "CAFE_TUD_preprocessed.csv",
  #   content = function(file) {
  #     write.csv(activitydata(), file, row.names = FALSE)
  #   }
  # )
  # 
  # output$download_summarised <- downloadHandler(
  #   filename = "CAFE_TUD_summarised.csv",
  #   content = function(file) {
  #     write.csv(summarydata(), file, row.names = FALSE)
  #   }
  # )
  
  outputOptions(output, 'auth', suspendWhenHidden = FALSE)
  
  output$histogram <- renderPlot({
    vals <- rnorm(input$vals)
    hist(vals, col = latticecols[1], border=latticecols[1])
    })
  

  
})
