library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(yaml)

source("pipelines/tud_load_data.R")
source("pipelines/tud_transform_data.R")
source("pipelines/tud_summarise_data.R")
source("servercomponents/plots.R")


read_yaml("secrets.yaml")

cols <- c("#ff3c5d",
    "#6124e2",
    "#ffe671",
    "#ff9a58",
    "#dd9e00",
    "#00be84")

shinyServer(function(input, output, session) {
  
  ###########################################
  # loading data and observing column names #
  ###########################################
  
  # load data
  rawdata <-
    eventReactive(input$login, {
      load_data(input$jwt)
    }, ignoreNULL=FALSE)
  
  activitydata <- reactive({
    print("processing")
    process_activities(rawdata())
  })

  subset_activitydata <- reactive({
    if (rawdata()$auth) {
      dur_by_child <- activitydata() %>%
        group_by(unique_day) %>%
        summarise(duration = sum(duration) / 60) %>%
        filter(duration > 18 & duration < 26) %>%
        select("child_id")
      activitydata() %>% filter(child_id %in% as_vector(dur_by_child))
    }
  })
  
  summarydata <- reactive({
      if (rawdata()$auth) {
      print("summarising")
      summarise_data(subset_activitydata())
    }
    })
  
  
  activity_coltypes <- reactive({
    if (rawdata()$auth) {
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
    if (rawdata()$auth) {
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
    updateSelectInput(
      session,
      "barchart_columns",
      choices = c(
        activity_coltypes()$factorial,
        activity_coltypes()$boolean
      )
    )
    updateSelectInput(
      session,
      "barchart_grouper_columns",
      choices = c(
        activity_coltypes()$factorial,
        activity_coltypes()$boolean
      )
    )
    updateSelectInput(
      session,
      "activity_columns",
      choices = c(
        activity_coltypes()$factorial,
        activity_coltypes()$boolean
      )
    )
  })
  
  observe({
    updateRadioButtons(session, "hist_column", choices = summary_coltypes()$numeric)
    updateCheckboxGroupInput(
      session,
      "cross_columns",
      choices = c(
        summary_coltypes()$numeric,
        summary_coltypes()$factorial,
        summary_coltypes()$boolean
      )
    )
  })
  
  # authenticated
  output$auth <- reactive({
    rawdata()$auth
  })
  
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
  
  
  #######################################
  # showing and downloading data tables #
  #######################################
  
  output$preprocessed <- renderDataTable({
    activitydata() %>% filter(table_access==TRUE) %>% select(-c("nc.SubjectIdentification"))
  },
  options = list(scrollX = TRUE))
  
  output$summarised <- renderDataTable({
    summarydata() %>% filter(table_access==TRUE)
  },
  options = list(scrollX = TRUE))

  output$download_preprocessed <- downloadHandler(
      filename = "CAFE_TUD_preprocessed.csv",
      content = function(file) {
          write.csv(activitydata() %>% filter(table_access==TRUE) %>% select(-c("nc.SubjectIdentification")), file, row.names = FALSE)
      }
  )
  
  output$download_summarised <- downloadHandler(
      filename = "CAFE_TUD_summarised.csv",
      content = function(file) {
          write.csv(summarydata() %>% filter(table_access==TRUE) %>% select(-c("nc.SubjectIdentification")), file, row.names = FALSE)
      }
  )
  
  #################
  # showing plots #
  #################
  
  output$A_hours_by_activity <-
    renderPlot({
      plot_hours_by_activity(subset_activitydata())
    })
  
  output$A_hours_by_activity_download <- 
      downloadHandler(
          filename = "hours_by_activity.png",
          content = function(file) {
              ggsave(file, plot_hours_by_activity(subset_activitydata()), width=8,height=5)
          })
  
  output$A_hours_by_activity_grouped <-
    renderPlot({
      plot_hours_by_activity(subset_activitydata(), input$activity_columns)
    })
  
  output$A_hours_by_activity_grouped_download <- 
      downloadHandler(
          filename = "hours_by_activity_grouped.png",
          content = function(file) {
              ggsave(file, plot_hours_by_activity(subset_activitydata(), input$activity_columns), width=8,height=5)
          })
  
  output$A_hours_total <-
    renderPlot({
      plot_total_hour_distribution(activitydata())
    })

  output$A_hours_total_download <- 
      downloadHandler(
          filename = "hours_total.png",
          content = function(file) {
              ggsave(file, plot_total_hour_distribution(activitydata()), width=8,height=5)
          })
  
  
  output$A_activities_cross <-
    renderPlot({
      plot_barchart_activities(
        subset_activitydata(),
        input$barchart_columns,
        input$barchart_grouper_columns
      )
    })

  output$A_activities_cross_download <- 
      downloadHandler(
          filename = "activities_cross.png",
          content = function(file) {
              ggsave(file, plot_barchart_activities(
                  subset_activitydata(),
                  input$barchart_columns,
                  input$barchart_grouper_columns
              ), width=8,height=5)
          })
  
  output$histogram <-
    renderPlot({
      plot_summary_histogram(summarydata(), input$hist_column)
    })

  output$histogram_download <- 
      downloadHandler(
          filename = "histogram.png",
          content = function(file) {
              ggsave(file, plot_summary_histogram(summarydata(), input$hist_column), width=8,height=5)
          })

    output$crossplot <-
    renderPlot({
      plot_crossplot(summarydata(), input$cross_columns)
    })
  
    output$crossplot_download <- 
        downloadHandler(
            filename = "crossplot.png",
            content = function(file) {
                ggsave(file, plot_crossplot(summarydata(), input$cross_columns), width=8,height=5)
            })
    
    output$emptyplot <- 
    renderPlot({
      data <- rawdata()
      empty_plot()
    })
  
  
  outputOptions(output, 'auth', suspendWhenHidden = FALSE)

})
