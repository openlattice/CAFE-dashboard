library(shiny)
library(shinydashboard)
library(GGally)

source("pipelines/load_data.R")
source("pipelines/transform_data.R")
source("pipelines/summarise_data.R")


shinyServer(function(input, output, session) {

  cols <- c("#6124e2", "#ff3c5d", "#ffe671", "#ff9a58", "#dd9e00", "#00be84")
  nacol <- "#dcdce7"


  rawdata <- reactive({load_data(input$jwt)})

  activitydata <- reactive({process_activities(rawdata())})
  actcols <- reactive({activitydata() %>% names()})
  observe({updateSelectInput(session, "barchart_columns", choices = actcols())})
  observe({updateSelectInput(session, "barchart_grouper_columns", choices = actcols())})

  summarydata <- reactive({summarise_data(activitydata())})
  numcols <- reactive({
    if (!(dim(summarydata())[1]== 0)){
      summarydata() %>% select(which(sapply(., is.numeric))) %>% names()
    }
    })
  observe({updateRadioButtons(session, "hist_column", choices = numcols())})
  observe({updateCheckboxGroupInput(session, "cross_columns", choices = numcols())})

  output$auth <- reactive({rawdata()$auth})

  output$activityCounterBox <- renderInfoBox({
    valueBox(rawdata()$n_act, "activity blocks", color = "purple")
  })
  output$kidsCounterBox <- renderInfoBox({
    valueBox(rawdata()$n_child, "children", color = "purple")
  })

  output$datasetCounterBox <- renderInfoBox({
    valueBox(length(rawdata()$nodes), "entities", color = "purple")
  })

 output$preprocessed <- renderDataTable({
   activitydata()
  },
  options = list(scrollX = TRUE)
  )

  output$summarised <- renderDataTable({
    summarydata()
  },
  options = list(scrollX = TRUE)
  )

  output$barplot <- renderPlot({
    if ((typeof(activitydata()[[input$barchart_columns]]) == "integer" | typeof(activitydata()[[input$barchart_columns]]) == "logical") &
        (typeof(activitydata()[[input$barchart_grouper_columns]]) == "integer" | typeof(activitydata()[[input$barchart_grouper_columns]]) == "logical")) {
      ggplot(activitydata(), aes_string(x = input$barchart_columns, fill=input$barchart_grouper_columns)) +
        geom_bar() + theme_light() +
        scale_fill_manual(values = cols, aesthetics = "fill", na.value = nacol)
    }
  })

  output$histogram <- renderPlot({
    ggplot(
      summarydata(),
      aes_string(x = input$hist_column)
    ) +
      geom_histogram(
        binwidth = 1,
        fill = "#4c14c4"
        ) +
      scale_fill_manual(values = cols, aesthetics = "fill", na.value = nacol)

  })

  output$crossplot <- renderPlot({
    if (length(input$cross_columns) > 0){
      ggpairs(
        summarydata()[,input$cross_columns],
        color = "black",
        diag = list(continuous = wrap("densityDiag", fill = cols[1])),
        lower = list(continuous=wrap("smooth", colour=cols[2]))
      )
    }
  })

  output$download_preprocessed <- downloadHandler(
    filename = "CAFE_TUD_preprocessed.csv",
    content = function(file) {
      write.csv(activitydata(), file, row.names = FALSE)
    }
  )

  output$download_summarised <- downloadHandler(
    filename = "CAFE_TUD_summarised.csv",
    content = function(file) {
      write.csv(summarydata(), file, row.names = FALSE)
    }
  )

  outputOptions(output, 'auth', suspendWhenHidden = FALSE)
})
