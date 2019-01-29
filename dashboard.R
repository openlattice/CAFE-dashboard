library(shiny)
library(shinydashboard)
library(GGally)
library(openapi)
library(knitr)
library(httr)

source("pipelines/load_data.R")
source("pipelines/transform_data.R")
source("pipelines/summarise_data.R")

if (interactive()) {
  
  ui <- dashboardPage(
  
    skin = "black",
    
    dashboardHeader(
      title = tags$a(href="http://www.openlattice.com", div(img(src='logo.png',height=30)))
      ),
    
    dashboardSidebar(
      sidebarSearchForm(textId = "jwt", buttonId = "authButton",
                        label = "Enter your jwt-token."),
      conditionalPanel(
        condition = "output.auth==1",
        sidebarMenu(
          menuItem("Home", tabName = "home"),
          menuItem("Activity level data", tabName = "preprocessed"),
          menuItem("Activity level charts", startExpanded = TRUE,
            menuSubItem("Barcharts", tabName = "barcharts")
          ),
          menuItem("Child level data", tabName = "summarised"),
          menuItem("Child level charts", startExpanded = TRUE,
            menuSubItem("Histograms", tabName = "histograms"),
            menuSubItem("Cross-Plots", tabName = "cross-plots")
          )
        )
      )
    ),
    
    dashboardBody(
      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="custom.css")
        ),
      tabItems(
        tabItem(
          'home',
          fluidRow(
            box(
              title = "Welcome",
              width = 12,
              background = "purple"
            ),
            valueBoxOutput( 'activityCounterBox'),
            valueBoxOutput( 'datasetCounterBox'),
            valueBoxOutput( 'kidsCounterBox')
          )
        ),
        tabItem(
          "preprocessed",
          fluidRow(
            box(
              width = 12,
              column(12, align="center", downloadButton("download_preprocessed", "Download"))
            )
          ),
          fluidRow(
            box(
              width = 12, solidHeader = TRUE,
              title = "Preprocessed data",
              dataTableOutput(outputId = "preprocessed")
            )
          )
        ),
        
        tabItem(
          "summarised",
          fluidRow(
            box(
              width = 12, 
              column(12, align="center", downloadButton("download_summarised", "Download"))
            )
          ),
          fluidRow(
            box(
              width = 12, solidHeader = TRUE,
              title = "Summarised data",
              dataTableOutput(outputId = "summarised")
            )
          )
        ),
  
        tabItem(
          "barcharts",
          fluidRow(
            column(
              width = 4,
              box(
                width = 12,
                title = "Select column",
                selectInput(inputId = 'barchart_columns', choices = c('test'), label='Column'),
                selectInput(inputId = "barchart_grouper_columns", choices = c('test'), label='Column')
              )
            ),
            column(
              width = 8,
              box(
                width = 12, solidHeader = TRUE,
                title = "Barplot",
                plotOutput(outputId = "barplot")
              )
            )
          )
        ),

                tabItem(
          "histograms",
          fluidRow(
            column(
              width = 4,
              box(
                width = 12,
                title = "Select column",
                radioButtons(inputId = 'hist_column', choices = c('test'), label='Column')
              )
            ),
            column(
              width = 8,
               box(
                width = 12, solidHeader = TRUE,
                title = "Histogram",
                plotOutput(outputId = "histogram")
              )
            )
          )
        ),
        
        
        tabItem(
          "cross-plots",
          fluidRow(
            column(
              width = 4,
              box(
                width = 12,solidHeader = TRUE,
                title = "Select column",
                checkboxGroupInput(
                  "cross_columns", 
                  "Choose columns:",
                  choices = c("test")
                )
              )
            ),
            column(
              width = 8,
              box(
                width = 12, solidHeader = TRUE,
                title = "Cross-plot",
                plotOutput("crossplot")
              )
            )
          )
        )
        
      )
    )
  )
  
  server <- function(input, output, session) {
    
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
  }
}
shinyApp(ui, server)
