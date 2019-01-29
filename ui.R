library(shiny)
library(shinydashboard)
library(GGally)

dashboardPage(

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
