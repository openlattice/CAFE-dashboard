library(shiny)
library(shinydashboard)
library(shinyWidgets)

home <- tabPanel(
  title = 'home',
  fluidRow(
    box(
      width = 4,
      solidHeader =  FALSE,
      status = "primary",
      HTML('<center><img src="cafe-logo.png" width="300px"></center>')
    ),
    box(
      title = "CAFE analytics dashboard",
      width = 4,
      solidHeader =  TRUE,
      status = "primary",
      'This application shows results from the \"Children and Screens\" study.  \
      To load the data, enter your jwt-token in the input above. \
      You can find your jwt-token',
      tags$a(href = "https://openlattice.com/gallery/", "here"),
      'in your account settings after logging in.',
      br(),
      br(),
      br(),
      column(
        textInput(inputId = "jwt",
                  label = "Enter your jwt-token."),
        width = 8
      ),
      column(
        actionButton(inputId = "login",
                     icon("sign-in")),
        width = 4,
        style = "padding-top: 25px"
      )
    ),
    box(
      width = 4,
      solidHeader =  FALSE,
      status = "primary",
      column(addSpinner(
        plotOutput("emptyplot", height = "200px"),
        spin = "bounce",
        color = cols[1]
      ),
      width = 12)
    )
  ),
  fluidRow(
    valueBoxOutput('activityCounterBox'),
    valueBoxOutput('datasetCounterBox'),
    valueBoxOutput('kidsCounterBox')
  ),
  conditionalPanel(condition = "output.auth==1",
                   fluidRow(
                     box(
                       title = "Average duration of activities per child.",
                       width = 12,
                       solidHeader = TRUE,
                       status = "primary",
                       plotOutput("A_hours_by_activity")
                     )
                   ),
                   fluidRow(
                     box(
                       title = "Total duration of measurements per child.",
                       width = 6,
                       solidHeader = TRUE,
                       status = "primary",
                       plotOutput("A_hours_total")
                     )
                   ))
)

preprocessed_table <- tabPanel("preprocessed_table",
                               fluidRow(box(
                                 width = 12,
                                 column(
                                   12,
                                   align = "center",
                                   downloadButton("download_preprocessed", "Download")
                                 )
                               )),
                               fluidRow(
                                 box(
                                   width = 12,
                                   solidHeader = TRUE,
                                   title = "Preprocessed data",
                                   dataTableOutput(outputId = "preprocessed")
                                 )
                               ))

summarised_table <- tabPanel("summarised_table",
                             fluidRow(box(
                               width = 12,
                               column(12, align = "center", downloadButton("download_summarised", "Download"))
                             )),
                             fluidRow(
                               box(
                                 width = 12,
                                 solidHeader = TRUE,
                                 title = "Summarised data",
                                 dataTableOutput(outputId = "summarised")
                               )
                             ))

activity_barcharts <- tabPanel("activity_barcharts",
                               fluidRow(column(
                                 width = 4,
                                 box(
                                   width = 12,
                                   title = "Select column",
                                   selectInput(
                                     inputId = 'activity_columns',
                                     choices = c('test'),
                                     label = 'Column'
                                   )
                                 )
                               ),
                               column(
                                 width = 8,
                                 box(
                                   title = "Average duration of activities per child.",
                                   width = 12,
                                   solidHeader = TRUE,
                                   status = "primary",
                                   addSpinner(plotOutput("A_hours_by_activity_grouped"),spin = "bounce", color = cols[1])
                                              
                                 )
                               )))


preprocessed_barcharts <- tabPanel("preprocessed_barcharts",
                                   fluidRow(column(
                                     width = 4,
                                     box(
                                       width = 12,
                                       title = "Select column",
                                       selectInput(
                                         inputId = 'barchart_columns',
                                         choices = c('test'),
                                         label = 'Column'
                                       ),
                                       selectInput(
                                         inputId = "barchart_grouper_columns",
                                         choices = c('test'),
                                         label = 'Column'
                                       )
                                     )
                                   ),
                                   column(
                                     width = 8,
                                     box(
                                       width = 12,
                                       solidHeader = TRUE,
                                       title = "Barplot",
                                       addSpinner(plotOutput(outputId = "A_activities_cross"),spin = "bounce", color = cols[1])
                                     )
                                   )))

summarised_histograms <- tabPanel("summarised_histograms",
                                  fluidRow(column(
                                    width = 4,
                                    box(
                                      width = 12,
                                      title = "Select column",
                                      radioButtons(
                                        inputId = 'hist_column',
                                        choices = c('test'),
                                        label = 'Column'
                                      )
                                    )
                                  ),
                                  column(
                                    width = 8,
                                    box(
                                      width = 12,
                                      solidHeader = TRUE,
                                      title = "Histogram",
                                      addSpinner(plotOutput(outputId = "histogram"),spin = "bounce", color = cols[1])
                                    )
                                  )))

summarised_crossplots <- tabPanel("summarised_crossplots",
                                  fluidRow(column(
                                    width = 4,
                                    box(
                                      width = 12,
                                      solidHeader = TRUE,
                                      title = "Select column",
                                      checkboxGroupInput("cross_columns",
                                                         "Choose columns:",
                                                         choices = c("test"))
                                    )
                                  ),
                                  column(
                                    width = 8,
                                    box(
                                      width = 12,
                                      solidHeader = TRUE,
                                      title = "Cross-plot",
                                      addSpinner(plotOutput("crossplot"),spin = "bounce", color = cols[1])
                                    )
                                  )))
