library(shiny)
library(shinydashboard)
library(shinyWidgets)

cols <- c("#ff3c5d",
          "#6124e2",
          "#ffe671",
          "#ff9a58",
          "#dd9e00",
          "#00be84")

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

QA_base <- tabPanel(title = "Quality Assessment",
                                fluidRow(
                                    box(
                                        width = 12,
                                        solidHeader = TRUE,
                                        title = "Quality assessment base",
                                        addSpinner(plotOutput(outputId = "qc_base"),spin = "bounce", color = cols[1])
                                    )
                               ))

preprocessed_table <- tabPanel(title = "TUD preprocessed",
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

summarised_table <- tabPanel("TUD summarised",
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

chronicle_table <- tabPanel("Chronicle",
                             fluidRow(box(
                                 width = 12,
                                 column(12, align = "center", downloadButton("download_chronicle", "Download"))
                             )),
                             fluidRow(
                                 box(
                                     width = 12,
                                     solidHeader = TRUE,
                                     title = "Chronicle data",
                                     dataTableOutput(outputId = "chronicle_raw")
                                 )
                             ))

activity_barcharts <- tabPanel("TUD activities",
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
                                              
                                 ),
                                 box(
                                     width=12,
                                     solidHeader=FALSE,
                                     downloadButton("A_hours_by_activity_grouped_download", "Download figure"),
                                     align = "left"
                                 )
                               )))


preprocessed_barcharts <- tabPanel("TUD barcharts",
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
                                     ),
                                     box(
                                         width=12,
                                         solidHeader=FALSE,
                                         downloadButton("A_activities_cross_download", "Download figure"),
                                         align = "left"
                                     )
                                   )))

summarised_histograms <- tabPanel("TUD histograms",
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
                                    ),
                                    box(
                                        width=12,
                                        solidHeader=FALSE,
                                        downloadButton("histogram_download", "Download figure"),
                                        align = "left"
                                    )
                                  )))

summarised_crossplots <- tabPanel("TUD crossplots",
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
                                    ),
                                    box(
                                        width=12,
                                        solidHeader=FALSE,
                                        downloadButton("crossplot_download", "Download figure"),
                                        align = "left"
                                    )
                                  )))

summarised_SBP <- tabPanel("TUD best practices",
                                  fluidRow(column(
                                      width = 4,
                                      box()
                                  ),
                                  column(
                                      width = 8,
                                      box(
                                          width = 12,
                                          solidHeader = TRUE,
                                          title = "Screen Best Practices",
                                          addSpinner(plotOutput("sbp_plot", height=500),spin = "bounce", color = cols[1])
                                      ),
                                      box(
                                          width=12,
                                          solidHeader=FALSE,
                                          downloadButton("sbp_download", "Download figure"),
                                          align = "left"
                                      )
                                  )))

chronicle_tud <- tabPanel("TUD + chronicle",
                                  fluidRow(
                                  column(
                                      width = 4,
                                      box(
                                          width = 12,
                                          title = "Select column",
                                          radioButtons(
                                              inputId = 'tud_chron_tud',
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
                                          title = "Cross-plot",
                                          addSpinner(plotOutput("tud_chron_plot"),spin = "bounce", color = cols[1])
                                      ),
                                      box(
                                          width=12,
                                          solidHeader=FALSE,
                                          downloadButton("tud_chron_plot_download", "Download figure"),
                                          align = "left"
                                      )
                                  )))

TUD_MAQ <- tabPanel("TUD - MAQ",
                                   fluidRow(column(
                                       width = 4,
                                       box(
                                           width = 12,
                                           title = "Select TUD columns",
                                           selectInput(
                                               inputId = 'tud_maq_column_T',
                                               choices = c('test'),
                                               label = 'Column'
                                           )
                                       ),
                                       box(
                                           width = 12,
                                           title = "Select MAQ columns",
                                           selectInput(
                                               inputId = "tud_maq_column_M",
                                               choices = c('study', 'employment', 'education'),
                                               label = 'Column'
                                           )
                                       )
                                       
                                   ),
                                   column(
                                       width = 8,
                                       box(
                                           width = 12,
                                           title = "tud maq",
                                           addSpinner(plotOutput("plot_maq_tud"),spin = "bounce", color = cols[1])
                                       )
                                   )))
