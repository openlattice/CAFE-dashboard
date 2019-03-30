###################
## UI COMPONENTS ##
###################

activity_barcharts_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Activities",
             fluidRow(column(
                 width = 4,
                 box(width = 12,
                     title = "Select column",
                     uiOutput(ns("act_cols")))
             ),
             column(
                 width = 8,
                 box(
                     title = "Average duration of activities per child.",
                     width = 12,
                     solidHeader = TRUE,
                     status = "primary",
                     addSpinner(plotOutput(ns("act_grouped")), spin = "bounce", color = cols[1])
                     
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("act_grouped_download"),
                                    "Download figure"),
                     align = "left"
                 )
             )))
}

preprocessed_barcharts_ui <- function(id) {
    ns <- NS(id)
    tabPanel("TUD barcharts",
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     title = "Select column",
                     uiOutput(ns("barchart_cols")),
                     uiOutput(ns("barchart_grouper_cols"))
                 )
             ),
             column(
                 width = 8,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Barplot",
                     addSpinner(
                         plotOutput(outputId = ns("barchart")),
                         spin = "bounce",
                         color = cols[1]
                     )
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("barchart_download"), "Download figure"),
                     align = "left"
                 )
             )))
}

#######################
## SERVER COMPONENTS ##
#######################

activity_plots_server <- function(input,
                           output,
                           session,
                           rawdata) {
    ns <- session$ns
    
    output$act_cols = renderUI(selectInput(
        inputId = ns('activity_columns'),
        choices = c(
            rawdata$tud$processed_coltypes$factorial[!rawdata$tud$processed_coltypes$factorial %in% c("site")],
            rawdata$tud$processed_coltypes$boolean
        ),
        label = 'Column'
    ))
    
    output$act_grouped <-
        renderPlot({
            plot_hours_by_activity(rawdata$tud$processed, input$activity_columns)
        })
    
    output$act_grouped_download <-
        downloadHandler(
            filename = "hours_by_activity_grouped.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_hours_by_activity(rawdata$tud$processed, input$activity_columns),
                    width = 8,
                    height = 5
                )
            }
        )
    
    output$barchart_cols <- renderUI(selectInput(
        inputId = ns("barchart_cols"),
        choices = c(
            rawdata$tud$processed_coltypes$factorial[!rawdata$tud$processed_coltypes$factorial %in% c("site")],
            rawdata$tud$processed_coltypes$boolean
        ),
        label = 'Column'
    ))
    
    output$barchart_grouper_cols <- renderUI(selectInput(
        inputId = ns("barchart_grouper_cols"),
        choices = c(
            rawdata$tud$processed_coltypes$factorial[!rawdata$tud$processed_coltypes$factorial %in% c("site")],
            rawdata$tud$processed_coltypes$boolean
        ),
        label = 'Column'
    ))
    
    output$barchart <-
        renderPlot({
            plot_barchart_activities(
                rawdata$tud$processed,
                input$barchart_cols,
                input$barchart_grouper_cols
            )
        })
    
    output$barchart_download <-
        downloadHandler(
            filename = "activities_cross.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_barchart_activities(
                        rawdata$tud$processed,
                        input$barchart_columns,
                        input$barchart_grouper_columns
                    ),
                    width = 8,
                    height = 5
                )
            }
        )
    
    
}