activity_barcharts <- function(id) {
    ns <- NS(id)
    tabPanel("TUD activities",
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     title = "Select column",
                     selectInput(
                         inputId = ns('activity_columns'),
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
                     addSpinner(plotOutput(ns(
                         "A_hours_by_activity_grouped"
                     )), spin = "bounce", color = cols[1])
                     
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(
                         ns("A_hours_by_activity_grouped_download"),
                         "Download figure"
                     ),
                     align = "left"
                 )
             )))
}

preprocessed_barcharts <- function(id) {
    ns <- NS(id)
    tabPanel("TUD barcharts",
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     title = "Select column",
                     selectInput(
                         inputId = ns('barchart_columns'),
                         choices = c('test'),
                         label = 'Column'
                     ),
                     selectInput(
                         inputId = ns("barchart_grouper_columns"),
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
                     addSpinner(
                         plotOutput(outputId = ns("A_activities_cross")),
                         spin = "bounce",
                         color = cols[1]
                     )
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("A_activities_cross_download"), "Download figure"),
                     align = "left"
                 )
             )))
}


activity_plots <- function(input,
                           output,
                           session,
                           activitydata,
                           activity_coltypes) {
    output$A_hours_by_activity_grouped <-
        renderPlot({
            plot_hours_by_activity(activitydata, input$activity_columns)
        })
    
    output$A_hours_by_activity_grouped_download <-
        downloadHandler(
            filename = "hours_by_activity_grouped.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_hours_by_activity(activitydata, input$activity_columns),
                    width = 8,
                    height = 5
                )
            }
        )
    
    observe({
        updateSelectInput(
            session,
            "activity_columns",
            choices = c(
                activity_coltypes$factorial[!activity_coltypes$factorial %in% c("site")],
                activity_coltypes$boolean
            )
        )
        updateSelectInput(
            session,
            "barchart_columns",
            choices = c(
                activity_coltypes$factorial[!activity_coltypes$factorial %in% c("site")],
                activity_coltypes$boolean
            )
        )
        updateSelectInput(
            session,
            "barchart_grouper_columns",
            choices = c(
                activity_coltypes$factorial[!activity_coltypes$factorial %in% c("site")],
                activity_coltypes$boolean
            )
        )
    })
    
    output$A_activities_cross <-
        renderPlot({
            plot_barchart_activities(activitydata,
                                     input$barchart_columns,
                                     input$barchart_grouper_columns)
        })
    
    output$A_activities_cross_download <-
        downloadHandler(
            filename = "activities_cross.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_barchart_activities(
                        activitydata,
                        input$barchart_columns,
                        input$barchart_grouper_columns
                    ),
                    width = 8,
                    height = 5
                )
            }
        )
    
    
}