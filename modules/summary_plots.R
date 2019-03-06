## UI COMPONENTS

summarised_histograms <- function(id) {
    ns <- NS("id")
    tabPanel("TUD histograms",
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
                     addSpinner(
                         plotOutput(outputId = ns("histogram")),
                         spin = "bounce",
                         color = cols[1]
                     )
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("histogram_download"), "Download figure"),
                     align = "left"
                 )
             )))
}

summarised_crossplots <- function(id) {
    ns <- NS("id")
    tabPanel("TUD crossplots",
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
                     addSpinner(plotOutput(ns("crossplot")), spin = "bounce", color = cols[1])
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("crossplot_download"), "Download figure"),
                     align = "left"
                 )
             )))
    
}

## SERVER COMPONENTS

summary_plots <-
    function(input,
             output,
             session,
             summarydata,
             summary_coltypes
             ) {
        output$histogram <-
            renderPlot({
                plot_summary_histogram(summarydata, input$hist_column)
            })
        
        output$histogram_download <-
            downloadHandler(
                filename = "histogram.png",
                content = function(file) {
                    ggsave(
                        file,
                        plot_summary_histogram(summarydata, input$hist_column),
                        width = 8,
                        height = 5
                    )
                }
            )
        
        output$crossplot <-
            renderPlot({
                plot_crossplot(summarydata(), input$cross_columns)
            })
        
        output$crossplot_download <-
            downloadHandler(
                filename = "crossplot.png",
                content = function(file) {
                    ggsave(
                        file,
                        plot_crossplot(summarydata, input$cross_columns),
                        width = 8,
                        height = 5
                    )
                }
            )
        observe({
            updateRadioButtons(session, "hist_column", choices = summary_coltypes$numeric[summary_coltypes$numeric != "nc.SubjectIdentification"])
            updateCheckboxGroupInput(
                session,
                "cross_columns",
                choices = c(
                    summary_coltypes$numeric,
                    summary_coltypes$factorial[summary_coltypes$factorial != "nc.SubjectIdentification"],
                    summary_coltypes$boolean
                )
            )
        })
        
    }
