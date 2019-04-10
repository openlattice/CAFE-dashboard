###################
## UI COMPONENTS ##
###################

summarised_histograms <- function(id) {
    ns <- NS(id)
    tabPanel("TUD histograms",
             fluidRow(column(
                 width = 4,
                 box(width = 12,
                     title = "Select column",
                     uiOutput(ns("hist_column")))
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
    ns <- NS(id)
    tabPanel("TUD crossplots",
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Select column",
                     uiOutput(ns("cross_columns"))
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
             )))}
    
    summarised_site <- function(id) {
        ns <- NS(id)
        tabPanel("TUD by site",
                 fluidRow(column(
                     width = 4,
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Select columns",
                         uiOutput(ns("site_column1")),
                         uiOutput(ns("site_column2"))
                     )
                 ),
                 column(
                     width = 8,
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Time use diary by site",
                         addSpinner(plotOutput(ns("siteplot")), spin = "bounce", color = cols[1])
                     ),
                     box(
                         width = 12,
                         solidHeader = FALSE,
                         downloadButton(ns("siteplot_download"), "Download figure"),
                         align = "left"
                     )
                 )))
    
        }

#######################
## SERVER COMPONENTS ##
#######################

summary_plots <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns

        output$hist_column <- renderUI(
            radioButtons(
                inputId = ns('histcol'),
                choices = rawdata$tud$coltypes$numeric[rawdata$tud$coltypes$numeric != "nc.SubjectIdentification"],
                label = 'Column'
            )
        )
        
        output$histogram <-
            renderPlot({
                plot_summary_histogram(rawdata$tud$summarised, input$histcol)
            })
        
        output$histogram_download <-
            downloadHandler(
                filename = "histogram.png",
                content = function(file) {
                    ggsave(
                        file,
                        plot_summary_histogram(rawdata$tud$summarised, input$histcol),
                        width = 8,
                        height = 5
                    )
                }
            )
        
        output$cross_columns <- renderUI(checkboxGroupInput(
            ns("crosscol"),
            "Choose columns:",
            choices = c(
                rawdata$tud$coltypes$numeric,
                rawdata$tud$coltypes$factorial[rawdata$tud$coltypes$factorial != "nc.SubjectIdentification"],
                rawdata$tud$coltypes$boolean
            )
        ))

        output$crossplot <-
            renderPlot({
                plot_crossplot(rawdata$tud$summarised, input$crosscol)
            })
        
        output$crossplot_download <-
            downloadHandler(
                filename = "crossplot.png",
                content = function(file) {
                    ggsave(
                        file,
                        plot_crossplot(rawdata$tud$summarised, input$crosscol),
                        width = 8,
                        height = 5
                    )
                }
            )

        output$site_column1 <- renderUI(selectInput(
            ns("sitecol1"),
            "Choose column 1:",
            choices = c(
                rawdata$tud$coltypes$numeric
            )
        ))
        
        output$site_column2 <- renderUI(selectInput(
            ns("sitecol2"),
            "Choose column 2:",
            choices = c(
                rawdata$tud$coltypes$numeric
            )
        ))
        
        output$siteplot <-
            renderPlot({
                plot_by_study(rawdata$tud$summarised, input$sitecol1, input$sitecol2)
            })
        
        output$siteplot_download <-
            downloadHandler(
                filename = "siteplot",
                content = function(file) {
                    ggsave(
                        file,
                        plot_by_study(rawdata$tud$summarised, input$sitecol1, input$sitecol2),
                        width = 8,
                        height = 5
                    )
                }
            )
        
    }
