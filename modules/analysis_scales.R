###################
## UI COMPONENTS ##
###################

scales_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Scales (internal consistency)",
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Select columns",
                     uiOutput(ns("scales_cols"))
                 )
             ),
             column(
                 width = 8,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Multivariate correlogram",
                     addSpinner(
                         plotOutput(ns("scales_plot"), height = 1000),
                         spin = "bounce",
                         color = cols[1]
                     )
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("scales_download"), "Download figure"),
                     align = "left"
                 )
             )))
}

#######################
## SERVER COMPONENTS ##
#######################

scales_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$scales_cols <- renderUI(selectInput(
            ns("scales_cols"),
            "",
            choices = c("PSI", "Valkenburg", "short form")
        ))
        
        
        output$scales_plot <- renderPlot({
            plot_multivariate(rawdata$alldata, get_scales_columns(rawdata, input$scales_cols))
        })
        
        output$multivariate_plot_download <-
            downloadHandler(
                filename = "scales.png",
                content = function(file) {
                    ggsave(
                        file,
                        plot_multivariate(rawdata$alldata, get_scales_columns(rawdata, input$scales_cols)),
                        width = 8,
                        height = 5
                    )
                }
            )
        
        
        
    }

plot_multivariate <- function(data, cols) {
    if (length(cols) > 0) {
        combined <- data[cols]
        corr <- round(cor(combined, use = "complete.obs"), 1)
        ggcorrplot(corr,
                   ggtheme = theme_light(),
                   colors = c('#2c7bb6',  '#ffffbf', '#d7191c'))
    }
}

