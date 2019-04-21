###################
## UI COMPONENTS ##
###################

multivariate_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Multivariate (crossplots)",
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Select columns",
                     uiOutput(ns("multivarcrosscols"))
                 )
             ),
             column(
                 width = 8,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Multivariate cross-plot",
                     addSpinner(
                         plotOutput(ns("multivariate_cross_plot"), height = 500),
                         spin = "bounce",
                         color = cols[1]
                     )
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("multivariate_cross_download"), "Download figure"),
                     align = "left"
                 )
             )))
}

#######################
## SERVER COMPONENTS ##
#######################

multivariate_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$multivarcrosscols <- renderUI(checkboxGroupInput(
            ns("multivarcrosscols"),
            "",
            choices = unlist(data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("boolean", "factorial", "numeric")), use.names=FALSE)
        ))
        
        
        output$multivariate_cross_plot <- renderPlot({
            plot_crossplot(rawdata$alldata, input$multivarcrosscols)
        })
        
        output$multivariate_cross_plot_download <-
            downloadHandler(
                filename = "bestpractices.png",
                content = function(file) {
                    ggsave(
                        file,
                        plot_crossplot(rawdata$alldata, input$multivarcrosscols),
                        width = 8,
                        height = 5
                    )
                }
            )
        
        
        
    }

plot_crossplot <- function(data, crosscols) {
    if (length(crosscols) > 0) {
        ggpairs(
            data[, crosscols],
            diag = list(continuous = wrap("densityDiag", fill = cols[1])),
            lower = list(continuous = wrap("smooth", colour = cols[2]))
        )
    }
}

