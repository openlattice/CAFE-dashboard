###################
## UI COMPONENTS ##
###################

multivariate_cor_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Multivariate (correlogram)",
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Select columns",
                     uiOutput(ns("multivarcols"))
                 )
             ),
                      column(
                          width = 8,
                          box(
                              width = 12,
                              solidHeader = TRUE,
                              title = "Multivariate correlogram",
                              addSpinner(
                                  plotOutput(ns("multivariate_plot"), height = 500),
                                  spin = "bounce",
                                  color = cols[1]
                              )
                          ),
                          box(
                              width = 12,
                              solidHeader = FALSE,
                              downloadButton(ns("multivariate_download"), "Download figure"),
                              align = "left"
                          )
                      )))
}

#######################
## SERVER COMPONENTS ##
#######################

multivariate_cor_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$multivarcols <- renderUI(checkboxGroupInput(
            ns("multivarcols"),
            "",
            choices = unlist(data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("boolean", "factorial", "numeric")), use.names=FALSE)
        ))
        
        
        output$multivariate_plot <- renderPlot({
            plot_multivariate(rawdata$alldata, input$multivarcols)
        })
        
        output$multivariate_plot_download <-
            downloadHandler(
                filename = "bestpractices.png",
                content = function(file) {
                    ggsave(
                        file,
                        plot_multivariate(rawdata$alldata, input$multivarcols),
                        width = 8,
                        height = 5
                    )
                }
            )
        
        
        
    }

plot_multivariate <- function(data, cols) {
    if (length(crosscols) > 0) {
    combined <- data[cols]
    corr <- round(cor(combined, use = "complete.obs"), 1)
    ggcorrplot(corr,
               ggtheme = theme_light(),
               colors = c('#2c7bb6',  '#ffffbf', '#d7191c'))
    }
}

