###################
## UI COMPONENTS ##
###################

sbp_ui <- function(id) {
    ns <- NS(id)
    tabPanel("TUD best practices",
             fluidRow(column(width = 4),
                      column(
                          width = 8,
                          box(
                              width = 12,
                              solidHeader = TRUE,
                              title = "Screen Best Practices",
                              addSpinner(
                                  plotOutput(ns("sbp_plot"), height = 500),
                                  spin = "bounce",
                                  color = cols[1]
                              )
                          ),
                          box(
                              width = 12,
                              solidHeader = FALSE,
                              downloadButton(ns("sbp_download"), "Download figure"),
                              align = "left"
                          )
                      )))
}

#######################
## SERVER COMPONENTS ##
#######################

sbp_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        output$sbp_plot <- renderPlot({
            plot_sbp(rawdata$tud$summarised)
        })
        
        output$sbp_plot_download <-
            downloadHandler(
                filename = "bestpractices.png",
                content = function(file) {
                    ggsave(
                        file,
                        plot_sbp(rawdata$tud$summarised),
                        width = 8,
                        height = 5
                    )
                }
            )
        
        
        
    }
