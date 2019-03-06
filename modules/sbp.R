sbp_ui <- function(id){
    ns <- NS(id)
    tabPanel("TUD best practices",
                           fluidRow(column(
                               width = 4
                           ),
                           column(
                               width = 8,
                               box(
                                   width = 12,
                                   solidHeader = TRUE,
                                   title = "Screen Best Practices",
                                   addSpinner(plotOutput(ns("sbp_plot"), height=500),spin = "bounce", color = cols[1])
                               ),
                               box(
                                   width=12,
                                   solidHeader=FALSE,
                                   downloadButton(ns("sbp_download"), "Download figure"),
                                   align = "left"
                               )
                           )))
}  

sbp_server <-
    function(input,
             output,
             session,
             summarydata) {
        output$sbp_plot <- renderPlot({
            plot_sbp(summarydata)
        })
        
        output$sbp_plot_download <-
            downloadHandler(
                filename = "bestpractices.png",
                content = function(file) {
                    ggsave(file,
                           plot_sbp(summarydata),
                           width = 8,
                           height = 5)
                }
            )
        
        
        
    }
