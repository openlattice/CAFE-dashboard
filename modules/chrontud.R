chronicle_tud <- function(id){
    ns <- NS(id)
    tabPanel("TUD + chronicle",
                          fluidRow(
                              column(
                                  width = 4,
                                  box(
                                      width = 12,
                                      title = "Select column",
                                      radioButtons(
                                          inputId = ns('tud_chron_tud'),
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
                                      addSpinner(plotOutput(ns("tud_chron_plot")),spin = "bounce", color = cols[1])
                                  ),
                                  box(
                                      width=12,
                                      solidHeader=FALSE,
                                      downloadButton(ns("tud_chron_plot_download"), "Download figure"),
                                      align = "left"
                                  )
                              )))
}

chronicle_tud_server <- function(input,
                                 output,
                                 session,
                                 summarydata,
                                 chronicle,
                                 summary_coltypes
                                 ){
    output$tud_chron_plot <-
        renderPlot({
            plot_tud_chron(
                summarydata,
                chronicle,
                "meantime",
                input$tud_chron_tud
            )
        })
    
    output$tudchronplot_download <-
        downloadHandler(
            filename = "tudchronplot.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_tud_chron(
                        summarydata,
                        chronicle,
                        "meantime",
                        input$tud_chron_chron
                    ),
                    width = 8,
                    height = 5
                )
            }
        )
    
    observe({
        updateRadioButtons(session,
                           "tud_chron_tud",
                           choices = summary_coltypes$numeric)
        
    })
    
}


plot_tud_chron <-
    function(summarydata, chronicle, var1, var2) {
        new <-
            merge(
                summarydata,
                chronicle,
                how = "inner",
                by.x = "nc.SubjectIdentification",
                by.y = "pid"
            )
        plt <- ggplot(new, aes_string(x = var1, y = var2)) +
            geom_point() + theme_light() +
            stat_smooth(method = "lm", col = cols[1]) +
            labs(x = "Chronicle: average usage per day",
                 y = paste("Tud:", var2))
        return(plt)
    }




