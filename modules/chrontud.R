###################
## UI COMPONENTS ##
###################

chronicle_tud <- function(id){
    ns <- NS(id)
    tabPanel("TUD + chronicle",
                          fluidRow(
                              column(
                                  width = 4,
                                  box(
                                      width = 12,
                                      title = "Select column",
                                      uiOutput(ns("tud_column"))
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

#######################
## SERVER COMPONENTS ##
#######################

chronicle_tud_server <- function(input,
                                 output,
                                 session,
                                 rawdata){
    ns <- session$ns
    
    output$tud_column = renderUI(radioButtons(
        inputId = ns('tud_chron_tud'),
        choices = rawdata$tud$summarised_coltypes$numeric,
        label = 'Column'
    ))
    
    output$tud_chron_plot <-
        renderPlot({
            plot_tud_chron(
                rawdata$tud$summarised,
                rawdata$chronicle$processed,
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
                        rawdata$tud$summarised,
                        rawdata$chronicle$processed,
                        "meantime",
                        input$tud_chron_chron
                    ),
                    width = 8,
                    height = 5
                )
            }
        )
    
}



