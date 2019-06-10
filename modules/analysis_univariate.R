###################
## UI COMPONENTS ##
###################

univar_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Univariate (continuous)",
             fluidRow(column(
                 width = 6,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Select column",
                     uiOutput(ns("con_column"))
                 )
             ),
             column(
                 width = 6,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Univeriate statistics",
                     verbatimTextOutput(ns("con_text"))
                 ),
                 box(
                     width = 12,
                     height = 1000,
                     solidHeader = TRUE,
                     title = "Histogram",
                     addSpinner(plotOutput(ns("histogramplot")), spin = "bounce", color = cols[1])
                     ),
                     box(
                         width = 12,
                         solidHeader = FALSE,
                         downloadButton(ns("histogramplot_download"), "Download figure"),
                         align = "left"
                 )
             ))
             
             
    )
    
}

#######################
## SERVER COMPONENTS ##
#######################

univar_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$con_column <- renderUI(selectInput(
            inputId = ns("concol"),
            "Choose column:",
            choices = c(data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("numeric", "factor", "boolean")), other = c("n"))
        ))
        
        output$con_text <-
            renderPrint({
                print(summary(rawdata$alldata[input$concol]))
            })
        
        output$histogramplot <-
            renderPlot({
                plot_histogram(rawdata$alldata, input$concol)
            }, height = 700
            )
        

        output$histogramplot_download <-
            downloadHandler(
                filename = "histogram_plot",
                content = function(file) {
                    ggsave(
                        file,
                        plot_histogram(rawdata$alldata, input$concol),
                        width = 8,
                        height = 5
                    )
                }
            )

        
    }

# functions

plot_histogram <- function(data, column) {
    if (column %in% names(data)) {
        if (class(data[[column]]) == "factor" | class(data[[column]]) == "logical"){
            ggplot(data,
                   aes_string(x = column)) +
                geom_bar(fill = "#4c14c4") +
                scale_fill_manual(values = cols,
                                  aesthetics = "fill",
                                  na.value = nacol)+ theme_light()
        } else {
        ggplot(data,
               aes_string(x = column)) +
            geom_histogram(binwidth = 1,
                           fill = "#4c14c4") +
            scale_fill_manual(values = cols,
                              aesthetics = "fill",
                              na.value = nacol)+ theme_light()
        }
    }
}
