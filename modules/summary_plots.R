###################
## UI COMPONENTS ##
###################

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
