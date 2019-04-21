###################
## UI COMPONENTS ##
###################

tud_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Activities (passive sensing)",
             fluidRow(column(
                 width = 4,
                 box(width = 12,
                     title = "Select column",
                     uiOutput(ns("activity_columns")))
             ),
             column(
                 width = 8,
                 box(
                     title = "Average duration of activities per child.",
                     width = 12,
                     solidHeader = TRUE,
                     status = "primary",
                     addSpinner(plotOutput(ns("act_grouped")), spin = "bounce", color = cols[1])
                     
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("act_grouped_download"),
                                    "Download figure"),
                     align = "left"
                 )
             )))
}


#######################
## SERVER COMPONENTS ##
#######################

tud_server <- function(input,
                           output,
                           session,
                           rawdata) {
    ns <- session$ns
    
    output$activity_columns = renderUI({
        print(names(rawdata))
        print(names(rawdata$coltypes))
        selectInput(
        inputId = ns('activity_columns'),
        choices = c(
            rawdata$coltypes$tud_activity$factorial[!rawdata$tud$processed_coltypes$factorial %in% c("site")],
            rawdata$coltypes$tud_activity$boolean
        ),
        label = 'Column'
    )})
    
    output$act_grouped <-
        renderPlot({
            plot_hours_by_activity(rawdata$tud$processed, input$activity_columns)
        })
    
    output$act_grouped_download <-
        downloadHandler(
            filename = "hours_by_activity_grouped.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_hours_by_activity(rawdata$tud$processed, input$activity_columns),
                    width = 8,
                    height = 5
                )
            }
        )

    
}