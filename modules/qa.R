###################
## UI COMPONENTS ##
###################

qa_ui <- function(id) {
    ns <- NS(id)
    tabPanel(title = "Quality Assessment",
             fluidRow(
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Quality assessment base",
                     addSpinner(
                         plotOutput(outputId = ns("qc_base")),
                         spin = "bounce",
                         color = cols[1]
                     )
                 )
             ))
}


#######################
## SERVER COMPONENTS ##
#######################

qa_server <- function(input, output, session, rawdata) {
    ns <- session$ns
    output$qc_base <-
        renderPlot({
            qa_plot(rawdata$tud$summarised)
        })
}

