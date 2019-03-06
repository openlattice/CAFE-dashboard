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


qa_server <- function(input, output, session, summarydata) {
    output$qc_base <-
        renderPlot({
            qa_plot(summarydata)
        })
}

# FUNCTIONS

qa_plot <- function(summarydata) {
    plt <- ggplot(summarydata, aes(x = site, y = progress)) +
        theme_light() +
        geom_bar(stat = "summary",
                 fun.y = "mean",
                 fill = cols[4])
    return(plt)
}
