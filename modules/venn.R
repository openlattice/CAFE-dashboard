###################
## UI COMPONENTS ##
###################

venn_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Data streams",
             fluidRow(column(width = 4),
                      column(
                          width = 8,
                          box(
                              width = 12,
                              solidHeader = TRUE,
                              title = "Venn-diagram for data streams",
                              addSpinner(
                                  plotOutput(ns("venn_plot"), height = 500),
                                  spin = "bounce",
                                  color = cols[1]
                              )
                          )
                      )))
}

#######################
## SERVER COMPONENTS ##
#######################

venn_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$venn_plot <- renderPlot({
            venn_plot(rawdata)
        })
        

        
        
    }
