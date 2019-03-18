###################
## UI COMPONENTS ##
###################

tud_maq_ui <- function(id) {
    ns <- NS(id)
    tabPanel("TUD - MAQ",
             
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     title = "Select TUD columns",
                     uiOutput(ns("tud_cols"))
                     
                 ),
                 box(
                     width = 12,
                     title = "Select MAQ columns",
                     selectInput(
                         inputId = ns("tud_maq_column_M"),
                         choices = c('study_id', 'employment', 'education', 'age_months', 'ethnicity', 'race'),
                         label = 'Column'
                     )
                 )
                 
             ),
             column(
                 width = 8,
                 box(
                     width = 12,
                     title = "tud maq",
                     addSpinner(plotOutput(ns("plot_maq_tud")), spin = "bounce", color = cols[1])
                 )
             )))
    
}

#######################
## SERVER COMPONENTS ##
#######################

tud_maq_base_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$tud_cols <- renderUI(selectInput(
            inputId = ns('tud_maq_column_T'),
            choices = c(
                rawdata$tud$summarised_coltypes$numeric,
                rawdata$tud$summarised_coltypes$factorial,
                rawdata$tud$summarised_coltypes$boolean
            ),
            label = 'Column'
        ))
        
        output$plot_maq_tud <-
            renderPlot({
                plot_maq(rawdata$tud$summarised,
                         rawdata$maq$processed,
                         input$tud_maq_column_T,
                         input$tud_maq_column_M)
            })
        

    }

