tud_maq_ui <- function(id) {
    ns <- NS(id)
    tabPanel("TUD - MAQ",
             
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     title = "Select TUD columns",
                     selectInput(
                         inputId = ns('tud_maq_column_T'),
                         choices = c('test'),
                         label = 'Column'
                     )
                 ),
                 box(
                     width = 12,
                     title = "Select MAQ columns",
                     selectInput(
                         inputId = ns("tud_maq_column_M"),
                         choices = c('study', 'employment', 'education'),
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

tud_maq_base_server <-
    function(input,
             output,
             session,
             summarydata,
             maqdata,
             summary_coltypes) {
        output$plot_maq_tud <-
            renderPlot({
                plot_maq(summarydata,
                         maqdata,
                         input$tud_maq_column_T,
                         input$tud_maq_column_M)
            })
        
        observe({
            updateSelectInput(
                session,
                "tud_maq_column_T",
                choices = c(
                    summary_coltypes$numeric,
                    summary_coltypes$factorial[summary_coltypes$factorial != "nc.SubjectIdentification"],
                    summary_coltypes$boolean
                )
            )
        })
        
    }



# FUNCTIONS

plot_maq <- function(summarydata, maqdata, tudcol, maqcol) {
    togdata <- summarydata %>% inner_join(maqdata)
    plt <- ggplot(togdata, aes_string(x = maqcol, y = tudcol)) +
        theme_light() +
        geom_bar(stat = "summary",
                 fun.y = "mean",
                 fill = cols[5]) +
        coord_flip()
    return(plt)
}
