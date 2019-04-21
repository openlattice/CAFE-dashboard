## UI FUNCTIONS

preprocessed_table <- function(id){
    ns <- NS(id)
    tabPanel(title = "TUD preprocessed",
                               fluidRow(box(
                                   width = 12,
                                   column(
                                       12,
                                       align = "center",
                                       downloadButton(ns("download_preprocessed"), "Download")
                                   )
                               )),
                               fluidRow(
                                   box(
                                       width = 12,
                                       solidHeader = TRUE,
                                       title = "Preprocessed data",
                                       dataTableOutput(outputId = ns("preprocessed"))
                                   )
                               ))
}

summarised_table <- function(id){
    ns <- NS(id)
    tabPanel("TUD summarised",
                             fluidRow(box(
                                 width = 12,
                                 column(12, align = "center", downloadButton(ns("download_summarised"), "Download"))
                             )),
                             fluidRow(
                                 box(
                                     width = 12,
                                     solidHeader = TRUE,
                                     title = "Summarised data",
                                     dataTableOutput(outputId = ns("summarised"))
                                 )
                             ))
}

maq_table <- function(id){
    ns <- NS(id)
    tabPanel("MAQ",
             fluidRow(box(
                 width = 12,
                 column(12, align = "center", downloadButton(ns("download_maq"), "Download"))
             )),
             fluidRow(
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "MAQs data",
                     dataTableOutput(outputId = ns("maq"))
                 )
             ))
}

chronicle_table <- function(id){
    ns <- NS(id)
    tabPanel("Chronicle",
                            fluidRow(box(
                                width = 12,
                                column(12, align = "center", downloadButton(ns("download_chronicle"), "Download"))
                            )),
                            fluidRow(
                                box(
                                    width = 12,
                                    solidHeader = TRUE,
                                    title = "Chronicle data",
                                    dataTableOutput(outputId = ns("chronicle_raw"))
                                )
                            ))
}

# SERVER FUNCTIONS

tables <-
    function(input,
             output,
             session,
             rawdata) {
        
        output$preprocessed <- renderDataTable({
            rawdata$tud$processed %>% filter(table_access == TRUE) #%>% select(-c("nc.SubjectIdentification"))
        },
        options = list(scrollX = TRUE))
        
        output$maq <- renderDataTable({
            rawdata$maq$processed %>% filter(table_access == TRUE) #%>% select(-c("nc.SubjectIdentification"))
        },
        options = list(scrollX = TRUE))
        
        output$summarised <- renderDataTable({
            rawdata$tud$summarised %>% filter(table_access == TRUE)# %>% select(-c("nc.SubjectIdentification"))
        },
        options = list(scrollX = TRUE))
        
        output$chronicle_raw <- renderDataTable({
            rawdata$chronicle$raw %>% filter(table_access == TRUE) #%>% select(-c("pid"))
        },
        options = list(scrollX = TRUE))
        
        output$download_preprocessed <- downloadHandler(
            filename = "CAFE_TUD_preprocessed.csv",
            content = function(file) {
                write.csv(
                    rawdata$tud$processed %>% filter(table_access == TRUE), #%>% select(-c("nc.SubjectIdentification")),
                    file,
                    row.names = FALSE
                )
            }
        )
        
        output$download_maq <- downloadHandler(
            filename = "CAFE_MAQ.csv",
            content = function(file) {
                write.csv(
                    rawdata$maq$processed %>% filter(table_access == TRUE), #%>% select(-c("nc.SubjectIdentification")),
                    file,
                    row.names = FALSE
                )
            }
        )
        
        output$download_summarised <- downloadHandler(
            filename = "CAFE_TUD_summarised.csv",
            content = function(file) {
                write.csv(
                    rawdata$tud$summarised %>% filter(table_access == TRUE), #%>% select(-c("nc.SubjectIdentification")),
                    file,
                    row.names = FALSE
                )
            }
        )
        
        output$download_chronicle <- downloadHandler(
            filename = "CAFE_chronicle.csv",
            content = function(file) {
                write.csv(
                    rawdata$chronicle$raw %>% filter(table_access == TRUE), #%>% select(-c("pid")),
                    file,
                    row.names = FALSE
                )
            }
        )
        
}