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
             activitydata,
             summarydata,
             chronicledata) {
        
        output$preprocessed <- renderDataTable({
            activitydata %>% filter(table_access == TRUE) %>% select(-c("nc.SubjectIdentification"))
        },
        options = list(scrollX = TRUE))
        
        output$summarised <- renderDataTable({
            summarydata %>% filter(table_access == TRUE)
        },
        options = list(scrollX = TRUE))
        
        output$chronicle <- renderDataTable({
            chronicledata$preprocessed %>% filter(table_access == TRUE)
        },
        options = list(scrollX = TRUE))
        
        output$chronicle_raw <- renderDataTable({
            chronicledata$raw %>% filter(table_access == TRUE)
        },
        options = list(scrollX = TRUE))
        
        output$download_preprocessed <- downloadHandler(
            filename = "CAFE_TUD_preprocessed.csv",
            content = function(file) {
                write.csv(
                    activitydata %>% filter(table_access == TRUE) %>% select(-c("nc.SubjectIdentification")),
                    file,
                    row.names = FALSE
                )
            }
        )
        
        output$download_summarised <- downloadHandler(
            filename = "CAFE_TUD_summarised.csv",
            content = function(file) {
                write.csv(
                    summarydata %>% filter(table_access == TRUE) %>% select(-c("nc.SubjectIdentification")),
                    file,
                    row.names = FALSE
                )
            }
        )
        
        output$download_chronicle <- downloadHandler(
            filename = "CAFE_chronicle.csv",
            content = function(file) {
                write.csv(
                    chronicledata$preprocessed %>% filter(table_access == TRUE) %>% select(-c("study", "pid")),
                    file,
                    row.names = FALSE
                )
            }
        )
        
}