###################
## UI COMPONENTS ##
###################

concon_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Bivariate (continuous)",
             fluidRow(column(
                 width = 6,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Select columns",
                     uiOutput(ns("con_column1")),
                     uiOutput(ns("con_column2")),
                     uiOutput(ns("split_by_study"))
                 ),
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Correlation",
                     verbatimTextOutput(ns("conconcor"))
                 ),
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Linear regression",
                     verbatimTextOutput(ns("concontest"))
                 )
             ),
             column(
                 width = 6,
                 box(
                     width = 12,
                     height = 1000,
                     solidHeader = TRUE,
                     title = "Scatter plot",
                     addSpinner(plotOutput(ns("conconplot")), spin = "bounce", color = cols[1])
                     # ),
                     # box(
                     #     width = 12,
                     #     solidHeader = FALSE,
                     #     downloadButton(ns("demographicsplot_download"), "Download figure"),
                     #     align = "left"
                 )
             ))#,
             # fluidRow(
             #     box(
             #         width = 12,
             #         solidHeader = TRUE,
             #         title = "Cross-table",
             #         dataTableOutput(outputId = ns("demographics_table"))
             #     )
             # )
             # fluidRow(box(
             #     width = 12,
             #     column(
             #         12,
             #         align = "center",
             #         downloadButton(ns("download_demographics_table"), "Download")
             #     )
             # )
             
             
    )
    
}

#######################
## SERVER COMPONENTS ##
#######################

concon_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$con_column1 <- renderUI(selectInput(
            inputId = ns("concol1"),
            "Choose column 1:",
            choices = data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("numeric"))
        ))
        
        output$con_column2 <- renderUI(selectInput(
            inputId = ns("concol2"),
            "Choose column 2:",
            choices = data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("numeric"))
        ))
        
        output$split_by_study <- renderUI(
            checkboxInput(
                inputId = ns("split_by_study"),
                label = "Split by study"
            ))
            
        
        output$conconplot <-
            renderPlot({
                concon_plot(rawdata$alldata, input$concol1, input$concol2, input$split_by_study)
            }, height = 700
            )
        
        output$concontest <- 
            renderPrint({
                concon_test(rawdata$alldata, input$concol1, input$concol2)
            })
        
        output$conconcor <- 
            renderPrint({
                cor(rawdata$alldata[input$concol1], rawdata$alldata[input$concol2], use="complete.obs")
            })
        
        # output$demographicsplot_download <-
        #     downloadHandler(
        #         filename = "demographics_plot",
        #         content = function(file) {
        #             ggsave(
        #                 file,
        #                 demographics_plot(data, input$demcol1, input$demcol2, input$demcol3, input$remove_missing_maq),
        #                 width = 8,
        #                 height = 5
        #             )
        #         }
        #     )
        # # 
        # output$demographics_table <- renderDataTable({
        #     data <- get_demographics_data(rawdata, input$demcol3, input$remove_missing_maq)
        #     create_table(data, input$demcol1, input$demcol2, input$demcol3, input$remove_missing_maq)
        # },
        # options = list(scrollX = TRUE))
        
        # output$download_demographics_table <- downloadHandler(
        #     filename = "CAFE_demographics_crosstable.csv",
        #     content = function(file) {
        #         write.csv(
        #             create_table(data(), input$demcol1, input$demcol2, input$demcol3, input$remove_missing_maq),
        #             file,
        #             row.names = FALSE
        #         )
        #     }
        # )
        
        
    }

# functions

# col1 <- "age_months"
# col2 <- "background_media_on_hours"
# data <- rawdata$alldata
# concon_plot(data, col1, col2, TRUE)
# concon_test(data, col1, col2)

concon_test <- function(data, col1, col2){
    if (is.null(data)){return(NULL)}
    formula <- as.formula(paste(col2, "~", col1))
    linmod <- lm(formula, data = data)
    summary(linmod)
}

concon_plot <- function(data, col1, col2, split_by_study=FALSE){
    if (is.null(data)){return(NULL)}
    if (split_by_study){
        baseplot = ggplot(data, aes_string(x=col1, y=col2, color = 'study'))
    } else {
        baseplot = ggplot(data, aes_string(x=col1, y=col2))
    }
    baseplot + geom_point(color=cols[2])+
        stat_smooth(method = "lm") + theme_light() +
        scale_fill_manual(values = cols,
                          aesthetics = "fill",
                          na.value = nacol) +
        scale_colour_manual(values = cols,
                            aesthetics = "colour",
                            na.value = nacol) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}





