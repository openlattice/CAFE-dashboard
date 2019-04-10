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
                     uiOutput(ns("con_column2"))
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
            choices = get_vars(rawdata, "numeric")
        ))
        
        output$con_column2 <- renderUI(selectInput(
            inputId = ns("concol2"),
            "Choose column 2:",
            choices = get_vars(rawdata, "numeric")
        ))
        
        output$conconplot <-
            renderPlot({
                data <- get_concon_data(rawdata, input$concol1, input$concol2)
                concon_plot(data, input$concol1, input$concol2)
            }, height = 700
            )
        
        output$concontest <- 
            renderPrint({
                data <- get_concon_data(rawdata, input$concol1, input$concol2)
                concon_test(data, input$concol1, input$concol2)
            })
        
        output$conconcor <- 
            renderPrint({
                data <- get_concon_data(rawdata, input$concol1, input$concol2)
                cor(data[input$concol1], data[input$concol2], use="complete.obs")
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
# data <- get_concon_data(rawdata, col1, col2)
# concon_plot(data, col1, col2)
# concon_test(data, col1, col2)

concon_test <- function(data, col1, col2){
    if (is.null(data)){return(NULL)}
    formula <- as.formula(paste(col2, "~", col1))
    linmod <- lm(formula, data = data)
    summary(linmod)
}

concon_plot <- function(data, col1, col2){
    if (is.null(data)){return(NULL)}
    ggplot(data, aes_string(x=col1, y=col2)) + 
        geom_point(color=cols[2])+
        geom_smooth(method=lm,  color=cols[1], fill=cols[2]) + theme_light() +
        scale_fill_manual(values = cols,
                          aesthetics = "fill",
                          na.value = nacol) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

get_concon_data <- function(rawdata, col1, col2) {
    if (!rawdata$auth) {
        return(NULL)
    }
    dat1 <- get_dataset_from_col(rawdata, col1)
    dat2 <- get_dataset_from_col(rawdata, col2)
    if (dat1 == dat2){
        if(dat1 == "tud"){data = rawdata$tud$summarised} else {data = rawdata[[dat1]]$processed}
    } else {
        if(dat1 == "tud"){dataset1 = rawdata$tud$summarised} else {dataset1 = rawdata[[dat1]]$processed}
        if(dat2 == "tud"){dataset2 = rawdata$tud$summarised} else {dataset2 = rawdata[[dat2]]$processed}
        data = dataset1 %>% full_join(dataset2, by = "nc.SubjectIdentification")
        }
    return(data)
}

