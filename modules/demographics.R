###################
## UI COMPONENTS ##
###################

demographics_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Demographics",
             fluidRow(column(
                 width = 4,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Select columns",
                     uiOutput(ns("remove_missing_maq")),
                     uiOutput(ns("dem_column1")),
                     uiOutput(ns("dem_column2")),
                     uiOutput(ns("dem_column3"))
                 )
             ),
             column(
                 width = 8,
                 box(
                     width = 12,
                     height = 1000,
                     solidHeader = TRUE,
                     title = "Demographics",
                     addSpinner(plotOutput(ns("demographicsplot")), spin = "bounce", color = cols[1])
                 # ),
                 # box(
                 #     width = 12,
                 #     solidHeader = FALSE,
                 #     downloadButton(ns("demographicsplot_download"), "Download figure"),
                 #     align = "left"
                 )
             )),
             fluidRow(
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Cross-table",
                     dataTableOutput(outputId = ns("demographics_table"))
                 )
             )
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

demographics_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
         output$dem_column1 <- renderUI(selectInput(
            inputId = ns("demcol1"),
            "Choose column 1:",
            choices = get_demographics(rawdata)
        ))
        
        output$dem_column2 <- renderUI(selectInput(
            inputId = ns("demcol2"),
            "Choose column 2:",
            choices = c("", get_demographics(rawdata))
        ))
        
        output$dem_column3 <- renderUI(selectInput(
            inputId = ns("demcol3"),
            "Choose column 3:",
            choices = get_vars(rawdata)
        ))
        
        output$remove_missing_maq <- renderUI(checkboxInput(
            inputId = ns("remove_missing_maq"),
            "Include children with missing MAQ (missing demography variables):",
            value=TRUE
        ))
        
        output$demographicsplot <-
            renderPlot({
                data <- get_demographics_data(rawdata, input$demcol3, input$remove_missing_maq)
                demographics_plot(data, input$demcol1, input$demcol2, input$demcol3, input$remove_missing_maq)
            }, height = 700
            )
        
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
        # 
        output$demographics_table <- renderDataTable({
            data <- get_demographics_data(rawdata, input$demcol3, input$remove_missing_maq)
            create_table(data, input$demcol1, input$demcol2, input$demcol3, input$remove_missing_maq)
        },
        options = list(scrollX = TRUE))
        
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

# function

# col1 <- "ethnicity"
# col2 <- "education"
# col3 <- "sf_Q1_mediahours_weekend"
# col3 <- "background_tv_hours"
# remove_missing_maq <- TRUE
# data <- get_demographics_data(rawdata, col3, remove_missing_maq)

create_table <- function(data, col1, col2, col3, remove_missing_maq=TRUE){
    if (is.null(data)){return(NA)}
    if (col2 == ""){
        base <- data %>% select(col1, col3) %>% 
            group_by_(col1)
    } else {
    base <- data %>% select(col1, col2, col3) %>% 
        group_by_(col1, col2)
    }
    stats <- base  %>% 
        summarise(
            mean = mean(!!sym(col3), na.rm=TRUE),
            sd = sd(!!sym(col3), na.rm=TRUE),
            count = n(),
            na = sum(is.na(!!sym(col3)), na.rm=TRUE)
        )
    return(stats)
}

# col1 <- "ethnicity"
# col2 <- "race"
# col3 <- "n"
# remove_missing_maq <- TRUE
# data <- get_demographics_data(rawdata, col3, remove_missing_maq)
# demographics_plot(data, col1, col2, col3, remove_missing_maq)


demographics_plot <- function(data, col1, col2, col3, remove_missing_maq=FALSE) {
    
    if (is.null(data)){return(NULL)}
    if (col3 == "n") {
        plot = ggplot(data,
                      aes_string(x = col1,
                                 fill = col2)) + 
            geom_bar(position=position_dodge())
        
    } else {
        if (col2=="") {
            plot = ggplot(data,
                          aes_string(x = col1,
                                     y = col3
                          ))
        } else {
            plot = ggplot(data,
                          aes_string(x = col1,
                                     fill = col2,
                                     y = col3
                          ))
        }
        plot <- plot + geom_bar(
            stat="summary", fun.y = "mean",
            position=position_dodge())
    }
        
    plot + theme_light() +
        scale_fill_manual(values = cols,
                          aesthetics = "fill",
                          na.value = nacol) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    
}


get_demographics_data <- function(rawdata, column, remove_missing_maq) {
    if (column != "n"){
        if (!rawdata$auth) {
            return(NULL)
        }
        dataset <- get_dataset_from_col(rawdata, column)
        if (dataset == "maq"){
            data <- rawdata$maq$processed
            return(data)
        }
    }
    if (remove_missing_maq) {
        data <- rawdata$maq$processed %>% full_join(rawdata$tud$summarised, 
                                                    by="nc.SubjectIdentification")
        return(data)
    }
    data <- rawdata$maq$processed %>% left_join(rawdata$tud$summarised, 
                                                by="nc.SubjectIdentification")
    return(data)
}

