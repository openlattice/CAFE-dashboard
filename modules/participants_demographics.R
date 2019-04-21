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
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("demographicsplot_download"), "Download figure"),
                     align = "left"
                 )
             )),
             fluidRow(
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Cross-table",
                     dataTableOutput(outputId = ns("demographics_table"))
                 ),
             box(
                width = 12,
                column(
                    12,
                    align = "center",
                    downloadButton(ns("download_demographics_table"), "Download")
                )
            )
            
    
             ))
    
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
            "Choose demography variable:",
            choices = get_demographics(rawdata)
        ))
        
        output$dem_column2 <- renderUI(selectInput(
            inputId = ns("demcol2"),
            "Choose demography variable for colors or leave blank:",
            choices = c("", get_demographics(rawdata))
        ))
        
        output$dem_column3 <- renderUI(selectInput(
            inputId = ns("demcol3"),
            "Choose continuous variable:",
            choices = c(data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("numeric")), other = c("n"))
        ))
        

        output$demographicsplot <-
            renderPlot({
                demographics_plot(rawdata$alldata, input$demcol1, input$demcol2, input$demcol3)
            }, height = 700
            )
        
        output$demographicsplot_download <-
            downloadHandler(
                filename = "demographics_plot.png",
                content = function(file) {
                    plt <- demographics_plot(rawdata$alldata, input$demcol1, input$demcol2, input$demcol3)
                    ggsave(
                        file,
                        plt,
                        width = 8,
                        height = 5
                    )
                }
            )

        output$demographics_table <- renderDataTable({
            create_dem_table(rawdata$alldata, input$demcol1, input$demcol2, input$demcol3, input$remove_missing_maq)
        },
        options = list(scrollX = TRUE))
        
        output$download_demographics_table <- downloadHandler(
            filename = "CAFE_demographics_crosstable.csv",
             content = function(file) {
                 tbl <- create_dem_table(rawdata$alldata, input$demcol1, input$demcol2, input$demcol3, input$remove_missing_maq)
                 write.csv(
                    tbl,
                    file,
                    row.names = FALSE
                )
            }
        )
        
        
    }

# function

# col1 <- "race"
# col2 <- "education"
# col3 <- "sf_Q1_mediahours_weekend"
# col3 <- "background_tv_hours"
# remove_missing_maq <- TRUE

create_dem_table <- function(data, col1, col2, col3, remove_missing_maq=TRUE){
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

# col1 <- "race"
# col2 <- "study"
# col3 <- "n"
# remove_missing_maq <- TRUE
# demographics_plot(rawdata$alldata, col1, col2, col3)


demographics_plot <- function(data, col1, col2, col3) {
    
    if (is.null(data)){return(NULL)}
    if (col3 == "n") {
        if (col2=="") {
            plot = ggplot(data,
                          aes_string(x = col1)) + 
                geom_bar(position=position_dodge())
        } else {
        plot = ggplot(data,
                      aes_string(x = col1,
                                 fill = col2)) + 
            geom_bar(position=position_dodge())
        }
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
