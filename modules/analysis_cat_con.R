###################
## UI COMPONENTS ##
###################

catcon_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Bivariate (categorical - continuous)",
             fluidRow(column(
                 width = 6,
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Select columns",
                     uiOutput(ns("cat_column1")),
                     uiOutput(ns("cat_column2")),
                     uiOutput(ns("cont_column3")),
                     uiOutput(ns("figtype"))
                 ),
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Linear regression",
                     verbatimTextOutput(ns("catcontest"))
                 ),
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Anova",
                     verbatimTextOutput(ns("catconanova"))
                 )
             ),
             column(
                 width = 6,
                 box(
                     width = 12,
                     height = 1000,
                     solidHeader = TRUE,
                     title = "BarChart",
                     addSpinner(plotOutput(ns("catconplot")), spin = "bounce", color = cols[1])
                 ),
                 box(
                     width = 12,
                     solidHeader = FALSE,
                     downloadButton(ns("catconplot_download"), "Download figure"),
                     align = "left"
                 )
            )),
             fluidRow(
                 box(
                     width = 12,
                     solidHeader = TRUE,
                     title = "Cross-table",
                     dataTableOutput(outputId = ns("catcon_table"))
                 ),
                 box(
                     width = 12,
                     column(
                         12,
                         align = "center",
                         downloadButton(ns("download_catcon_table"), "Download")
                     )
                 
             )
    ))
    
}

#######################
## SERVER COMPONENTS ##
#######################

catcon_server <-
    function(input,
             output,
             session,
             rawdata) {
        ns <- session$ns
        
        output$cat_column1 <- renderUI(selectInput(
            inputId = ns("catcol1"),
            "Choose categorical column 1:",
            choices = data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("boolean", "factorial")),
            selected = "race"
        ))
        
        output$cat_column2 <- renderUI(selectInput(
            inputId = ns("catcol2"),
            "Choose categorical column 2 (or leave blank):",
            choices = data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("boolean", "factorial")),
            selected = ""
        ))
        
        output$cont_column3 <- renderUI(selectInput(
            inputId = ns("catcol3"),
            "Choose column y:",
            choices = data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types=c("numeric")),
            selected = "screen_hours"
        ))
        
        output$figtype <- renderUI(selectInput(
            inputId = ns("figtype"),
            "Choose figure type:",
            choices = c("boxplot", "violinplot"),
            selected = "boxplot"
        ))
        
        output$catconplot <-
            renderPlot({
                catcon_plot(rawdata$alldata, input$catcol1, input$catcol2, input$catcol3, input$figtype)
            }, height = 700
            )
        
        output$catcontest <- 
            renderPrint({
                catcon_test(rawdata$alldata, input$catcol1, input$catcol2, input$catcol3)
            })
        
        output$catconanova <- 
            renderPrint({
                catcon_anova(rawdata$alldata, input$catcol1, input$catcol2, input$catcol3)
            })
        
        output$catconplot_download <-
            downloadHandler(
                filename = "catcon_plot",
                content = function(file) {
                    plt <- catcon_plot(rawdata$alldata, input$catcol1, input$catcol2, input$catcol3, input$figtype)
                    ggsave(
                        file,
                        plt,
                        width = 8,
                        height = 5
                    )
                }
            )
        #
        output$catcon_table <- renderDataTable({
            create_con_table(rawdata$alldata, input$catcol1, input$catcol2, input$catcol3)
        },
        options = list(scrollX = TRUE))

        output$download_catcon_table <- downloadHandler(
            filename = "CAFE_demographics_crosstable.csv",
            content = function(file) {
                tbl <- create_con_table(rawdata$alldata, input$catcol1, input$catcol2, input$catcol3)
                write.csv(
                    data,
                    file,
                    row.names = FALSE
                )
            }
        )
        
        
    }

# functions

# col1 <- "race"
# col2 <- "ethnicity"
# col3 <- "screen_hours"
# data <- get_data_from_cols(rawdata, c(col1, col2, col3))
# catcon_plot(data, col1, col2, col3)
# catcon_test(data, col1, col2, col3)
# create_con_table(data, col1, col2, col3)

catcon_test <- function(data, col1, col2, col3){
    if (is.null(data)){return(NULL)}
    if (col2 == ""){
        formula = as.formula(paste(col3, "~", col1))
    } else {
        formula = as.formula(paste(col3, "~", col1, "+", col2))
    }
    linmod <- lm(formula, data = data)
    summary(linmod)
}

catcon_anova <- function(data, col1, col2, col3){
    if (is.null(data)){return(NULL)}
    if (col2 == ""){
        formula = as.formula(paste(col3, "~", col1))
    } else {
        formula = as.formula(paste(col3, "~", col1, "+", col2))
    }
    aovmod <- aov(formula, data = data)
    summary(aovmod)
}

catcon_plot <- function(data, col1, col2, col3, figtype = "boxplot"){
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
    
    if (figtype == "boxplot") {
        plot = plot + geom_boxplot(outlier.size=1, outlier.alpha=0.5, lwd=0.2)
    } else {
        plot = plot + geom_violin()
    }
    
    plot + theme_light() +
    scale_fill_manual(values = cols,
                      aesthetics = "fill",
                      na.value = nacol) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

create_con_table <- function(data, col1, col2, col3){
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
            na = sum(is.na(!!sym(col3)), na.rm=TRUE),
            se = sd/(count-na)
        )
    return(stats)
}

