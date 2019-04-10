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
                 ))
             )
    )
    
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
            "Choose column 1:",
            choices = c(get_vars(rawdata, "boolean"), get_vars(rawdata, "factorial")),
            selected = "race"
        ))
        
        output$cat_column2 <- renderUI(selectInput(
            inputId = ns("catcol2"),
            "Choose column 2 (or leave blank):",
            choices = c("", get_vars(rawdata, "boolean"), get_vars(rawdata, "factorial")),
            selected = ""
        ))
        
        output$cont_column3 <- renderUI(selectInput(
            inputId = ns("catcol3"),
            "Choose column 3:",
            choices = get_vars(rawdata, "numeric"),
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
                data <- get_catcon_data(rawdata, input$catcol1, input$catcol2, input$catcol3)
                catcon_plot(data, input$catcol1, input$catcol2, input$catcol3, input$figtype)
            }, height = 700
            )
        
        output$catcontest <- 
            renderPrint({
                data <- get_catcon_data(rawdata, input$catcol1, input$catcol2, input$catcol3)
                catcon_test(data, input$catcol1, input$catcol2, input$catcol3)
            })
        
        output$catconanova <- 
            renderPrint({
                data <- get_catcon_data(rawdata, input$catcol1, input$catcol2, input$catcol3)
                catcon_anova(data, input$catcol1, input$catcol2, input$catcol3)
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

# col1 <- "race"
# col2 <- "ethnicity"
# col3 <- "screen_hours"
# data <- get_catcon_data(rawdata, col1, col2, col3)
# catcon_plot(data, col1, col2, col3)
# catcon_test(data, col1, col2, col3)

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

catcon_plot <- function(data, col1, col2, col3, figtype){
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

get_catcon_data <- function(rawdata, col1, col2, col3) {
    if (!rawdata$auth) {
        return(NULL)
    }
    dat1 <- get_dataset_from_col(rawdata, col1)
    if(dat1 == "tud"){dataset1 = rawdata$tud$summarised} else {data = rawdata[[dat1]]$processed}

    if (col2 != ""){
        dat2 <- get_dataset_from_col(rawdata, col2)
        if(dat2 == "tud"){dataset2 = rawdata$tud$summarised} else {dataset2 = rawdata[[dat2]]$processed}
        data = data %>% full_join(dataset2)
    }

    dat3 <- get_dataset_from_col(rawdata, col3)
    if(dat3 == "tud"){dataset3 = rawdata$tud$summarised} else {dataset3 = rawdata[[dat3]]$processed}
    data = data %>% full_join(dataset3)
    return(data)
}

