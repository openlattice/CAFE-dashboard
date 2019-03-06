shinyServer(function(input, output, session) {
    
    ###########################################
    # loading data and observing column names #
    ###########################################
    
    hide(selector = "#navbar li a[data-value=tables]")
    hide(selector = "#navbar li a[data-value=plots]")
    hide(selector = "#navbar li a[data-value=QA]")
    hide(selector = "#navbar li a[data-value=TUD-MAQ]")
    
    # load data
    rawdata <-
        eventReactive(input$login, {
            print("reading...")
            rawdata <- get_data(input$jwt, cache = TRUE, auth = TRUE)
            rawdata
        }, ignoreNULL = FALSE)
    
    activitydata <- reactive({
        print("processing TUD...")
        process_activities(rawdata())
    })
    
    maqdata <- reactive({
        print("processing MAQ...")
        process_maq(rawdata())
    })
    
    summarydata <- reactive({
        print("summarising...")
        summarise_data(activitydata())
    })
    
    observe({
        if (rawdata()$auth) {
            shinyjs::show(selector = "#navbar li a[data-value=tables]")
            shinyjs::show(selector = "#navbar li a[data-value=plots]")
            shinyjs::show(selector = "#navbar li a[data-value=QA]")
            shinyjs::show(selector = "#navbar li a[data-value=TUD-MAQ]")
        }
    })
    
    activity_coltypes <- reactive({
        if (rawdata()$auth) {
            list(
                numeric = activitydata() %>% select(which(sapply(
                    ., is.numeric
                ))) %>% names,
                factorial = activitydata() %>% select(which(sapply(
                    ., is.factor
                ))) %>% names,
                boolean = activitydata() %>% select(which(sapply(
                    ., is.logical
                ))) %>% names
            )
        } else {
            list()
        }
    })
    
    summary_coltypes <- reactive({
        if (rawdata()$auth) {
            list(
                numeric = summarydata() %>% select(which(sapply(
                    ., is.numeric
                ))) %>% names,
                factorial = summarydata() %>% select(which(sapply(
                    ., is.factor
                ))) %>% names,
                boolean = summarydata() %>% select(which(sapply(
                    ., is.logical
                ))) %>% names
            )
        } else {
            list()
        }
    })
    
    # authenticated
    output$auth <- reactive({
        rawdata()$auth
    })
    
    #############################
    # loading information boxes #
    #############################
    
    output$activityCounterBox <- renderInfoBox({
        valueBox(rawdata()$n_act,
                 "activity blocks",
                 icon = icon("heartbeat"))
    })
    output$kidsCounterBox <- renderInfoBox({
        valueBox(rawdata()$n_child, "children", icon = icon("child"))
    })
    
    output$datasetCounterBox <- renderInfoBox({
        valueBox(length(rawdata()$nodes),
                 "entities",
                 icon = icon("pie-chart"))
    })
    
    #################
    # showing plots #
    #################
    
    output$A_hours_by_activity <-
        renderPlot({
            plot_hours_by_activity(activitydata())
        })
    
    output$A_hours_by_activity_download <-
        downloadHandler(
            filename = "hours_by_activity.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_hours_by_activity(activitydata()),
                    width = 8,
                    height = 5
                )
            }
        )
    
    output$emptyplot <-
        renderPlot({
            data <- rawdata()
            empty_plot()
        })
    
    callModule(activity_plots,
               "activity",
               activitydata(),
               activity_coltypes())
    callModule(summary_plots,
               "summary",
               summarydata(),
               summary_coltypes())
    callModule(tables,
               "tables",
               activitydata(),
               summarydata(),
               rawdata()$chronicle)
    callModule(sbp_server, "sbp", summarydata())
    callModule(qa_server, "qa", summarydata())
    callModule(tud_maq_base_server,
               "tud_maq",
               summarydata(),
               maqdata(),
               summary_coltypes())
    callModule(
        chronicle_tud_server,
        "chrontud",
        summarydata(),
        rawdata()$chronicle$processed,
        summary_coltypes()
    )
    
})
