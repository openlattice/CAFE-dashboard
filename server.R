shinyServer(function(input, output, session) {
    ###########################################
    # loading data and observing column names #
    ###########################################
    
    hide(selector = "#navbar li a[data-value=participants]")
    hide(selector = "#navbar li a[data-value=tables]")
    hide(selector = "#navbar li a[data-value=plots]")
    hide(selector = "#navbar li a[data-value=QA]")
    hide(selector = "#navbar li a[data-value=TUD-MAQ]")
    hide(id="waitforauth")
    
    # load data
    rawdata <- reactiveValues(
        tud = list(
                nodes = list(),
                edges = list()
            ),
        maq = list(
            nodes = list(),
            edges = list()
        ),
            chronicle = list(
                raw = tibble(),
                processed = tibble()
            ),
            auth = FALSE,
            n_act = 0,
            n_child = 0
        )
    
    observeEvent(input$login, {
        newdat <- get_data(input$jwt, cache = TRUE, auth = FALSE)
        rawdata$tud <- newdat$tud
        rawdata$chronicle <- newdat$chronicle
        rawdata$maq <- newdat$maq
        rawdata$n_child <- newdat$n_child
        rawdata$n_act <- newdat$n_act
        rawdata$auth = newdat$auth
    }, ignoreNULL=FALSE)
    
    observeEvent(input$subset, {
    print("subsetting...")
        newdat <- subset_data(
            rawdata = rawdata,
            hourrange = c(
                ifelse(input$subset_hours_on, input$subset_hours[1], FALSE),
                ifelse(input$subset_hours_on, input$subset_hours[2], FALSE)
            ))
        rawdata$tud$processed = newdat$tud
        rawdata$chronicle <- rawdata$chronicle
        rawdata$maq <- rawdata$maq
       }, ignoreNULL=FALSE)

    observe({
        if (rawdata$auth) {
            shinyjs::show(selector = "#navbar li a[data-value=participants]")
            shinyjs::show(selector = "#navbar li a[data-value=tables]")
            shinyjs::show(selector = "#navbar li a[data-value=plots]")
            shinyjs::show(selector = "#navbar li a[data-value=QA]")
            shinyjs::show(selector = "#navbar li a[data-value=TUD-MAQ]")
            shinyjs::show(id = "waitforauth")
        }
    })
        
    #############################
    # loading information boxes #
    #############################
    
    output$activityCounterBox <- renderInfoBox({
        valueBox(rawdata$n_act,
                 "activity blocks",
                 icon = icon("heartbeat"))
    })
    output$kidsCounterBox <- renderInfoBox({
        valueBox(rawdata$n_child, "children", icon = icon("child"))
    })
    
    output$datasetCounterBox <- renderInfoBox({
        valueBox(length(rawdata$nodes),
                 "entities",
                 icon = icon("pie-chart"))
    })
    
    #################
    # showing plots #
    #################
    
    output$A_hours_by_activity <-
        renderPlot({
            plot_hours_by_activity(rawdata$tud$processed)
        })
    
    output$A_hours_by_activity_download <-
        downloadHandler(
            filename = "hours_by_activity.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_hours_by_activity(rawdata$tud$processed),
                    width = 8,
                    height = 5
                )
            }
        )
    
    output$A_hours_total <-
        renderPlot({
            plot_total_hour_distribution(rawdata$tud$processed)
        })
    
    
    output$emptyplot <-
        renderPlot({
            justtmp <- rawdata
            empty_plot()
        })
    
    callModule(activity_plots_server,
               "activity",
               rawdata)
    callModule(venn_server, "participants", rawdata)
    callModule(summary_plots,
               "summary",
               rawdata)
    callModule(tables,
               "tables",
               rawdata)
    callModule(sbp_server, "sbp", rawdata)
    callModule(qa_server, "qa", rawdata)
    callModule(tud_maq_base_server,
               "tud_maq",
               rawdata)
    callModule(chronicle_tud_server,
               "chrontud",
               rawdata)
    
})
