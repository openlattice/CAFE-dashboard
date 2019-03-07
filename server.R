shinyServer(function(input, output, session) {
    
    ###########################################
    # loading data and observing column names #
    ###########################################
    
    hide(selector = "#navbar li a[data-value=participants]")
    hide(selector = "#navbar li a[data-value=tables]")
    hide(selector = "#navbar li a[data-value=plots]")
    hide(selector = "#navbar li a[data-value=QA]")
    hide(selector = "#navbar li a[data-value=TUD-MAQ]")
    
    # load data
    rawdata <-
        eventReactive(input$login, {
            print("reading...")
            get_data(input$jwt, cache = TRUE, auth = FALSE)
        }, ignoreNULL = FALSE)
    
    observe({
        if (rawdata()$auth) {
            shinyjs::show(selector = "#navbar li a[data-value=participants]")
            shinyjs::show(selector = "#navbar li a[data-value=tables]")
            shinyjs::show(selector = "#navbar li a[data-value=plots]")
            shinyjs::show(selector = "#navbar li a[data-value=QA]")
            shinyjs::show(selector = "#navbar li a[data-value=TUD-MAQ]")
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
            plot_hours_by_activity(rawdata()$tud$processed)
        })
    
    output$A_hours_by_activity_download <-
        downloadHandler(
            filename = "hours_by_activity.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_hours_by_activity(rawdata()$tud$processed),
                    width = 8,
                    height = 5
                )
            }
        )
    
    output$emptyplot <-
        renderPlot({
            justtmp <- rawdata()
            empty_plot()
        })
    
    callModule(activity_plots_server,
               "activity",
               rawdata())
    callModule(venn_server, "participants", rawdata())
    callModule(summary_plots,
               "summary",
               rawdata())
    callModule(tables,
               "tables",
               rawdata())
    callModule(sbp_server, "sbp", rawdata())
    callModule(qa_server, "qa", rawdata())
    callModule(tud_maq_base_server,
               "tud_maq",
               rawdata())
    callModule(
        chronicle_tud_server,
        "chrontud",
        rawdata())
    
})
