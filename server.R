shinyServer(function(input, output, session) {
    ###########################################
    # loading data and observing column names #
    ###########################################
    
    runjs('
        var get_cookies = function() {
            var ze_cookies = Cookies.get();
            Shiny.onInputChange("cookies", ze_cookies);
            console.log(ze_cookies);
        }
        get_cookies()
    ')

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
    
    # authentication via cookie
    observeEvent(input$cookies, {
        jwt = str_replace(input$cookies$authorization,"Bearer ", "")
        shinyjs::addCssClass(
            id = "emptyplot",
            class = "recalculating"
        )
        print(input$cookies)
        newdat <- get_data(jwt, cache = TRUE, auth = FALSE, local=FALSE)
        rawdata$tud <- newdat$tud
        rawdata$chronicle <- newdat$chronicle
        rawdata$maq <- newdat$maq
        rawdata$n_child <- newdat$n_child
        rawdata$n_act <- newdat$n_act
        rawdata$auth = newdat$auth
        shinyjs::removeCssClass(
            id = "emptyplot",
            class = "recalculating"
        )
    }, ignoreNULL=FALSE)
    
    # authentication via jwt
    observeEvent(input$jwt, {
        newdat <- get_data(input$jwt, cache = TRUE, auth = FALSE, local=FALSE)
        rawdata$tud <- newdat$tud
        rawdata$chronicle <- newdat$chronicle
        rawdata$maq <- newdat$maq
        rawdata$n_child <- newdat$n_child
        rawdata$n_act <- newdat$n_act
        rawdata$auth = newdat$auth
        shinyjs::removeCssClass(
            id = "emptyplot",
            class = "recalculating"
        )
    }, ignoreNULL=FALSE)

    observeEvent(input$subset, {
        print("subsetting...")
        newdat <- subset_data(
            rawdata = rawdata,
            hourbool = input$subset_hours_on,
            hourrange = input$subset_hours,
            sitesbool = input$subset_sites_on,
            sitesrange = input$subset_sites,
            progressbool = input$subset_progress_on,
            progressrange = input$subset_progress
            )
        rawdata$tud$processed = newdat$tud
        rawdata$tud$summarised <- newdat$summary
        rawdata$maq$processed <- newdat$maq
       })

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
    
    output$A_pie_hours_by_activity <-
        renderPlot({
            pie_hours_by_activity(rawdata$tud$processed)
        }, height=400)
    
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
    
    output$A_pie_hours_by_activity_download <-
        downloadHandler(
            filename = "pie_hours_by_activity.png",
            content = function(file) {
                ggsave(
                    file,
                    pie_hours_by_activity(rawdata$tud$processed),
                    width = 8,
                    height = 5
                )
            }
        )
    
    output$A_subjects_by_site <-
        renderPlot({
            plot_subjects_by_site(rawdata)
        })
    
    output$A_hours_total <-
        renderPlot({
            plot_total_hour_distribution(rawdata$tud$processed)
        })
    
    output$A_hours_total_download <-
        downloadHandler(
            filename = "hours_distribution.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_total_hour_distribution(rawdata$tud$processed),
                    width = 8,
                    height = 5
                )
            }
        )
    
    output$A_ages_total <-
        renderPlot({
            plot_total_age_distribution(rawdata$maq$processed)
        })
    
    output$A_ages_total_download <-
        downloadHandler(
            filename = "ages_distribution.png",
            content = function(file) {
                ggsave(
                    file,
                    plot_total_age_distribution(rawdata$maq$processed),
                    width = 8,
                    height = 5
                )
            }
        )
    
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
