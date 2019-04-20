shinyServer(function(input, output, session) {
    ###########################################
    # loading data and observing column names #
    ###########################################
    
    token = "NA"

    jwt <- reactiveVal(token)
    jwt <- callModule(authentication_server, "authentication", jwt)
    
    hide(selector = "#navbar li a[data-value=participants]")
    hide(selector = "#navbar li a[data-value=analysis]")
    hide(selector = "#navbar li a[data-value=tables]")
    hide(selector = "#navbar li a[data-value=plots]")
    hide(selector = "#navbar li a[data-value=QA]")
    hide(selector = "#navbar li a[data-value=TUD-MAQ]")
    hide(selector = "#navbar li a[data-value=ScreenBestPractices]")
    hide(id = "waitforauth")
    
    # load data
    rawdata <- reactiveValues(
        tud = list(nodes = list(),
                   edges = list()),
        maq = list(nodes = list(),
                   edges = list()),
        chronicle = list(raw = tibble(),
                         processed = tibble()),
        auth = FALSE,
        n_act = 0,
        n_child = 0,
        n_nodes = 0
    )
    
    data_r <- reactiveValues(data = tibble(), name = "CAFE")
    
    # authentication via cookie
    observe({
        shinyjs::addCssClass(id = "emptyplot",
                             class = "recalculating")
        newdat <-
            get_data(jwt(),
                     cache = TRUE,
                     auth = TRUE,
                     local = TRUE)
        rawdata$tud <- newdat$tud
        rawdata$chronicle <- newdat$chronicle
        rawdata$maq <- newdat$maq
        rawdata$n_child <- newdat$n_child
        rawdata$n_act <- newdat$n_act
        rawdata$n_nodes <- newdat$n_nodes
        rawdata$auth = newdat$auth
        rawdata$alldata = newdat$alldata
        rawdata$coltypes = newdat$coltypes
        shinyjs::removeCssClass(id = "emptyplot",
                                class = "recalculating")
        columns <-
            data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types = c("boolean", "factorial", "numeric"))
        data_r$data <- rawdata$alldata[unique(unlist(columns, use.names=FALSE))]
    })
    
    observeEvent(input$subset, {
        print("subsetting...")
        newdat <- subset_data(
            rawdata = rawdata,
            hourbool = input$subset_hours_on,
            hourrange = input$subset_hours,
            agebool = input$subset_age_on,
            agerange = input$subset_age,
            sitesbool = input$subset_sites_on,
            sitesrange = input$subset_sites,
            progressbool = input$subset_progress_on,
            progressrange = input$subset_progress,
            qualitybool = input$subset_quality_on,
            qualityrange = input$subset_quality
        )
        rawdata$tud$processed = newdat$tud
        rawdata$tud$summarised <- newdat$summary
        rawdata$maq$processed <- newdat$maq
    })
    
    observe({
        if (rawdata$auth) {
            shinyjs::show(selector = "#navbar li a[data-value=participants]")
            shinyjs::show(selector = "#navbar li a[data-value=tables]")
            shinyjs::show(selector = "#navbar li a[data-value=analysis]")
            shinyjs::show(selector = "#navbar li a[data-value=plots]")
            shinyjs::show(selector = "#navbar li a[data-value=QA]")
            shinyjs::show(selector = "#navbar li a[data-value=TUD-MAQ]")
            shinyjs::show(selector = "#navbar li a[data-value=ScreenBestPractices]")
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
        valueBox(rawdata$n_nodes,
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
        }, height = 400)
    
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
    
    callModule(venn_server, "participants", rawdata)
    callModule(demographics_server, "participants", rawdata)
    callModule(univar_server, "analysis", rawdata)
    callModule(concon_server, "analysis", rawdata)
    callModule(catcon_server, "analysis", rawdata)
    callModule(tud_server, "analysis", rawdata)
    callModule(tables,
               "tables",
               rawdata)
    callModule(multivariate_server, "analysis", rawdata)
    callModule(multivariate_cor_server, "analysis", rawdata)
    callModule(module = esquisserServer, id = "esquisse", data = data_r)
    
})
