## UI FUNCTIONS

codebook <- function(id){
    ns <- NS(id)
    navbarMenu(
        "documentation",
        tabPanel(title = "Codebook: demographics",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_demographics"))
                     )
                 )),
        tabPanel(title = "Codebook: media use",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_avoid_media"))
                     )
                 )),
        tabPanel(title = "Codebook: videochat",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_videochat"))
                     )
                 )),
        tabPanel(title = "Codebook: quality assessment",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_qa"))
                     )
                 )),
        tabPanel(title = "Codebook: deviceuse",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_deviceuse"))
                     )
                 )),
        tabPanel(title = "Codebook: mobile deviceuse",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_mobiledeviceuse"))
                     )
                 )),
        tabPanel(title = "Codebook: devices",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_devices"))
                     )
                 )),
        tabPanel(title = "Codebook: sleep",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_childsleep"))
                     )
                 )),
        tabPanel(title = "Codebook: language",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_childlanguage"))
                     )
                 )),
        tabPanel(title = "Codebook: parents media use",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_parentsmediause"))
                     )
                 )),
        tabPanel(title = "Codebook: parents mediation",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_parentsmediation"))
                     )
                 )),
        tabPanel(title = "Codebook: parents media attitudes",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_parentsmediaattitude"))
                     )
                 )),
        tabPanel(title = "Codebook: parents media use",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_parentmediause"))
                     )
                 )),
        tabPanel(title = "Codebook: households",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_households"))
                     )
                 )),
        tabPanel(title = "Codebook: parent mood",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_parentmood"))
                     )
                 )),
        tabPanel(title = "Codebook: parents stress index",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_parentstressindex"))
                     )
                 )),
        tabPanel(title = "Codebook: imeuse",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_timeuse"))
                     )
                 )),
        tabPanel(title = "Codebook: passive sensing (Time Use Diary)",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_tuactivity"))
                     )
                 )),
        tabPanel(title = "Codebook: passive sensing summary (Time Use Diary)",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_tusummary"))
                     )
                 )),
        tabPanel(title = "Codebook: other",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_other"))
                     )
                 )),
        tabPanel(title = "Technical codebook",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_technical"))
                     )
                 )),
        tabPanel(title = "Data summary",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Data Summary",
                         verbatimTextOutput(outputId = ns("summary"))
                     )
                 ))
        
    )
}

# SERVER FUNCTIONS

documentation_server <-
    function(input,
             output,
             session,
             rawdata
             ) {
        
        ns <- session$ns

        doc = gsheet2tbl("docs.google.com/spreadsheets/d/1KlUTXi_s_AHa7hslB8ZeI0I1QHqGczzDQwQJbGYs8-E/edit?usp=sharing")
        sfvars = doc %>% filter(str_detect(Variable, "sf_")) %>% pull(Variable)
        keywords = list(
            codebook_demographics = "Demographics",
            codebook_avoid_media = "Avoiding Media Device Use",
            codebook_videochat = "Video Chat",
            codebook_qa = "Quality Control",
            codebook_devices = "Devices",
            codebook_deviceuse = "Device Use",
            codebook_deviceuse = "Mobile Device Use",
            codebook_childsleep = "Child Sleep",
            codebook_childlanguage = "Child Language",
            codebook_parentsmediause = "Parent's Media Use",
            codebook_parentsmediaexposure = "Parent's Media Exposure",
            codebook_parentsmediaattitude = "Parent's Media Attitudes",
            codebook_parentsmediation = "Parent Mediation",
            codebook_coview = "Media Content",
            codebook_parentmediause = "Parent's Media Use",
            codebook_households = "Households",
            codebook_parentmood = "Parent Mood",
            codebook_parentstressindex = "Parent Stress index",
            codebook_timeuse = "Time Use",
            codebook_tuactivity = "Time Use Diary activity",
            codebook_tusummary = "Time Use Diary summary"
        )
        
        output$codebook_short_form <- renderDataTable({
            doc %>% filter(Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_avoid_media <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_avoid_media']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_videochat <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_videochat']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_demographics <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_demographics']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_devices <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_devices']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_qa <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_qa']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_parentsmediause <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_parentsmediause']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_parentsmediaexposure <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_parentsmediaexposure']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))

        output$codebook_parentsmediaattitude <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_parentsmediaattitude']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))

        output$codebook_parentsmediation <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_parentsmediation']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_childlanguage <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_childlanguage']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_households <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_households']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_parentmood <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_parentmood']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_parentstressindex <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_parentstressindex']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_coview <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_coview']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_deviceuse <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_deviceuse']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_mobiledeviceuse <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_mobiledeviceuse']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_childsleep <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_childsleep']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_parentmediause <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_parentmediause']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_tuactivity <- renderDataTable({
            doc %>% filter((Domain == keywords[['codebook_timeuse']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_tuactivity <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_tuactivity']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_other <- renderDataTable({
            doc %>% filter(!str_detect(Domain, paste0(flatten_chr(keywords), collapse="|")) & !Variable %in% sfvars)
        }, options = list(scrollX = TRUE))
        
        output$codebook_technical <- renderDataTable({
            doc
        }, options = list(scrollX = TRUE))
        
        output$summary <- renderPrint({
            rawdata$alldata %>% summary()
        })
        
 
    }

# get_first <- function(data, variable){
#     vals = data[variable] %>% unlist(use.names= FALSE) %>% unique()
#     return(vals[!is.na(vals)][1])
#     
# }
# 
# datatypes = data_get_coltypes(rawdata, types = c("numeric", "factorial", "boolean", "character"))
# tp =  datatypes %>% unlist() %>% names()
# nm = datatypes %>% unlist() %>% as.vector()
# tibble(nm,tp) %>%
#     rowwise() %>%
#     mutate(
#         numeric = str_detect(tp, "numeric"),
#         factor = str_detect(tp, "factor"),
#         boolean = str_detect(tp, "boolean"),
#         character = str_detect(tp, "character"),
#         firstval = get_first(rawdata$alldata, nm)
#         )


