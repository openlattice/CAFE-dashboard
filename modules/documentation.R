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
        tabPanel(title = "Codebook: parents media attitudes",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_parentsmediaattitude"))
                     )
                 )),
        tabPanel(title = "Codebook: parents mediation",
                 fluidRow(
                     box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Codebook",
                         dataTableOutput(outputId = ns("codebook_parentmediation"))
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
                 ))
        
    )
}

# SERVER FUNCTIONS

documentation_server <-
    function(input,
             output,
             session) {
        
        ns <- session$ns

        doc = gsheet2tbl("docs.google.com/spreadsheets/d/1KlUTXi_s_AHa7hslB8ZeI0I1QHqGczzDQwQJbGYs8-E/edit?usp=sharing")
        sfvars = doc %>% filter(str_detect(Variable, "sf_")) %>% pull(Variable)
        keywords = list(
            codebook_demographics = "Demographics",
            codebook_avoid_media = "Avoiding Media Device Use",
            codebook_videochat = "Video Chat",
            codebook_qa = "Quality Control",
            codebook_deviceuse = "Device Use",
            codebook_childsleep = "Child Sleep",
            codebook_childlanguage = "Child Language",
            codebook_parentsmediause = "Parent's Media Use",
            codebook_parentsmediaexposure = "Parent's Media Exposure",
            codebook_parentsmediaattitude = "Parent's Media Attitudes",
            # codebook_coview = "Media Content",
            codebook_parentmediation = "Parent Mediation",
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

        output$codebook_childlanguage <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_childlanguage']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        # output$codebook_coview <- renderDataTable({
        #     doc %>% filter(str_detect(Domain, keywords[['codebook_coview']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        # }, options = list(scrollX = TRUE))
        
        output$codebook_deviceuse <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_deviceuse']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_childsleep <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_childsleep']]) & !Variable %in% sfvars) %>% select(Variable, Description)
        }, options = list(scrollX = TRUE))
        
        output$codebook_parentmediation <- renderDataTable({
            doc %>% filter(str_detect(Domain, keywords[['codebook_parentmediation']]) & !Variable %in% sfvars) %>% select(Variable, Description)
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
        
 
    }



