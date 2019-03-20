home <- tabPanel(
    title = 'home',
    fluidRow(
        column(width = 4,
        box(
            width = 12,
            solidHeader =  TRUE,
            status = "primary",
            HTML('<center><img src="cafe-logo.png" width="300px"></center>')
        )),
        column(width = 4,box(
            title = "CAFE analytics dashboard",
            width = 12,
            solidHeader =  TRUE,
            status = "primary",
            'This application shows results from the \"Children and Screens\" study.  \
            To load the data, enter your jwt-token in the input above. \
            You can find your jwt-token',
            tags$a(href = "https://openlattice.com/gallery/", "here"),
            'in your account settings after logging in.',
            br(),
            br(),
            br(),
            column(
                textInput(inputId = "jwt",
                          label = "Enter your jwt-token."),
                width = 8
            ),
            column(
                actionButton(inputId = "login",
                             icon("sign-in")),
                width = 4,
                style = "padding-top: 25px"
            )
        )),
        column(width = 4,box(
            width = 12,
            solidHeader =  TRUE,
            status = "primary",
            column(addSpinner(
                plotOutput("emptyplot", height = "200px"),
                spin = "bounce",
                color = cols[1]
            ),
            width = 12)
        ))
    ),
    fluidRow(
        column(width = 4, valueBoxOutput('activityCounterBox', width = 12)),
        column(width = 4, valueBoxOutput('datasetCounterBox', width = 12)),
        column(width = 4, valueBoxOutput('kidsCounterBox', width = 12))
    ),
     fluidRow(id="waitforauth",
         column(
             width = 8,
             box(
             title = "Average duration of activities per child.",
             width = 12,
             solidHeader = TRUE,
             status = "primary",
             tabsetPanel(
                 tabPanel(
                     "Bar chart", 
                     plotOutput("A_hours_by_activity"),
                     downloadButton("A_hours_by_activity_download")
                     ),
                    tabPanel(
                        "Pie chart", 
                        plotOutput("A_pie_hours_by_activity"),
                        downloadButton("A_pie_hours_by_activity_download")
                        )
                 )
             ),
             box(
                 title = "Distributions per child",
                 width = 12,
                 solidHeader = TRUE,
                 status = "primary",
                 tabsetPanel(
                     tabPanel(
                         "Hours per day", 
                         plotOutput("A_hours_total"),
                         downloadButton("A_hours_total_download")
                     ),
                     tabPanel(
                         "Child ages", 
                         plotOutput("A_ages_total"),
                         downloadButton("A_ages_total_download")
                     )
                 )
                 
             )
         ),
         column(
             width = 4,
         box(
             title = "Subsetting the data",
             width = 12,
             solidHeader = TRUE,
             status = "primary",
             "Note that when subsetting, missing data for the condition is removed.",
            "For example, age is a variable present in MAQ data.  As such, when subsetting ages, children who don't have MAQ will not be included. Similarly, when subsetting the total number of hours reported in TUD, children without TUD will not be included.",
             h4("Total number of hours"),
             checkboxInput(
                 "subset_hours_on",
                 label = "Subset total number of hours"
             ),
             sliderInput(
                 "subset_hours", 
                 label="",
                 min = 0,
                 max = 48,
                 value = c(18, 26)
            ),
            # h4("Age"),
            # tags$b("NOT WORKING YET"),
            # checkboxInput(
            #     "subset_age_on",
            #     label = "Subset age"
            # ),
            # sliderInput(
            #     "subset_age",
            #     label = "",
            #     min = 0,
            #     max = 20,
            #     value = c(0, 20)
            # ),
         h4("Progress"),
            checkboxInput(
                "subset_progress_on",
                label = "Subset average progress per block"
            ),
            sliderInput(
                "subset_progress",
                label = "",
                min = 0,
                max = 100,
                value = c(0, 100)
            ),
            h4("Site"),
            checkboxInput(
                "subset_sites_on",
                label = "Subset site"
            ),
            checkboxGroupInput(
                "subset_sites",
                label = h5("Site"),
                choices = c("UM", "WIAMP", "UWCRT", "PM", "GU", "BYU"),
                selected  = c("UM", "WIAMP", "UWCRT", "PM", "GU", "BYU")
            ),
            actionButton(inputId = "subset", "SUBSET")
         )
                     ))
)