home <- tabPanel(
    title = 'home',
    fluidRow(
        column(width = 6,
        box(
            width = 12,
            solidHeader =  TRUE,
            status = "primary",
            HTML('<center><img src="cafe-logo.png" width="300px"></center>')
        )),
        column(width = 6,box(
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
                         "Participants by site", 
                         plotOutput("A_subjects_by_site")
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
            h4("Age"),
            checkboxInput(
                "subset_age_on",
                label = "Subset age (months)"
            ),
            sliderInput(
                "subset_age",
                label = "",
                min = 0,
                max = 240,
                value = c(0, 24)
            ),
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
         h4("Quality checks"),
         checkboxInput(
             "subset_quality_on",
             label = "Subset quality"
         ),
         sliderInput(
             "subset_quality",
             label = "",
             min = 0,
             max = 1,
             value = c(0, 1)
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