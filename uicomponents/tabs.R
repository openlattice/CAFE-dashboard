home <- tabPanel(
    title = 'home',
    fluidRow(
        box(
            width = 4,
            solidHeader =  FALSE,
            status = "primary",
            HTML('<center><img src="cafe-logo.png" width="300px"></center>')
        ),
        box(
            title = "CAFE analytics dashboard",
            width = 4,
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
        ),
        box(
            width = 4,
            solidHeader =  FALSE,
            status = "primary",
            column(addSpinner(
                plotOutput("emptyplot", height = "200px"),
                spin = "bounce",
                color = cols[1]
            ),
            width = 12)
        )
    ),
    fluidRow(
        valueBoxOutput('activityCounterBox'),
        valueBoxOutput('datasetCounterBox'),
        valueBoxOutput('kidsCounterBox')
    ),
    conditionalPanel(condition = "output.auth==1",
                     fluidRow(
                         box(
                             title = "Average duration of activities per child.",
                             width = 12,
                             solidHeader = TRUE,
                             status = "primary",
                             plotOutput("A_hours_by_activity")
                         )
                     ),
                     fluidRow(
                         box(
                             title = "Total duration of measurements per child.",
                             width = 6,
                             solidHeader = TRUE,
                             status = "primary",
                             plotOutput("A_hours_total")
                         )
                     ))
)