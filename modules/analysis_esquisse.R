###################
## UI COMPONENTS ##
###################

esquisse_ui <- function(id) {
    ns <- NS(id)
    tabPanel("Plot builder",
             fluidRow(
                 tags$div(
                 style = "height: 1300px;", # needs to be in fixed height container
                 esquisserUI(
                     id = "esquisse",
                     header = FALSE, # dont display gadget title
                     choose_data = FALSE, # dont display button to change data
                     container = esquisseContainer(height = "700px"),
                     disable_filters = TRUE
                 )
             )
             )
             
    )
    
}

