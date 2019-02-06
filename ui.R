library(shiny)
library(shinydashboard)

source("UIcomponents/sidebar.R")
source("UIcomponents/tabs.R")

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
       title =
         div(
           img(
             src = "logomark.png",
             class = "logo"
           ),
           "Cafe"
         ),
      theme = "custom.css",
      # fluid = TRUE,
      header = tags$head(
        tags$style(HTML("#page-nav > li:first-child { display: none; }")),
        tags$link(rel= "stylesheet", type = "text/css", href = "http://fonts.googleapis.com/css?family=Open+Sans"),
        tags$link(rel= "stylesheet", type = "text/css", href = "AdminLTE.css"),
        tags$link(rel= "stylesheet", type = "text/css", href = "shinydashboard.css"),
        tags$link(rel= "stylesheet", type = "text/css", href = "custom.css"),
        tags$link(rel = "icon", type = "image/png", href="favicon.png")
      ),
      # title = div(
      #   column(img(src="logomark.png", class="logo"),width=1),
      #   column(class="portalname", "Cafe", width=6)
      #   ),
      home,
      preprocessed_table,
      summarised_table,
      activity_barcharts,
      preprocessed_barcharts,
      summarised_histograms,
      summarised_crossplots
      )
    )

 
