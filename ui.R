library(shiny)
library(shinydashboard)

source("uicomponents/tabs.R")

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title =
      div(img(src = "logomark.png",
              class = "logo"),
          "Cafe",
          class = "navbar-title"),
    theme = "custom.css",
    fluid = TRUE,
    windowTitle = "CAFE",
    collapsible = TRUE,
    header = tags$head(
      tags$style(HTML(
        "#page-nav > li:first-child { display: none; }"
      )),
      tags$link(rel = "stylesheet", type = "text/css", href = "http://fonts.googleapis.com/css?family=Chivo"),
      tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "shinydashboard.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "icon", type = "image/png", href = "favicon.png")
    ),
    home,
    conditionalPanel(
      condition = "output.auth==1",
      preprocessed_table,
      summarised_table,
      activity_barcharts,
      preprocessed_barcharts,
      summarised_histograms,
      summarised_crossplots
    )
  )
)
