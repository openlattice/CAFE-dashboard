library(shiny)
library(shinydashboard)
library(shinyjs)

source("uicomponents/tabs.R")

# Define UI for application that draws a histogram
tagList(
    useShinyjs(),
  navbarPage(
      id = "navbar",
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
    navbarMenu("tables",
        preprocessed_table,
        summarised_table,
        chronicle_table
        ),
    navbarMenu("plots",
        activity_barcharts,
        preprocessed_barcharts,
        summarised_histograms,
        summarised_crossplots,
        summarised_SBP,
        chronicle_tud
    ),
    navbarMenu("QA",
        QA_base
    ),
    navbarMenu("TUD-MAQ",
        TUD_MAQ
    )
  )
)
