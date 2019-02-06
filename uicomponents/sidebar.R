library(shiny)
library(shinydashboard)

dbSidebar <- dashboardSidebar(
  sidebarSearchForm(
    textId = "jwt",
    buttonId = "authButton",
    label = "Enter your jwt-token."
  ),
  conditionalPanel(
    condition = "output.auth==1",
    sidebarMenu(

      # home

      menuItem("Home", tabName = "home"),

      # activity level data

      menuItem("Activity level data", tabName = "preprocessed_table"),
      menuItem(
        "Activity level charts",
        startExpanded = FALSE,
        menuSubItem("Activities", tabName = "activity_barcharts"),
        menuSubItem("Barcharts", tabName = "preprocessed_barcharts")
      ),

      # child level data

      menuItem("Child level data", tabName = "summarised_table"),
      menuItem(
        "Child level charts",
        startExpanded = FALSE,
        menuSubItem("Histograms", tabName = "summarised_histograms"),
        menuSubItem("Cross-Plots", tabName = "summarised_crossplots")
      )
    )
  )
)
