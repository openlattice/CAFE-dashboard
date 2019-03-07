library(shinydashboard)
library(shinyWidgets)
library(VennDiagram)
library(openlattice)
library(ggcorrplot)
library(tidyverse)
library(lubridate)
library(shinyjs)
library(GGally)
library(shiny)
library(auth0)
library(httr)
library(yaml)

httr::set_config(httr::config(http_version = 0))

cols <- c("#ff3c5d",
          "#6124e2",
          "#ffe671",
          "#ff9a58",
          "#dd9e00",
          "#00be84")

nacol <- "#dcdce7"


source("functions/tud_load_data.R")
source("functions/tud_transform_data.R")
source("functions/maq_transform_data.R")
source("functions/tud_summarise_data.R")
source("functions/plots.R")
source("functions/constants.R")
source("functions/configuration.R")
source("functions/chronicle_transform_data.R")
source("functions/read_tables.R")


source("modules/qa.R")
source("modules/tud_maq.R")
source("modules/sbp.R")
source("modules/tables.R")
source("modules/activity_plots.R")
source("modules/summary_plots.R")
source("modules/chrontud.R")
source("modules/venn.R")

source("uicomponents/tabs.R")

read_yaml("secrets.yaml")


