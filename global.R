library(shinydashboard)
library(shinyWidgets)
library(VennDiagram)
library(openlattice)
library(ggcorrplot)
library(tidyverse)
library(lubridate)
library(shinyjs)
library(plotly)
library(GGally)
library(shiny)
library(auth0)
library(httr)
library(yaml)

httr::set_config(httr::config(http_version = 0))

cols <- c("#6124e2",
          "#44beff",
          "#bc0000",
          "#ff9a58",
          "#00be84",
          "#a939ff",
          "#ffde00",
          "#00bace",
          "#f25497",
          "#2f69ff",
          "#00583d",
          "#870000",
          "#0021ba")

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


