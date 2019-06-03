library(shinydashboard)
library(shinyWidgets)
library(VennDiagram)
library(openlattice)
library(ggcorrplot)
library(tidyverse)
library(lubridate)
library(esquisse)
library(shinyjs)
library(janitor)
library(plotly)
library(GGally)
library(gsheet)
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


source("data_processing/get_raw_data.R")
source("data_processing/data_loading.R")
source("data_processing/tud_transform_data.R")
source("data_processing/maq_transform_data.R")
source("data_processing/maq_transform_parents.R")
source("data_processing/maq_transform_psi.R")
source("data_processing/maq_transform_respondentdetails.R")
source("data_processing/maq_transform_pm.R")
source("data_processing/maq_transform_deviceuse.R")
source("data_processing/maq_transform_language.R")
source("data_processing/tud_summarise_data.R")
source("data_processing/chronicle_transform_data.R")
source("data_processing/subset_data.R")
source("data_processing/read_tables.R")

source("functions/plots.R")
source("functions/constants.R")
source("functions/configuration.R")
source("functions/utils.R")

source("modules/home.R")
source("modules/authentication.R")
source("modules/tables.R")
source("modules/participants_venn.R")
source("modules/participants_demographics.R")
source("modules/analysis_con_con.R")
source("modules/analysis_cat_con.R")
source("modules/analysis_univariate.R")
source("modules/analysis_multivariate_correlogram.R")
source("modules/analysis_multivariate_crossplots.R")
source("modules/analysis_esquisse.R")
source("modules/analysis_tud.R")
source("modules/analysis_scales.R")
source("modules/documentation.R")


secrets <- read_yaml("secrets.yaml")


