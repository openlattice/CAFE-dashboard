library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(yaml)
library(shinyjs)
library(openlattice)
library(tidyverse)
library(auth0)
library(httr)
library(yaml)
library(tidyverse)
library(lubridate)
library(shiny)
library(GGally)
library(ggcorrplot)
library(tidyverse)
library(openlattice)
library(httr)
library(yaml)
library(lubridate)


source("functions/tud_load_data.R")
source("functions/tud_transform_data.R")
source("functions/maq_transform_data.R")
source("functions/tud_summarise_data.R")
source("functions/plots.R")
source("functions/constants.R")
source("functions/configuration.R")
source("functions/chronicle_transform_data.R")
source("functions/read_tables.R")
source("functions/tud_load_data.R")
source("functions/tud_transform_data.R")


source("modules/qa.R")
source("modules/tud_maq.R")
source("modules/sbp.R")
source("modules/tables.R")
source("modules/activity_plots.R")
source("modules/summary_plots.R")
source("modules/chrontud.R")

read_yaml("secrets.yaml")

cols <- c("#ff3c5d",
          "#6124e2",
          "#ffe671",
          "#ff9a58",
          "#dd9e00",
          "#00be84")

nacol <- "#dcdce7"


