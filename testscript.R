library(tidyverse)
library(openlattice)

setwd("/Users/jokedurnez/Documents/accounts/CAFE/CAFE_code/dashboard/")
source("pipelines/tud_load_data.R")
source("pipelines/tud_transform_data.R")
source("pipelines/tud_summarise_data.R")
source("servercomponents/plots.R")

jwt <- "*"

rawdata <- load_data(jwt, local = FALSE)
activitydata <- process_activities(rawdata)
summarydata <- summarise_data(activitydata)

# activitydata <- read_csv( "/Users/jokedurnez/Desktop/test.csv")

write_csv(activitydata, "/Users/jokedurnez/Desktop/test.csv")
write_csv(summarydata, "/Users/jokedurnez/Desktop/test_sum.csv")
