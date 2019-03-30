setwd("/Users/jokedurnez/Documents/accounts/CAFE/CAFE_code/dashboard/")
source("global.R")

jwt <- "*"
rawdata <- get_data(jwt, cache=FALSE, auth=FALSE, local=TRUE)

write_yaml(rawdata, "data/rawdata_20190324.yaml")


# chr <- read_yaml("data/rawdata2.yaml")
# rawdata$chronicle$raw = chr$chronicle$raw %>% as_tibble()
# rawdata$chronicle$processed = chr$chronicle$processed %>% as_tibble()

