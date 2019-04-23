setwd("/Users/jokedurnez/Documents/accounts/CAFE/CAFE_code/dashboard/")
source("global.R")

# read in for local testing
local = TRUE
auth = TRUE
cache = TRUE
rawdata = get_data(jwt, TRUE, TRUE, TRUE)

# load from db and add chronicle
rawdata = get_raw_data(jwt, FALSE, FALSE, TRUE)
chr = read_yaml("data/rawdata_20190409.yaml")
rawdata$chronicle$raw = chr$chronicle$raw
subset_tosave <- list(
    tud = list(nodes = rawdata$tud$nodes, edges = rawdata$tud$edges),
    maq = list(nodes = rawdata$maq$nodes, edges = rawdata$maq$edges),
    chronicle = list(raw = chr$chronicle$raw)
)
write_yaml(subset_tosave, "data/rawdata_20190422.yaml")



apis <- get_apis(jwt, TRUE)
rawdata <- load_data(apis, TUD=FALSE)
