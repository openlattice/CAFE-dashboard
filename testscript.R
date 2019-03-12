setwd("/Users/jokedurnez/Documents/accounts/CAFE/CAFE_code/dashboard/")
source("global.R")

jwt <- "*"

rawdata <- get_data(jwt, cache=TRUE, auth=TRUE)