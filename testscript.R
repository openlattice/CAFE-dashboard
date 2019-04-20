setwd("/Users/jokedurnez/Documents/accounts/CAFE/CAFE_code/dashboard/")
source("global.R")

# read in for local testing
jwt="eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJlbWFpbCI6Impva2VAb3BlbmxhdHRpY2UuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsInVzZXJfbWV0YWRhdGEiOnt9LCJhcHBfbWV0YWRhdGEiOnsicm9sZXMiOlsiQXV0aGVudGljYXRlZFVzZXIiLCJhZG1pbiJdLCJvcmdhbml6YXRpb25zIjpbIjAwMDAwMDAwLTAwMDAtMDAwMS0wMDAwLTAwMDAwMDAwMDAwMCIsImU2NmM5ZWVmLTIzNzktNGEwYy05Y2JiLWM2ZDhiMmIxM2M1ZiJdfSwibmlja25hbWUiOiJqb2tlIiwicm9sZXMiOlsiQXV0aGVudGljYXRlZFVzZXIiLCJhZG1pbiJdLCJ1c2VyX2lkIjoiZ29vZ2xlLW9hdXRoMnwxMDM3NzE3NTU1NDA4NjUyOTkyNDgiLCJpc3MiOiJodHRwczovL29wZW5sYXR0aWNlLmF1dGgwLmNvbS8iLCJzdWIiOiJnb29nbGUtb2F1dGgyfDEwMzc3MTc1NTU0MDg2NTI5OTI0OCIsImF1ZCI6IktUemd5eHM2S0JjSkhCODcyZVNNZTJjcFRIemh4Uzk5IiwiaWF0IjoxNTU1NjkxNDE2LCJleHAiOjE1NTU3Nzc4MTZ9.cwnQ2XtFubG1UQxMc0y5QI6aaqFZijnvyQGveAJ11E8"
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
write_yaml(subset_tosave, "data/rawdata_20190418.yaml")



