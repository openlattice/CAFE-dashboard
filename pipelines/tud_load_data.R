library(tidyverse)
library(openlattice)
library(httr)
library(yaml)

source("pipelines/constants.R")
source("pipelines/configuration.R")
source("pipelines/chronicle_transform_data.R")
source("pipelines/read_tables.R")

get_data <- function(jwt, cache = FALSE) {
    print("Getting authenticated !")
    
    apis <- get_apis(jwt)
    if (is.null(apis)) {return (get_empty_rawdata())}
    
    if (cache) {
        rawdata <- read_data(apis)
    } else {
        rawdata <- load_data(apis)
    }
    
    rawdata['auth'] <- is_authorized(apis)
    
    return(rawdata)
    
}


read_data <- function(apis) {
    print("Getting the data !")
    rawdata <- read_yaml("data/rawdata1.yaml")
    rawdata <- add_authentication_to_raw(rawdata, apis)
    print("Got the data !")
    return(rawdata)
}

load_data <-
    function(apis) {
        print("Getting the data !")
        
        # TUD
        print("-- TUD: Getting nodes.")
        datasets <- TUD_entities %>% map(get_node_table, apis)
        names(datasets) <- TUD_entities
        

        print("-- TUD: Getting edges.")
        edgesdata <- TUD_associations %>% map(get_edge_table, datasets, apis)
        names(edgesdata) <-
            TUD_associations %>% map_chr(function(x) {
                return (paste0(x['src'], "_", x['dst']))
            })
        
        outdata <- list(
            tud = list(
                nodes = datasets,
                edges = edgesdata
            ),
            chronicle = list(
                raw = tibble(),
                processed = tibble()
            ),
            maq = list(
                nodes = list(),
                edges = list()
            ),
            n_act = dim(rawdata$tud$nodes$primary_activity)[1],
            n_child = dim(rawdata$tud$nodes$people)[1],
            auth = TRUE
        )
        
        print("Got the data !")
        return (outdata)
    }

get_empty_rawdata <- function(){
    return (list(
        tud = list(
            nodes = list(),
            edges = list()
        ),
        chronicle = list(
            raw = tibble(),
            processed = tibble()
        ),
        auth = FALSE,
        n_act = 0,
        n_child = 0
    ))
}

get_auth <- function(study, entitysets){
    return(ifelse(study %in% entitysets, TRUE, FALSE))
}


add_authentication_to_raw <- function(data, apis){
    
    newdata <- list(
        tud = list(edges = list(), nodes = list()),
        chronicle = list(raw = list(), processed = list()),
        maq = list(edges = list(), nodes = list),
        n_child = data$n_child,
        n_act = data$n_act
    )
    
    entitysets <- apis$personal$edmApi$get_all_entity_sets()$name
    
    # TUD
    for (edge in names(data$tud$edges)) {
        newdata$tud$edges[[edge]] <- as.tibble(data$tud$edges[[edge]])
    }
    
    for (node in names(data$tud$nodes)) {
        nodetable <- as.tibble(data$tud$nodes[[node]])
        nodetable <- nodetable %>% mutate(table_access = study %in% entitysets)
        newdata$tud$nodes[[node]] <- nodetable
    }
        
    # chronicle
    rawtable <- as.tibble(data$chronicle$raw)
    rawtable <- rawtable %>% mutate(table_access = study %in% entitysets)
    proctable <- as.tibble(data$chronicle$processed)
    proctable <- proctable %>% mutate(table_access = study %in% entitysets)
    newdata$chronicle$raw = rawtable
    newdata$chronicle$processed = proctable

    return(newdata)
    
}

