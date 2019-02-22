library(tidyverse)
library(openlattice)
library(httr)
library(yaml)

source("pipelines/constants.R")
source("pipelines/configuration.R")

load_data <- function(jwt, local=FALSE, auth=FALSE) {
    
    print("Getting authenticated !")
    
    apis <- get_apis(jwt)
    
    print("Getting the data !")
    
    # read all entity sets --> is a basic step for data access
    
    if (is.null(apis)) {
        return (list(data = list(), edges = list(), auth=auth, n_act=0, n_child = 0))
    }

    # the constant TUD_entities comes from constants.R and  is a general name that appears in all entity sets
    # since we're combining different entity sets
    
    print("-- Getting nodes.")
    
    datasets <- TUD_entities %>% map(get_node_table, apis)
    names(datasets) <- TUD_entities
    
    lengths <- datasets %>% map(nrow)

    print("-- Getting edges.")
    
    edges <- TUD_associations %>% map(get_edge_table, datasets, apis)
    names(edges) <- TUD_associations %>% map_chr(function(x){return (paste0(x['src'], "_", x['dst']))})
    
    outdata <- list(
        nodes = datasets,
        edges = edges,
        auth = TRUE,
        n_act = nrow(datasets$primary_activity),
        n_child = nrow(datasets$people)
    )
    
    print("Got the data !")
    return (outdata)
}


get_node_table <- function(cafename, apis){
    
    personal_all_entsets <- apis$personal$edmApi$get_all_entity_sets()
    master_all_entsets <- apis$master$edmApi$get_all_entity_sets()
    
    # get master data
    
    entsetnames <- master_all_entsets %>% filter(str_detect(name, paste0("CAFE_.{0,5}",cafename))) %>% pull(name)

    entsets <- tibble()
    
    for (entsetname in entsetnames) {
        
        # load data
        entid <- apis$master$edmApi$get_entity_set_id(entsetname)
        dat <- apis$master$dataApi$load_entity_set_data(entid)
        
        if (length(dat["openlattice.@id"]) == 1){
            dat <- dat %>% lapply( function(x){x <- gsub("NULL", NA, paste(x))}) %>% as_tibble()
        } else {
            dat <- dat %>% sapply( function(x){x <- gsub("NULL", NA, paste(x))}) %>% as_tibble()
        }

        # add column for table access
        
        if (dim(dat)[1] > 0){
            if (entsetname %in% personal_all_entsets$name){
                dat['table_access'] = TRUE
            } else {
                dat['table_access'] = FALSE
            }
        }
        
        # bind to table
        
        entsets <- bind_rows(entsets, dat)
    }
    
    return( entsets )
    
}


transform_edges <- function(table){

    # with the entity key ids to link data tables

    table <- table[['neighborDetails']]
    
    if (length(table["openlattice.@id"]) == 1){
        table <- table %>% lapply( function(x){x <- gsub("NULL", NA, as.character(x))}) %>% as_tibble()
    } else {
        table <- table %>% sapply( function(x){x <- gsub("NULL", NA, as.character(x))}) %>% as_tibble()
    }

    newtable <- table %>% select('openlattice.@id')
    return (newtable)
}

get_edge_table <- function(cafeedge, datasets, apis){
    
    master_all_entsets <- apis$master$edmApi$get_all_entity_sets()
    
    # get entity set ids for this edge
    
    entsetids <- list()
    for (part in c("src", 'edge', 'dst')){
        entsetnames <- master_all_entsets %>% filter(str_detect(name, paste0("CAFE_.{0,5}",cafeedge[part]))) %>% pull(name)
        entsetids[[part]] <- entsetnames %>% map_chr( apis$master$edmApi$get_entity_set_id )
    }

    # get entity keys for source and filter
    
    if (!'openlattice.@id' %in% names(datasets[[cafeedge$src]])){
        return (tibble())
        }
    
    entkeys <- datasets[[cafeedge$src]] %>% pull('openlattice.@id')
    
    filter = NeighborSearchFilter$new(
        entityKeyIds = entkeys,
        edge = entsetids[['edge']],
        dst = entsetids[['dst']],
        src = entsetids[['src']]
    )
    
    # get edges
    
    edges_table <- tibble()
    
    for (src_id in entsetids[['src']]){
        
        edges <- apis$master$searchApi$execute_filtered_entity_neighbor_search(src_id, filter)
        
        # process edges response and append
        
        edges_trans <- edges %>% map(transform_edges) %>% map2_dfr(names(edges), ~mutate(.x, name=.y))
        if (dim(edges_trans)[1] == 0){next}
        
        names(edges_trans) <- c("dst", "src")
        edges_table <- bind_rows(edges_table, edges_trans)
       
    }
    
    return (edges_table)
}


