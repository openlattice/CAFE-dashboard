library(tidyverse)
library(openapi)
library(httr)

setwd("/Users/jokedurnez/Documents/accounts/CAFE/CAFE/dashboard/")
source("pipelines/constants.R")

edmApi <- EdmApi$new()
dataApi <- DataApi$new()
searchApi <- SearchApi$new()


load_data <- function(jwt, local=FALSE) {
  print("Getting the data !")
  
  basepath = ifelse(local == TRUE, "http://localhost:8080", "https://api.openlattice.com")
  header_params = unlist(list("Authorization" = paste("Bearer", jwt)))
  client <- ApiClient$new(
    defaultHeaders = header_params,
    basePath = basepath
  )

  edmApi <<- EdmApi$new(apiClient = client)
  dataApi <<- DataApi$new(apiClient = client)
  searchApi <<- SearchApi$new(apiClient = client)
  
  entsets <- edmApi$get_all_entity_sets()

  if (typeof(entsets) != "list"){
    return (list(data = list(), edges = list(), auth=FALSE, n_act=0, n_child = 0))
  }
  
  datasets <- TUD_entities %>% map(get_dataset, entsets)
  names(datasets) <- TUD_entities
  
  edges <- TUD_associations %>% map(get_edge_table, datasets, entsets)
  names(edges) <- TUD_associations %>% map_chr(function(x){return (paste0(x['src'], "_", x['dst']))})
  
  print("Got the data !")
  return (
    list(
      nodes = datasets,
      edges = edges,
      auth = TRUE,
      n_act = nrow(datasets$primary_activity),
      n_child = nrow(datasets$people)
    )
  )
}

for (nm in TUD_entities){
  print(get_id(nm,entsets))
}

get_id <- function(cafename, entsets){
  # get entities and check if response came back
  return(entsets %>% as_tibble() %>% filter(str_detect(name, cafename) & str_detect(name, "CAFE")) %>% pull(id))
}

get_dataset <- function(cafename, entsets){
  entid <- get_id(cafename, entsets)
  data <- dataApi$load_entity_set_data(entid) %>% sapply(as.character) %>% as_tibble()
  return( data )
}


transform_edges <- function(name, edges){
  neighdetails <- edges[[name]][['neighborDetails']]
  if (dim(neighdetails)[1] == 1){
    newtable <- lapply(neighdetails, as.character)
  } else {
    newtable <- neighdetails %>% sapply(as.character)
  }
  newtable <- newtable %>% as_tibble() %>% select('openlattice.@id')
  names(newtable) <- 'dst'
  newtable['src'] <- name
  return (newtable)
}

get_edge_table <- function(input, datasets, entsets){
  
  if (dim(datasets[[input$src]])[1] == 0) {
    return (tibble())
  }
  entkeys = as.vector(unlist(datasets[[input$src]]['openlattice.@id']))
  filter = NeighborSearchFilter$new(
    entityKeyIds = entkeys,
    edge = c(get_id(input$edge, entsets)),
    dst = c(get_id(input$dst, entsets)),
    src = c(get_id(input$src, entsets))
  )
  
  edges <- searchApi$execute_filtered_entity_neighbor_search(get_id(input$src, entsets), filter)
  edge_table <- names(edges) %>% map_dfr(transform_edges, edges)
  return (edge_table)
}


