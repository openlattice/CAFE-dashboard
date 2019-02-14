library(tidyverse)
library(openlattice)
library(httr)
library(yaml)

source("pipelines/constants.R")

load_data <- function(jwt, local=FALSE, auth=FALSE) {
  print("Getting the data !")
  
  # setting up configuration
  basepath = ifelse(local == TRUE, "http://localhost:8080", "https://api.openlattice.com")
  header_params = unlist(list("Authorization" = paste("Bearer", jwt)))
  client <- ApiClient$new(
    defaultHeaders = header_params,
    basePath = basepath
  )

  edmApi <- EdmApi$new(apiClient = client)
  dataApi <- DataApi$new(apiClient = client)
  searchApi <- SearchApi$new(apiClient = client)
  
  # read all entity sets --> is a basic step for data access
  entsets <- edmApi$get_all_entity_sets()

  if (typeof(entsets) != "list"){
    return (list(data = list(), edges = list(), auth=auth, n_act=0, n_child = 0))
  }
    # the constant TUD_entities comes from constants.R and  is a general name that appears in all entity sets
    # since we're combining different entity sets
  
    print("-- Getting data.")
    datasets <- TUD_entities %>% map(get_dataset, entsets, dataApi)
    names(datasets) <- TUD_entities
 
    print("-- Getting edges")
    edges <- TUD_associations %>% map(get_edge_table, datasets, entsets, searchApi)
    names(edges) <- TUD_associations %>% map_chr(function(x){return (paste0(x['src'], "_", x['dst']))})
    
    outdata <- list(
      nodes = datasets,
      edges = edges,
      auth = TRUE,
      n_act = nrow(datasets$primary_activity),
      n_child = nrow(datasets$people)
    )
    
    out <- as.yaml(outdata)
    print("Got the data !")
    return (outdata)
}

get_id <- function(cafename, entsets){
  
  # function that maps general name from constants.R to entitysetid's that 
  # this person has access to.
  
  return(entsets[c("id", "name")] %>% as_tibble() %>% filter(str_detect(name, cafename) & str_detect(name, "CAFE")) %>% pull(id))
}

get_dataset <- function(cafename, entsets, dataApi){
  
  # function to get and combine data from different entity sets
  
  entids <- get_id(cafename, entsets)
  datalist <- list()
  for (num in 1:length(entids)){
    entid = entids[num]
    dat <- dataApi$load_entity_set_data(entid)
    if (is.null(dim(dat))){
      return (tibble())
    } else if (dim(dat)[1] == 1){
      dat <- lapply(dat, as.character)
    } else {
      dat <- dat %>% sapply(as.character)
    }
    datalist[[num]] <- dat %>% as_tibble()
  }
  data <- do.call(bind_rows, datalist)
  return( data )
}

transform_edges <- function(name, edges){
  
  # from edge table --> create | src | dst |
  # with the entity key ids to link data tables
  
  neighdetails <- edges[[name]][['neighborDetails']]
  if (is.null(neighdetails)){
    return (tibble())
  } else if (dim(neighdetails)[1] == 1){
    newtable <- lapply(neighdetails, as.character)
  } else {
    newtable <- neighdetails %>% sapply(as.character)
  }
  newtable <- newtable %>% as_tibble() %>% select('openlattice.@id')
  names(newtable) <- 'dst'
  newtable['src'] <- name
  return (newtable)
}

get_edge_table <- function(input, datasets, entsets, searchApi){
  
  # get edge table from api
  
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
  
  srcids <- get_id(input$src, entsets)
  edgelist <- list()
  for (num in 1:length(srcids)){
    srcid = srcids[num]
    edges <- searchApi$execute_filtered_entity_neighbor_search(srcid, filter)
    edge_table <- names(edges) %>% map_dfr(transform_edges, edges)
    edgelist[[num]] <- edge_table
  }
  edge_table <- do.call(bind_rows, edgelist)
  return (edge_table)
}


