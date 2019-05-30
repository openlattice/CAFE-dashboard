read_data <- function(apis, auth = FALSE, local = FALSE) {
    ptm <- proc.time()
    filename = "rawdata_20190519.yaml"
    if (local) {
        cat(file=stderr(), "Reading the data from disk...\n")
        rawdata <- read_yaml(paste0("data/", filename))
    } else {
        rawdata <- read_yaml(paste0("/opt/shiny/data/", filename))
    }
    rawdata <- add_authentication_to_raw(rawdata, apis, auth = auth)
    cat(file=stderr(), paste0("    ---- Reading in the data took: ", (proc.time() - ptm)['elapsed'], " seconds.\n"))
    return(rawdata)
}

load_data <-
    function(apis, TUD = TRUE, MAQ = TRUE) {
        cat(file=stderr(), "Loading the data from the platform...\n")
        
        # TUD
        if (TUD){
            cat(file=stderr(), "  -- TUD: Getting nodes.\n")
            datasets <- TUD_entities %>% map(get_node_table, apis)
            names(datasets) <- TUD_entities
            
            cat(file=stderr(), "  -- TUD: Getting edges.\n")
            edgesdata <-
                TUD_associations %>% map(get_edge_table, datasets, apis)
            names(edgesdata) <-
                TUD_associations %>% map_chr(function(x) {
                    return (paste0(x['src'], "_", x['dst']))
                })
        }

        # MAQ
        if (MAQ) {
            cat(file=stderr(), "  -- MAQ: Getting nodes.\n")
            maqdatasets <- MAQ_entities %>% map(get_node_table, apis)
            names(maqdatasets) <- MAQ_entities
            
            cat(file=stderr(), "  -- MAQ: Getting edges.\n")
            maqedgesdata <-
                MAQ_associations %>% map(get_edge_table, maqdatasets, apis)
            names(maqedgesdata) <-
                MAQ_associations %>% map_chr(function(x) {
                    return (paste0(x['src'], "_", x['dst']))
                })
            
            # fun fact: sometimes same name in list !
            maqedges = tibble(maqedgesdata)
            maqedges['name'] = names(maqedgesdata)
            maqedges = maqedges %>% unnest() %>% group_by(name) %>% nest()
            maqedgesdata = maqedges %>% pull(data) %>% map(function(x){return(remove_empty(x, which = c("cols")))})
            names(maqedgesdata) = maqedges %>% pull(name)
        }
        
        outdata <- list()
        if (TUD){
            outdata[['tud']] <- list(nodes = datasets,
                                     edges = edgesdata)
            outdata[['n_act']] = dim(datasets$primary_activity)[1]
            outdata[['n_child']] = dim(datasets$people)[1]
        }
        if (MAQ) {
            outdata[['maq']] <- list(nodes = maqdatasets,
                                     edges = maqedgesdata)
        }
        outdata[['chronicle']] <- list(raw = tibble(),
                             processed = tibble())
        outdata[['auth']] <- TRUE
        
        outdata <-
            add_authentication_to_raw(outdata, apis, auth = TRUE)
        
        cat(file=stderr(), "Got the data !\n")
        return (outdata)
    }

add_authentication_to_raw <- function(data, apis, auth = FALSE) {
    newdata <- list(
        tud = list(edges = list(), nodes = list()),
        chronicle = list(raw = list(), processed = list()),
        maq = list(edges = list(), nodes = list()),
        n_child = data$n_child,
        n_act = data$n_act,
        n_nodes = data$n_nodes
    )
    
    if (auth == FALSE) {
        entitysets <- apis$personal$edmApi$get_all_entity_sets()$name
    }
    
    # TUD
    for (edge in names(data$tud$edges)) {
        newdata$tud$edges[[edge]] <- as.tibble(data$tud$edges[[edge]])
    }
    
    for (node in names(data$tud$nodes)) {
        nodetable <- as.tibble(data$tud$nodes[[node]])
        if (dim(nodetable)[1] == 0) {
            nodetable = nodetable
        } else if (auth == FALSE) {
            nodetable <-
                nodetable %>% mutate(table_access = study %in% entitysets)
        } else {
            nodetable <- nodetable %>% mutate(table_access = TRUE)
        }
        newdata$tud$nodes[[node]] <- nodetable
    }
    
    # MAQ
    for (edge in names(data$maq$edges)) {
        newdata$maq$edges[[edge]] <- as.tibble(data$maq$edges[[edge]])
    }
    
    for (node in names(data$maq$nodes)) {
        maqnodetable <- as.tibble(data$maq$nodes[[node]])
        if (dim(maqnodetable)[1] == 0) {
            maqnodetable <- maqnodetable
        } else if (auth == FALSE) {
            maqnodetable <-
                maqnodetable %>% mutate(table_access = study %in% entitysets)
        } else {
            maqnodetable <- maqnodetable %>% mutate(table_access = TRUE)
        }
        newdata$maq$nodes[[node]] <- maqnodetable
    }
    
    # chronicle
    rawtable <- as.tibble(data$chronicle$raw)
    if (auth == FALSE) {
        rawtable <-
            rawtable %>% mutate(table_access = study %in% entitysets)
    } else {
        rawtable <- rawtable %>% mutate(table_access = TRUE)
    }
    
    newdata$chronicle$raw = rawtable

    return(newdata)
    
}

get_auth <- function(study, entitysets) {
    return(ifelse(study %in% entitysets, TRUE, FALSE))
}
