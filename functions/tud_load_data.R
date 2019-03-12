# subset_data <- function(rawdata, hourrange = NULL, agerange = NULL, progressrange = NULL, sites = NULL) {
#     if (!is.null(hourrange)){
#         dur_by_session <- rawdata$tud$preprocessed %>%
#             group_by(day_id) %>%
#             summarise(duration = sum(duration) / 60) %>%
#             filter(duration > hourrange[1] & duration < hourrange[2]) %>%
#             select("day_id")
#         rawdata[['processed']] <- rawdata$tud$preprocessed %>% filter(day_id %in% as_vector(dur_by_session))
#     }
# # if (!is.null(agerange)){
# # 
# # }
# 
#     return(rawdata)
# }


get_data <- function(jwt, cache = FALSE, auth=FALSE) {
    print("Getting authenticated !")
    
    if(auth == FALSE){
        apis <- get_apis(jwt)
        if (is.null(apis)) {return (get_empty_rawdata())}
    } else {
        apis <- NULL
    }
    
    if (cache) {
        rawdata <- read_data(apis, auth = auth)
    } else {
        rawdata <- load_data(apis)
    }
    
    if (auth == FALSE){
        rawdata['auth'] <- is_authorized(apis)
    } else {
        rawdata['auth'] <- TRUE
    }

    rawdata$tud[['preprocessed']] = process_activities(rawdata)
    rawdata$tud[['processed']] = process_activities(rawdata)
    rawdata$tud[['summarised']] = summarise_data(rawdata$tud[['processed']])
    rawdata$maq[['preprocessed']] = process_maq(rawdata)
    rawdata$maq[['processed']] = process_maq(rawdata)
 
    rawdata$tud[['processed_coltypes']] <- list(
                numeric = rawdata$tud$processed %>% select(which(sapply(
                    ., is.numeric
                ))) %>% names,
                factorial = rawdata$tud$processed %>% select(which(sapply(
                    ., is.factor
                ))) %>% names,
                boolean = rawdata$tud$processed %>% select(which(sapply(
                    ., is.logical
                ))) %>% names
            )

    rawdata$tud[['summarised_coltypes']] <- list(
        numeric = rawdata$tud$summarised %>% select(which(sapply(
                    ., is.numeric
                ))) %>% names,
                factorial = rawdata$tud$summarised %>% select(which(sapply(
                    ., is.factor
                ))) %>% names,
                boolean = rawdata$tud$summarised %>% select(which(sapply(
                    ., is.logical
                ))) %>% names
            )

    
    return(rawdata)
    
}


read_data <- function(apis,auth = FALSE) {
    print("Getting the data !")
    rawdata <- read_yaml("data/rawdata.yaml")
    rawdata <- add_authentication_to_raw(rawdata, apis, auth=auth)
    print("Got the data !")
    return(rawdata)
}

load_data <-
    function(apis) {
        print("Getting the data !")
        
        # # TUD
        # print("-- TUD: Getting nodes.")
        # datasets <- TUD_entities %>% map(get_node_table, apis)
        # names(datasets) <- TUD_entities
        # 
        # print("-- TUD: Getting edges.")
        # edgesdata <- TUD_associations %>% map(get_edge_table, datasets, apis)
        # names(edgesdata) <-
        #     TUD_associations %>% map_chr(function(x) {
        #         return (paste0(x['src'], "_", x['dst']))
        #     })
        # 
        # MAQ
        print("-- MAQ: Getting nodes.")
        maqdatasets <- MAQ_entities %>% map(get_node_table, apis)
        names(maqdatasets) <- MAQ_entities
        
        print("-- MAQ: Getting edges.")
        maqedgesdata <- MAQ_associations %>% map(get_edge_table, maqdatasets, apis)
        names(maqedgesdata) <-
            MAQ_associations %>% map_chr(function(x) {
                return (paste0(x['src'], "_", x['dst']))
            })

        outdata <- list(
            # tud = list(
            #     nodes = datasets,
            #     edges = edgesdata
            # ),
            chronicle = list(
                raw = tibble(),
                processed = tibble()
            ),
            maq = list(
                nodes = maqdatasets,
                edges = maqedgesdata
            ),
            n_act = dim(datasets$primary_activity)[1],
            n_child = dim(datasets$people)[1],
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


add_authentication_to_raw <- function(data, apis, auth = FALSE){
    
    
    newdata <- list(
        tud = list(edges = list(), nodes = list()),
        chronicle = list(raw = list(), processed = list()),
        maq = list(edges = list(), nodes = list()),
        n_child = data$n_child,
        n_act = data$n_act
    )
    
    if (auth == FALSE){
        entitysets <- apis$personal$edmApi$get_all_entity_sets()$name
    }
    
    # TUD
    for (edge in names(data$tud$edges)) {
        newdata$tud$edges[[edge]] <- as.tibble(data$tud$edges[[edge]])
    }
    
    for (node in names(data$tud$nodes)) {
        nodetable <- as.tibble(data$tud$nodes[[node]])
        if (dim(nodetable)[1]==0){
            nodetable = nodetable
        } else if (auth==FALSE){
            nodetable <- nodetable %>% mutate(table_access = study %in% entitysets)
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
        if (dim(maqnodetable)[1]==0){
            maqnodetable <- maqnodetable
        } else if (auth==FALSE ){
            maqnodetable <- maqnodetable %>% mutate(table_access = study %in% entitysets)
        } else {
            maqnodetable <- maqnodetable %>% mutate(table_access = TRUE)
        }
        newdata$maq$nodes[[node]] <- maqnodetable
    }
    
    # chronicle
    rawtable <- as.tibble(data$chronicle$raw)
    if (auth==FALSE){
        rawtable <- rawtable %>% mutate(table_access = study %in% entitysets)
    } else {
        rawtable <- rawtable %>% mutate(table_access = TRUE)
    }
    
    proctable <- as.tibble(data$chronicle$processed)
    if (auth==FALSE){
        proctable <- proctable %>% mutate(table_access = study %in% entitysets)
    } else {
        proctable <- proctable %>% mutate(table_access = TRUE)
    }
    newdata$chronicle$raw = rawtable
    newdata$chronicle$processed = proctable

    return(newdata)
    
}

