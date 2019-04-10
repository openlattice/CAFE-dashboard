subset_data <-
    function(rawdata,
             hourbool,
             hourrange,
             agebool,
             agerange,
             sitesbool,
             sitesrange,
             progressbool,
             progressrange,
             qualitybool,
             qualityrange
             ) {
        
        output = list(tud = rawdata$tud$preprocessed,
                      maq = rawdata$maq$preprocessed)

        # subset hours
        if (hourbool) {
            print("hours")
            subset <- rawdata$tud$summarised %>% 
                filter(total_time > hourrange[1] & total_time < hourrange[2]) %>% select("nc.SubjectIdentification", "day_id")
            output$tud <- output$tud %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification) %>%
                filter(day_id %in% subset$day_id)
            output$maq <- output$maq %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
        }
        print(dim(output$tud))
        # subset sites
        if (sitesbool) {
            print("sites")
            sites = paste0(sitesrange, collapse="|")
            output$tud <- output$tud %>% filter(str_detect(site, sites))
            output$maq <- output$maq %>% filter(str_detect(study, sites))
        }
        print(dim(output$tud))
        
        if (progressbool) {
            print("progress")
            subset <- rawdata$tud$summarised %>% 
                filter(progress > progressrange[1] & progress < progressrange[2]) %>% select("nc.SubjectIdentification", "day_id")
            output$tud <- output$tud %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification) %>%
                filter(day_id %in% subset$day_id)
            output$maq <- output$maq %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
        }
        
        if (agebool) {
            print("age")
            subset <- rawdata$maq$preprocessed %>% 
                filter(age_months > agerange[1] & age_months < agerange[2]) %>% select("nc.SubjectIdentification")
            output$tud <- output$tud %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            output$maq <- output$maq %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            
        }
        
        if (qualitybool) {
            print("quality")
            subset <- rawdata$maq$preprocessed %>% 
                filter(mean_quality > qualityrange[1] & mean_quality < qualityrange[2]) %>% select("nc.SubjectIdentification")
            output$tud <- output$tud %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            output$maq <- output$maq %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            
        }

        output$summary = summarise_data(output$tud)
        
        return(output)
    }


get_data <- function(jwt, cache = FALSE, auth = FALSE, local = FALSE) {
    print("Getting authenticated !")
    
    if (auth == FALSE) {
        apis <- get_apis(jwt, local)
        if (is.null(apis)) {
            return (get_empty_rawdata())
        }
    } else {
        apis <- NULL
    }
    
    print("Getting data !")
    
    if (cache) {
        rawdata <- read_data(apis, auth = auth, local = local)
    } else {
        rawdata <- load_data(apis)
    }
    
    if (auth == FALSE) {
        rawdata['auth'] <- is_authorized(apis, local)
    } else {
        rawdata['auth'] <- TRUE
    }
    
    rawdata$tud[['preprocessed']] = process_activities(rawdata)
    rawdata$tud[['processed']] = rawdata$tud[['preprocessed']]
    rawdata$tud[['summarised']] = summarise_data(rawdata$tud[['processed']])
    rawdata$maq[['preprocessed']] = process_maq(rawdata)
    rawdata$maq[['processed']] = rawdata$maq[['preprocessed']]
    
    toremove <- c("child_id", "respondent_id", "nc.SubjectIdentification", "day_id")
    
    rawdata$tud[['processed_coltypes']] <- list(
        factorial = rawdata$tud$processed %>% select(which(sapply(., is.factor))) %>% names,
        boolean = rawdata$tud$processed %>% select(which(sapply(., is.logical))) %>% names
    )
    done = c(rawdata$tud$processed_coltypes$factorial, rawdata$tud$processed_coltypes$boolean, toremove)
    rawdata$tud[['processed_coltypes']][['numeric']] <-  setdiff(names(rawdata$tud$processed), done)
    
    rawdata$tud[['coltypes']] <- list(
        factorial = rawdata$tud$summarised %>% select(which(sapply(., is.factor))) %>% names,
        boolean = rawdata$tud$summarised %>% select(which(sapply(., is.logical))) %>% names
    )
    done = c(rawdata$tud$coltypes$factorial, rawdata$tud$coltypes$boolean, toremove)
    rawdata$tud[['coltypes']][['numeric']] <-  setdiff(names(rawdata$tud$summarised), done)
    
    rawdata$maq[['coltypes']] <- list(
        factorial = rawdata$maq$processed %>% select(which(sapply(., is.factor))) %>% names,
        boolean = rawdata$maq$processed %>% select(which(sapply(., is.logical))) %>% names
    )
    done = c(rawdata$maq$coltypes$factorial, rawdata$maq$coltypes$boolean, toremove)
    rawdata$maq$coltypes[['numeric']] <-  setdiff(names(rawdata$maq$processed), done)
    
    rawdata$chronicle[['coltypes']] <- list(
        factorial = rawdata$chronicle$processed %>% select(which(sapply(., is.factor))) %>% names,
        boolean = rawdata$chronicle$processed %>% select(which(sapply(., is.logical))) %>% names
    )
    done = c(rawdata$chronicle$coltypes$factorial, rawdata$chronicle$coltypes$boolean, toremove)
    rawdata$chronicle$coltypes[['numeric']] <-  setdiff(names(rawdata$chronicle$processed), done)
    
    return(rawdata)
    
}


read_data <- function(apis, auth = FALSE, local = FALSE) {
    print("Getting the data !")
    filename = "rawdata_20190409.yaml"
    if (local) {
        rawdata <- read_yaml(paste0("data/", filename))
    } else {
        rawdata <- read_yaml(paste0("/opt/shiny/data/", filename))
    }
    rawdata <- add_authentication_to_raw(rawdata, apis, auth = auth)
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

        # MAQ
        print("-- MAQ: Getting nodes.")
        maqdatasets <- MAQ_entities %>% map(get_node_table, apis)
        names(maqdatasets) <- MAQ_entities
        
        print("-- MAQ: Getting edges.")
        maqedgesdata <-
            MAQ_associations %>% map(get_edge_table, maqdatasets, apis)
        names(maqedgesdata) <-
            MAQ_associations %>% map_chr(function(x) {
                return (paste0(x['src'], "_", x['dst']))
            })
        
        outdata <- list(
            tud = list(
                nodes = datasets,
                edges = edgesdata
            ),
            chronicle = list(raw = tibble(),
                             processed = tibble()),
            maq = list(nodes = maqdatasets,
                       edges = maqedgesdata),
            n_act = dim(datasets$primary_activity)[1],
            n_child = dim(datasets$people)[1],
            auth = TRUE
        )
        
        outdata <- add_authentication_to_raw(outdata, apis, auth = TRUE)
        
        print("Got the data !")
        return (outdata)
    }

get_empty_rawdata <- function() {
    return (list(
        tud = list(nodes = list(),
                   edges = list()),
        chronicle = list(raw = tibble(),
                         processed = tibble()),
        auth = FALSE,
        n_act = 0,
        n_child = 0
    ))
}

get_auth <- function(study, entitysets) {
    return(ifelse(study %in% entitysets, TRUE, FALSE))
}


add_authentication_to_raw <- function(data, apis, auth = FALSE) {
    newdata <- list(
        tud = list(edges = list(), nodes = list()),
        chronicle = list(raw = list(), processed = list()),
        maq = list(edges = list(), nodes = list()),
        n_child = data$n_child,
        n_act = data$n_act
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
    
    proctable <- as.tibble(data$chronicle$processed)
    if (auth == FALSE) {
        proctable <-
            proctable %>% mutate(table_access = study %in% entitysets)
    } else {
        proctable <- proctable %>% mutate(table_access = TRUE)
    }
    newdata$chronicle$raw = rawtable
    newdata$chronicle$processed = proctable
    
    return(newdata)
    
}
