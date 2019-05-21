### main function to get data

get_data <-
    function(jwt,
             cache = FALSE,
             auth = FALSE,
             local = FALSE) {
        rawdata = get_raw_data(jwt, cache, auth, local)
        if (rawdata$auth) {
            rawdata <- data_add_processed(rawdata)
        }
        return(rawdata)
    }

get_raw_data <-
    function(jwt,
             cache = FALSE,
             auth = FALSE,
             local = FALSE) {
        print("Getting authenticated...")
        
        if (auth == FALSE) {
                apis <- get_apis(jwt, local)
                if (is.null(apis)) {
                    return (get_empty_rawdata())
                }
        } else {
            apis <- NULL
        }
        
        if (cache) {
            rawdata <- read_data(apis, auth = auth, local = local)
        } else {
            rawdata <- load_data(apis)
        }
        
        if (local) {
            basedir = "data/"
        } else {
            basedir = "/opt/shiny/data/"
        }
        voc_prod_fem <-
            read_csv(paste0(
                basedir,
                "vocabulary_production_norms_table_lvl_IIA_female.csv"
            ))
        voc_prod_fem$sex = "Female"
        # cols = names(voc_prod_fem)[str_detect(names(voc_prod_fem), "percentile")]
        # voc_prod_fem[cols] = voc_prod_fem[cols]/max(voc_prod_fem[cols])
        voc_prod_mal <-
            read_csv(paste0(
                basedir,
                "vocabulary_production_norms_table_lvl_IIA_male.csv"
            ))
        voc_prod_mal$sex <- "Male"
        voc_prod = bind_rows(voc_prod_fem, voc_prod_mal)
        # cols = names(voc_prod_mal)[str_detect(names(voc_prod_mal), "percentile")]
        # voc_prod_mal[cols] = voc_prod_mal[cols]/max(voc_prod_mal[cols])
        # ws <- read_csv(paste0(basedir, "words_produced_norms_table_WS.csv"))
        # cols = names(ws)[str_detect(names(ws), "percentile")]
        # ws[cols] = ws[cols]/max(ws[cols])
        # wg <- read_csv(paste0(basedir, "words_produced_norms_table_WG.csv"))
        # cols = names(wg)[str_detect(names(wg), "percentile")]
        # wg[cols] = wg[cols]/max(wg[cols])
        # ws <- read_csv(paste0(basedir, "words_produced_norms_table_WS.csv"))
        # cols = names(ws)[str_detect(names(ws), "percentile")]
        # ws[cols] = ws[cols]/max(ws[cols])
        # norms = rbind(wg, ws)
        
        rawdata[['language_norms']] = list(norms_18_30 = voc_prod)
        
        rawdata['auth'] <- is_authorized(apis, local)

        return(rawdata)
        
    }


# function to add processed data to data

data_add_processed <- function(rawdata) {
    print("Processing the data...")
    ptm <- proc.time()
    tud_pr = process_activities(rawdata)
    print(paste0(
        "    ---- Transforming TUD took: ",
        (proc.time() - ptm)['elapsed'],
        " seconds."
    ))
    ptm <- proc.time()
    tud_sm = summarise_data(tud_pr) %>% mutate(tud_present = TRUE)
    print(paste0("    ---- Summarising TUD took: ", (proc.time() - ptm)['elapsed'], " seconds."))
    ptm <- proc.time()
    maq_pr = process_maq(rawdata) %>% mutate(maq_present = TRUE)
    print(paste0(
        "    ---- Transforming MAQ took: ",
        (proc.time() - ptm)['elapsed'],
        " seconds."
    ))
    ptm <- proc.time()
    chr_pr = process_chronicle(rawdata) %>% mutate(chr_present = TRUE)
    print(paste0(
        "    ---- Transforming chronicle took: ",
        (proc.time() - ptm)['elapsed'],
        " seconds."
    ))
    ptm <- proc.time()
    bigdataset = tud_sm %>%
        full_join(maq_pr, by = c('child_id', "study")) %>%
        full_join(chr_pr, by = 'child_id') %>%
        arrange(time_deviance_from_24) %>%
        mutate(study = study.x,
               table_access = table_access.x & table_access.y) %>%
        group_by(study, child_id, tud_present, maq_present, chr_present) %>%
        filter(row_number() <= 1) %>%
        select(-c(study.x, study.y, table_access.x, table_access.y))
    print(paste0(
        "    ---- Putting data together took: ",
        (proc.time() - ptm)['elapsed'],
        " seconds."
    ))
    colnames(bigdataset) <- make.unique(names(bigdataset))
    
    outdata <- rawdata
    outdata$tud[['preprocessed']] = tud_pr
    outdata$tud[['processed']] = tud_pr
    outdata$tud[['summarised']] = tud_sm
    outdata$maq[['preprocessed']] = maq_pr
    outdata$maq[['processed']] = maq_pr
    outdata$chronicle[['preprocessed']] = chr_pr
    outdata$chronicle[['processed']] = chr_pr
    outdata[['alldata']] = bigdataset
    outdata[['coltypes']] = data_get_coltypes(outdata,
                                              datasets = c("tud_activity", "tud", "maq", "chronicle"))
    outdata[['n_act']] = dim(tud_pr)[1]
    outdata[['n_child']] = dim(bigdataset)[1]
    outdata[['n_nodes']] = length(names(rawdata$tud$nodes)) + length(names(rawdata$maq$edges))
    
    return(outdata)
}

# function to add coltypes to data

data_get_coltypes <-
    function(rawdata,
             datasets = c("tud", "maq", "chronicle"),
             types = c("numeric", "factorial", "boolean")) {
        toremove <-
            c(
                "child_id",
                "respondent_id",
                "nc.SubjectIdentification",
                "day_id",
                "table_access"
            )
        
        coltype = list(
            tud_activity = list(
                factorial = rawdata$tud$processed %>% select(which(sapply(., is.factor))) %>% names %>% setdiff(toremove),
                boolean = rawdata$tud$processed %>% select(which(sapply(., is.logical))) %>% names %>% setdiff(toremove)
            ),
            tud = list(
                factorial = rawdata$tud$summarised %>% select(which(sapply(., is.factor))) %>% names %>% setdiff(toremove),
                boolean = rawdata$tud$summarised %>% select(which(sapply(., is.logical))) %>% names %>% setdiff(toremove)
            ),
            maq = list(
                factorial = rawdata$maq$processed %>% select(which(sapply(., is.factor))) %>% names %>% setdiff(toremove),
                boolean = rawdata$maq$processed %>% select(which(sapply(., is.logical))) %>% names %>% setdiff(toremove)
            ),
            chronicle = list(
                factorial = rawdata$chronicle$processed %>% select(which(sapply(., is.factor))) %>% names %>% setdiff(toremove),
                boolean = rawdata$chronicle$processed %>% select(which(sapply(., is.logical))) %>% names %>% setdiff(toremove)
            )
        )
        
        done = c(
            coltype$tud_activity$factorial,
            coltype$tud_activity$boolean,
            coltype$tud$factorial,
            coltype$tud$boolean,
            coltype$maq$factorial,
            coltype$maq$boolean,
            coltype$chronicle$factorial,
            coltype$chronicle$boolean
        )
        coltype$tud_activity[['numeric']] = setdiff(names(rawdata$tud$processed), c(done, toremove))
        coltype$tud[['numeric']] = setdiff(names(rawdata$tud$summarised), c(done, toremove))
        coltype$maq[['numeric']] = setdiff(names(rawdata$maq$processed), c(done, toremove))
        coltype$chronicle[['numeric']] = setdiff(names(rawdata$chronicle$processed), c(done, toremove))
        
        
        newlist <- list()
        for (dt in datasets) {
            newlist[[dt]] <- list()
            for (tp in types) {
                newlist[[dt]][[tp]] <- coltype[[dt]][[tp]]
            }
        }
        
        return (newlist)
        
    }



get_empty_rawdata <- function() {
    return (
        list(
            tud = list(nodes = list(),
                       edges = list()),
            maq = list(nodes = list(),
                       edges = list()),
            chronicle = list(raw = tibble(),
                             processed = tibble()),
            alldata = tibble(),
            auth = FALSE,
            n_act = 0,
            n_nodes = 0,
            n_child = 0
        )
    )
}
