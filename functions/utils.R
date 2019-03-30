get_demographics <- function(rawdata) {
    dem <- c("education", "employment", "race", "ethnicity")
    return (dem)
}

get_vars <- function(rawdata) {
    c(list(maq = rawdata$maq$coltypes$numeric, tud = rawdata$tud$summarised_coltypes$numeric))
}


get_dataset_from_col <- function(rawdata, column){
    dataset <- case_when(
        column %in% rawdata$tud$summarised_coltypes$numeric ~ "tud",
        column %in% rawdata$maq$coltypes$numeric ~ "maq"
    )
    return(dataset)
}

get_demographics_data <- function(rawdata, column, remove_missing_maq) {
    if (!rawdata$auth) {
        return(NULL)
    }
    dataset <- get_dataset_from_col(rawdata, column)
    if (dataset == "maq"){
        data <- rawdata$maq$processed
        return(data)
    }
    if (remove_missing_maq) {
        data <- rawdata$maq$processed %>% full_join(rawdata$tud$summarised, 
                                                    by="nc.SubjectIdentification")
        return(data)
    }
    data <- rawdata$maq$processed %>% left_join(rawdata$tud$summarised, 
                                                by="nc.SubjectIdentification")
    return(data)
}

