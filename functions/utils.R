get_demographics <- function(rawdata) {
    dem <- c("education", "employment", "race", "ethnicity", "income")
    return (dem)
}

get_numeric_vars <- function(rawdata) {
    if (length(rawdata$maq$coltypes$numeric)>1){
        maqcols <- rawdata$maq$coltypes$numeric
    } else {
        maqcols <- c(rawdata$maq$coltypes$numeric)
    }
    
    if (length(rawdata$chronicle$coltypes$numeric)>1){
        chroniclevars <- rawdata$chronicle$coltypes$numeric
    } else {
        chroniclevars <- c(rawdata$chronicle$coltypes$numeric)
    }
    
    c(list(maq = maqcols, tud = rawdata$tud$summarised_coltypes$numeric, chronicle = chroniclevars), "n")
}

get_vars <- function(rawdata, type) {
    out = list()
    toremove <- c("child_id", "respondent_id", "nc.SubjectIdentification", "day_id", "pid", "table_access", "study_id")

    for (kw in c("tud", "maq", "chronicle")) {
        if (length(rawdata[[kw]]$coltypes[[type]])>1){
            cols <- rawdata[[kw]]$coltypes[[type]]
        } else if (length(rawdata[[kw]]$coltypes[[type]])==0) {
            cols <- c()
        } else {
            cols <- c(rawdata[[kw]]$coltypes[[type]])
        }
        out[[kw]] <- setdiff(cols, toremove)
    }
    return(out)
}


get_dataset_from_col <- function(rawdata, column){
    dataset <- case_when(
        column %in% names(rawdata$tud$summarised) ~ "tud",
        column %in% names(rawdata$maq$processed) ~ "maq",
        column %in% names(rawdata$chronicle$processed) ~ "chronicle"
    )
    return(dataset)
}


