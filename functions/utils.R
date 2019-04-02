get_demographics <- function(rawdata) {
    dem <- c("education", "employment", "race", "ethnicity")
    return (dem)
}

get_vars <- function(rawdata) {
    if (length(rawdata$maq$coltypes$numeric)>1){
        maqcols <- rawdata$maq$coltypes$numeric
    } else {
        maqcols <- c(rawdata$maq$coltypes$numeric)
    }
    c(list(maq = maqcols, tud = rawdata$tud$summarised_coltypes$numeric), "n")
}


get_dataset_from_col <- function(rawdata, column){
    dataset <- case_when(
        column %in% rawdata$tud$summarised_coltypes$numeric ~ "tud",
        column %in% rawdata$maq$coltypes$numeric ~ "maq"
    )
    return(dataset)
}


