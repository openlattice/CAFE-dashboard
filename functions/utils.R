get_demographics <- function(rawdata) {
    dem <- c("parental_education", "parental_employment", "race", "ethnicity", "parental_highest_income", 'parental_least_public_assistance', "study")
    return (dem)
}

get_dataset_from_col <- function(rawdata, column){
    dataset <- case_when(
        column %in% names(rawdata$tud$summarised) ~ "tud",
        column %in% names(rawdata$maq$processed) ~ "maq",
        column %in% names(rawdata$chronicle$processed) ~ "chronicle"
    )
    return(dataset)
}

get_data_from_cols <- function(rawdata, cols) {
    if (!rawdata$auth) {
        return(NULL)
    }

    datasets <- c()
    for (col in cols){
        if (col == "n"){next}
        dataset <- get_dataset_from_col(rawdata, col)
        if (!(dataset %in% datasets)){
            datasets <- c(datasets,dataset)
        }
    }
    
    data <- tibble(child_id = character(), nc.SubjectIdentification = character())
    if ("tud" %in% datasets){data <- data %>% full_join(rawdata$tud$summarised)}
    if ("maq" %in% datasets){data <- data %>% full_join(rawdata$maq$processed)}
    if ("chronicle" %in% datasets){data <- data %>% full_join(rawdata$chronicle$processed)}
    return(data)
}

