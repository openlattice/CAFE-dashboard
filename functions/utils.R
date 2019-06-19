get_demographics <- function(rawdata) {
    dem <- c("parental_education", "parental_employment", "race", "ethnicity", "parental_highest_income", 'parental_least_public_assistance', "sex", "study")
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

get_scales_columns <- function(rawdata, scale = "PSI") {
    if (scale == "short form"){scale = "sf_"}
    columns <-
        data_get_coltypes(rawdata, datasets = c("tud", "maq", "chronicle"), types = c("boolean", "factorial", "numeric"))
    columns = unlist(columns, use.names = FALSE)
    columns = columns[str_detect(columns, scale)]
    return(columns)
}


## combine everything
recombine <- function(nodes, rawdata, joinfnc = left_join, association = NULL) {
    src = rawdata$maq$nodes[[nodes[[1]]]]
    names(src) = paste0(nodes[[1]], ".", names(src))
    dst = rawdata$maq$nodes[[nodes[[2]]]]
    names(dst) = paste0(nodes[[2]], ".", names(dst))
    combined = rawdata$maq$edges[[paste0(nodes[1], "_", nodes[2])]] %>%
        joinfnc(src, by = c(src = paste0(nodes[[1]], ".openlattice.@id"))) %>%
        joinfnc(dst, by = c(dst = paste0(nodes[[2]], ".openlattice.@id"))) %>%
        separate(paste0(nodes[[1]], ".study"),
                 c("not1", 'not2', 'study_id', 'not3'),
                 sep = "_") %>%
        select(-c(not1, not2, not3)) %>% unique()
    
    if (!is.null(association)){
        edge = rawdata$maq$nodes[[association]]
        names(edge) = paste0(association, ".", names(edge))
        combined = combined %>% left_join(edge, by = c(edge = paste0(association, ".openlattice.@id")))
    }
    
    if ("Children.nc.SubjectIdentification" %in% names(combined)) {
        combined = combined %>% rename(child_id = Children.nc.SubjectIdentification)
    }
    if ("Respondents.nc.SubjectIdentification" %in% names(combined)) {
        combined = combined %>% rename(respondent_id = Respondents.nc.SubjectIdentification)
    }
    
    return(combined)
    
}
