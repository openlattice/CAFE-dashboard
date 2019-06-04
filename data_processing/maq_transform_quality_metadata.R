qa_transform <- function(rawdata, children) {
    quality = recombine(list("Respondents", "QualityControl"), rawdata) %>%
        left_join(children, by = 'respondent_id') %>%
        mutate(correct = str_detect(
            QualityControl.ol.description,
            regex(QualityControl.ol.selection, ignore_case = TRUE)
        )) %>%
        group_by(child_id) %>%
        summarise(n_quality = n(),
                  mean_quality = mean(correct, na.rm = TRUE))
    
    return(quality)
}


metadata_transform <- function(rawdata, children) {
    metadata = recombine(list("Respondents", "SurveyMetadata"), rawdata) %>%
        left_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarise(date_maq = first(ymd_hms(SurveyMetadata.ol.recordeddate))) %>%
        select(child_id, date_maq)
    return(metadata)
}
