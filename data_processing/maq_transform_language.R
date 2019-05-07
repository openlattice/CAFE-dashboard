childlanguage_transform <- function(rawdata, children, childrendetails) {
    WG_8_18 = recombine(list("Respondents", "VocabularyAssessment_WG_8_18"), rawdata) %>%
        group_by(respondent_id) %>%
        summarise(
            WG_8_18_says = sum(str_detect(VocabularyAssessment_WG_8_18.ol.name, "says it")),
            WG_8_18_understands = sum(str_detect(VocabularyAssessment_WG_8_18.ol.name, "Understands")),
            n_words = n()
        ) %>%
        group_by(child_id) %>%
        summarise(
            WG_8_18_says = mean(WG_8_18_says),
            WG_8_18_understands = mean(WG_8_18_understands),
            n_words = n()
        )
            
    WS_30_38 = recombine(list("Respondents", "VocabularyAssessment_WS_30_38"), rawdata) %>%
        left_join(children) %>%
        mutate(splits = strsplit(VocabularyAssessment_WS_30_38.ol.vocabularyword, ",")) %>%
        rowwise() %>%
        mutate(nwords = length(splits[!is.na(splits)])) %>%
        group_by(respondent_id, child_id) %>%
        summarise(
            WS_30_38_says = first(nwords)/100
        ) %>%
        group_by(child_id) %>%
        summarise(
            WS_30_38_says = mean(WS_30_38_says)
        )
    
    WS_18_30 = recombine(list("Respondents", "VocabularyAssessment_WS_18_30"), rawdata) %>%
        left_join(children) %>%
        group_by(respondent_id, child_id) %>%
        summarise(
            WS_18_30_says = sum(str_detect(VocabularyAssessment_WS_18_30.ol.name, "says the word")) / 50
        ) %>%
        group_by(child_id) %>%
        summarise(
            WS_18_30_says = mean(WS_18_30_says)
        )
    
    childlanguage <- WG_8_18 %>%
        left_join(WS_30_38) %>%
        left_join(WS_18_30)
    
    childlanguage = childlanguage %>%
        left_join(childrendetails) %>%
        rowwise() %>% 
        mutate(
            percentile_wg_8_18 = get_percentiles(WG_8_18_says, age_months, norms$wg),
            percentile_ws_30_38 = get_percentiles(WS_30_38_says, age_months, norms$ws),
            percentile_ws_18_30 = get_percentiles(WS_18_30_says, age_months, norms$ws)
        )  %>% 
        select(
            child_id,
            percentile_wg_8_18,
            percentile_ws_30_38,
            percentile_ws_18_30,
            WG_8_18_says,
            WG_8_18_understands,
            WS_30_38_says,
            WS_18_30_says
        )
    
    return(childlanguage)
}


get_percentiles <- function(score, age, norms) {
    cols = names(norms)[str_detect(names(norms), "percentile")]
    ind = max(which(norms$age <= age))
    percentile = cols[which(norms[ind,cols]<= score)] %>% 
        str_replace("_percentile", "") %>% 
        as.numeric() %>% 
        max
    if (!is.finite(percentile)){return(0)}
    return(percentile)
}

