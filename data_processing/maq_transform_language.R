childlanguage_transform <- function(rawdata, children_age_sex) {
    WG_8_18 = recombine(list("Children", "VocabularyAssessment_WG_8_18"),
                        rawdata) %>%
        rowwise() %>%
        mutate(
            WG_8_18_split = strsplit(VocabularyAssessment_WG_8_18.ol.vocabularyword, ","),
            WG_8_18_n = length(WG_8_18_split[!is.na(WG_8_18_split)])
        ) %>%
        spread(VocabularyAssessment_WG_8_18.ol.name, WG_8_18_n) %>%
        rename(
            understands_and_says = "Understands the word and says it",
            understands_does_not_say = "Understands the word but does not yet say it",
            does_not_understand = "Does not understand the word",
        ) %>%
        ungroup() %>%
        group_by(child_id) %>%
        mutate(
            n_words = sum(
                understands_and_says,
                understands_does_not_say,
                does_not_understand,
                na.rm = TRUE
            )
        ) %>%
        summarise(
            WG_8_18_understands_and_says = mean(understands_and_says, na.rm = TRUE),
            WG_8_18_understands_does_not_say = mean(understands_does_not_say, na.rm =
                                                        TRUE),
            WG_8_18_does_not_understand = mean(does_not_understand, na.rm =
                                                   TRUE)
        )
    
    WS_18_30 = recombine(list("Children", "VocabularyAssessment_WS_18_30"),
                         rawdata) %>%
        rowwise() %>%
        mutate(
            WS_18_30_split = strsplit(VocabularyAssessment_WS_18_30.ol.vocabularyword, ","),
            WS_18_30_n = length(WS_18_30_split[!is.na(WS_18_30_split)]),
            WS_18_30_n = ifelse(
                is.na(VocabularyAssessment_WS_18_30.ol.vocabularyword),
                NA,
                WS_18_30_n
            )
        ) %>%
        spread(VocabularyAssessment_WS_18_30.ol.name, WS_18_30_n) %>%
        rename(says = "Yes (says the word)",
               does_not_say = "No (does not say the word)") %>%
        ungroup() %>%
        group_by(child_id) %>%
        mutate(n_words = sum(says, does_not_say, na.rm = TRUE)) %>%
        summarise(
            WS_18_30_says = mean(says, na.rm = TRUE),
            WS_18_30_does_not_say = mean(does_not_say, na.rm = TRUE)
        )
    
    WS_30_38 = recombine(list("Children", "VocabularyAssessment_WS_30_38"),
                         rawdata) %>%
        rowwise() %>%
        mutate(
            WS_30_38_split = strsplit(VocabularyAssessment_WS_30_38.ol.vocabularyword, ","),
            WS_30_38_n = length(WS_30_38_split[!is.na(WS_30_38_split)]),
            WS_30_38_n = ifelse(
                is.na(VocabularyAssessment_WS_30_38.ol.vocabularyword),
                NA,
                WS_30_38_n
            )
        ) %>%
        ungroup() %>%
        group_by(child_id) %>%
        summarise(WS_30_38_says = mean(WS_30_38_n, na.rm = TRUE))
    
    childlanguage <- WG_8_18 %>%
        left_join(WS_30_38, by = 'child_id') %>%
        left_join(WS_18_30, by = 'child_id')
    
    childlanguageassessment <- recombine(list("Children", "ChildLanguageAssessment"), rawdata) 
    removecols = c(names(childlanguageassessment)[str_detect(names(childlanguageassessment), "Children")], 
                   "ChildLanguageAssessment.ol.id", 
                   "ChildLanguageAssessment.study",
                   "src", "study_id", 
                   "dst")
    childlanguageassessment <- childlanguageassessment %>% 
        select(-removecols)
    oldnames = childlanguageassessment %>% names
    newnames <- str_replace(oldnames, "ChildLanguageAssessment.ol.|ChildLanguageAssessment.", "")
    childlanguageassessment <- childlanguageassessment %>% rename_at(vars(oldnames), ~newnames)

    childlanguage = childlanguage %>%
        left_join(children_age_sex, by = 'child_id') %>%
        rowwise() %>%
        mutate(
            # percentile_wg_8_18 = get_percentiles(WG_8_18_says, age_months, sex, norms$wg),
            # percentile_ws_30_38 = get_percentiles(WS_30_38_says, age_months, sex, voc_prod)
            WS_18_30_percentile = get_percentiles(
                WS_18_30_says,
                age_months,
                sex,
                rawdata$language_norms$norms_18_30
            )
        )  %>%
        ungroup() %>%
        select(-c(sex, age_months, table_access)) %>%
        full_join(childlanguageassessment, by = "child_id")
    
    return(childlanguage)
}


get_percentiles <- function(score, age, sex, norms) {
    cols = names(norms)[str_detect(names(norms), "percentile")]
    if (is.na(age) | is.na(sex)) {
        return (NA)
    }
    ind = max(which(norms$age <= age, norms$sex == sex))
    percentiles = cols[which(norms[ind, cols] <= score)] %>%
        str_replace("_percentile", "") %>%
        as.numeric()
    if (length(percentiles) == 0) {
        return (0)
    } else {
        return (max(percentiles))
    }
    return(percentile)
}
