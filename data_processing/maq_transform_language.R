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
                                                   TRUE),
            WG_8_18_ntotal = mean(n_words, na.rm = TRUE),
            WG_8_18_understands_and_says_percentage = WG_8_18_understands_and_says /
                WG_8_18_ntotal,
            WG_8_18_understands_does_not_say_percentage = WG_8_18_understands_does_not_say /
                WG_8_18_ntotal,
            WG_8_18_does_not_understand_percentage = WG_8_18_does_not_understand /
                WG_8_18_ntotal
        )
    
    WS_18_30 = recombine(list("Children", "VocabularyAssessment_WS_18_30"),
                         rawdata) %>%
        rowwise() %>%
        mutate(
            WS_18_30_split = strsplit(VocabularyAssessment_WS_18_30.ol.vocabularyword, ","),
            WS_18_30_n = length(WS_18_30_split[!is.na(WS_18_30_split)])
        ) %>%
        spread(VocabularyAssessment_WS_18_30.ol.name, WS_18_30_n) %>%
        rename(says = "Yes (says the word)",
               does_not_say = "No (does not say the word)") %>%
        group_by(child_id) %>%
        mutate(n_words = sum(says, does_not_say, na.rm = TRUE)) %>%
        summarise(
            WS_18_30_says = mean(says, na.rm = TRUE),
            WS_18_30_does_not_say = mean(does_not_say, na.rm = TRUE),
            WS_18_30_ntotal = mean(n_words, na.rm = TRUE),
            WS_18_30_says_percentage = WS_18_30_says / WS_18_30_ntotal,
            WS_18_30_does_not_say_percentage = WS_18_30_does_not_say / WS_18_30_ntotal
        )
    
    WS_30_38 = recombine(list("Children", "VocabularyAssessment_WS_30_38"),
                         rawdata) %>%
        rowwise() %>%
        mutate(
            WS_30_38_split = strsplit(VocabularyAssessment_WS_30_38.ol.vocabularyword, ","),
            WS_30_38_n = length(WS_30_38_split[!is.na(WS_30_38_split)])
        ) %>%
        group_by(child_id) %>%
        summarise(WS_30_38_says = mean(WS_30_38_n, na.rm = TRUE))
    
    childlanguage <- WG_8_18 %>%
        left_join(WS_30_38) %>%
        left_join(WS_18_30)
    
    childlanguage = childlanguage %>%
        left_join(children_age_sex) %>%
        rowwise() %>%
        mutate(
            # percentile_wg_8_18 = get_percentiles(WG_8_18_says, age_months, sex, norms$wg),
            # percentile_ws_30_38 = get_percentiles(WS_30_38_says, age_months, sex, voc_prod)
            percentile_ws_18_30 = get_percentiles(
                WS_18_30_says,
                age_months,
                sex,
                rawdata$language_norms$norms_18_30
            )
        )  %>%
        select(-c(sex, age_months))
    
    return(childlanguage)
}


get_percentiles <- function(score, age, sex, norms) {
    cols = names(norms)[str_detect(names(norms), "percentile")]
    ind = max(which(norms$age <= age, norms$sex == sex))
    percentile = cols[which(norms[ind, cols] <= score)] %>%
        str_replace("_percentile", "") %>%
        as.numeric() %>%
        max
    if (!is.finite(percentile)) {
        return(0)
    }
    return(percentile)
}
