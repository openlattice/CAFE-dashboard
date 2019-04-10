## combine everything
process_maq <- function(rawdata) {
    if (rawdata$auth == FALSE) {
        return (tibble())
    }
    
    children = rawdata$maq$edges$Respondents_Children %>%
        full_join(rawdata$maq$nodes$Respondents,
                  by = c(src = "openlattice.@id")) %>%
        rename(respondent_id = nc.SubjectIdentification) %>%
        left_join(rawdata$maq$nodes$Children, by = c(dst = "openlattice.@id")) %>%
        rename(child_id = nc.SubjectIdentification) %>%
        mutate(table_access = (table_access.x & table_access.y)) %>%
        separate(study.x, c("not1", 'not2', 'study_id', 'not3'), sep = "_") %>%
        select(-c(not1, not2, not3)) %>%
        select(-c(
            table_access.x,
            table_access.y,
            ol.birthmonth,
            ol.birthyear
        )) %>%
        group_by(child_id) %>% slice(1) %>% ungroup()
    
    deviceuse = rawdata$maq$edges$Children_Device_Use %>%
        left_join(rawdata$maq$nodes$Children, by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$Device_Use, by = c(dst = "openlattice.@id")) %>%
        rename(child_id = nc.SubjectIdentification) %>%
        select(-c(table_access.x, table_access.y)) %>%
        # rowwise() %>%
        mutate(
            mn = ol.duration,
            mn = str_replace(mn, "30 minutes to 1 hour", "0.75"),
            mn = str_replace(mn, "1-2 hours", "1.5"),
            mn = str_replace(mn, "2-3 hours", "2.5"),
            mn = str_replace(mn, "3-4 hours", "3.5"),
            mn = str_replace(mn, "4-5 hours", "4.5"),
            mn = str_replace(mn, "More than 5 hours", "5"),
            mn = as.numeric(mn)
            ) %>%
        group_by(child_id) %>% summarise(
            sf_Q1_mediahours_weekday = sum(mn[
                str_detect(ol.description, "TV or DVDs|computer|smartphone|mobile device|console video|iPad|virtual assistant") &
                    str_detect(ol.description, "weekday") &
                    str_detect(ol.description, "Time spent")
                ], na.rm=TRUE),
            sf_Q1_mediahours_weekend = sum(mn[
                str_detect(ol.description, "TV or DVDs|computer|smartphone|mobile device|console video|iPad|virtual assistant") &
                    str_detect(ol.description, "weekend") &
                    str_detect(ol.description, "Time spent")
                ], na.rm=TRUE),
            sf_Q3_noscreenmediahours_weekday = sum(mn[
                str_detect(ol.description, "paper books|electronic books") &
                    str_detect(ol.description, "weekday") &
                    str_detect(ol.description, "Time spent")
                ], na.rm=TRUE),
            sf_Q3_noscreenmediahours_weekend = sum(mn[
                str_detect(ol.description, "paper books|electronic books") &
                    str_detect(ol.description, "weekend") &
                    str_detect(ol.description, "Time spent")
                ], na.rm=TRUE),
            sf_Q2_no_media_bedtime = sum(str_detect(ol.description, "hour before bedtime") &
                              str_detect(general.frequency, "Never|Less than once a week"),na.rm=TRUE)>0

        )

    childrendetails = rawdata$maq$edges$Children_ChildrenDetails %>%
        full_join(rawdata$maq$nodes$Children, by = c(src = "openlattice.@id")) %>%
        rename(child_id = nc.SubjectIdentification) %>%
        drop_na(child_id) %>%
        left_join(rawdata$maq$nodes$ChildrenDetails,
                  by = c(dst = "openlattice.@id")) %>%
        select(-c(table_access.x, table_access.y)) %>%
        mutate(
            birthmonth = match(str_sub(ol.birthmonth, 1, 3), month.abb),
            birthmonth = ifelse(is.na(birthmonth), as.numeric(ol.birthmonth), birthmonth),
            birthyear = ifelse(
                is.na(ol.birthyear),
                2018 - as.numeric(person.ageatevent),
                as.numeric(ol.birthyear)
            )
        ) %>% arrange(child_id, birthyear) %>%
        group_by(child_id) %>% slice(1) %>% ungroup()
    
    incomes = rawdata$maq$edges$Respondents_Incomes %>%
        left_join(rawdata$maq$nodes$Respondents, by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$Incomes, by = c(dst = "openlattice.@id"))%>%
        rename(
            respondent_id = nc.SubjectIdentification,
            study = study.x,
            income = ol.type
        ) %>%
        select(-c(table_access.x, table_access.y)) %>%
        filter(str_detect(income, "$")) %>%
        group_by(respondent_id) %>% slice(1) %>% ungroup()
    
    employment = rawdata$maq$edges$Respondents_Employment %>%
        left_join(rawdata$maq$nodes$Respondents,
                  by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$Employment,
                  by = c(dst = "openlattice.@id")) %>%
        select(
            'nc.PersonEthnicity',
            'nc.PersonRace',
            'nc.SubjectIdentification',
            'study.x',
            'ol.status',
            'table_access.x',
            'table_access.y'
        ) %>%
        rename(
            respondent_id = nc.SubjectIdentification,
            ethnicity = nc.PersonEthnicity,
            race = nc.PersonRace,
            study = study.x,
            employment = ol.status
        ) %>%
        mutate(table_access = (table_access.x & table_access.y)) %>%
        select(-c(table_access.x, table_access.y)) %>%
        group_by(respondent_id) %>% 
        filter(row_number() == n()) %>% 
        mutate(race = ifelse(str_detect(race, ","), "Biracial", race))
    
    education = rawdata$maq$edges$Respondents_Education %>%
        left_join(rawdata$maq$nodes$Respondents,
                  by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$Education, by = c(dst = "openlattice.@id")) %>%
        select(
            'nc.SubjectIdentification',
            'study.x',
            'person.highesteducation',
            'table_access.x',
            'table_access.y'
        ) %>%
        rename(respondent_id = nc.SubjectIdentification,
               study = study.x,
               education = person.highesteducation) %>%
        select(-c(table_access.x, table_access.y)) %>%
        group_by(respondent_id) %>% slice(1) %>% ungroup()
    
    quality = rawdata$maq$edges$Respondents_QualityControl %>%
        left_join(rawdata$maq$nodes$Respondents,
                  by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$QualityControl, by = c(dst = "openlattice.@id")) %>%
        select(-c(table_access.x, table_access.y))%>%
        mutate(correct = str_detect(ol.description, regex(ol.selection, ignore_case=TRUE))) %>%
        rename(respondent_id = nc.SubjectIdentification,
               study = study.x)  %>%
        group_by(respondent_id) %>%
        summarise(
            n_quality = n(),
            mean_quality = mean(correct, na.rm=TRUE)
        )
    
    defensive_vars = c('ol.overwhelmed',
                           'ol.parentalresponsibilities',
                           'ol.feelingtrapped',
                           # 'ol.dissatisfiedwithlife',
                           'ol.relationshipissues',
                           'ol.loneliness',
                           'ol.socialdisinterest')
    pd_vars = c('ol.overwhelmed',
                    'ol.parentalresponsibilities',
                    'ol.feelingtrapped',
                    'ol.opportunitycost',
                    'ol.restrictedactivities',
                    'ol.dissatisfiedwithpurchase',
                    # 'ol.dissatisfiedwithlife',
                    'ol.relationshipissues',
                    'ol.loneliness',
                    'ol.expectations',
                    'ol.socialdisinterest',
                    'ol.anhedonia')
    pcdi_vars = c('ol.dissatisfiedwithparenting', 
                      'ol.emotionallydistant',
                      'ol.facialexpression',
                      'ol.unappreciated',
                      'ol.laughter',
                      'ol.relativespeed',
                      'ol.relativefrequency', 
                      'ol.limitations',
                      'ol.behaviorchange', 
                      'ol.parentingquality', 
                      'ol.strengthofemotion', 
                      'ol.behavioralissue')
    dc_vars = c('ol.affect',
                    'bhr.disposition',
                    'ol.mood' , 
                    'ol.botheredbyperson' , 
                    'ol.reactionseverity' ,
                    'ol.easilyupset' , 
                    'neurological.sleeppattern' , 
                    'ol.discipline', 
                    # 'ol.number', 
                    'ol.botheredbyactions' , 
                    'ol.moreproblematic' , 
                    'ol.demanding')
    
    
    level_key_1 <- c(
        "Strongly disagree" = '1',
        "Disagree" = '2',
        "Not Sure" = '3',
        "Agree" = '4',
        "Strongly Agree" = '5'
    )
    level_key_2 <- c(
        "A very good parent" = '1',
        "A better than average parent" = '2',
        "An average parent" = '3',
        "A person who has some trouble being a parent" = '4',
        "Not very good at being a parent" = '5'
    )
    level_key_3 <- c("Much easier than I expected" = '1', 
                     "Somewhat easier than I expected" = '2', 
                     "About as hard as I expected" = '3', 
                     "Somewhat harder than I expected" = '4', 
                     "Much harder than I expected" = '5')
    
    
    psi = rawdata$maq$edges$Respondents_PSI_Assessment %>%
        left_join(rawdata$maq$nodes$Respondents,
                  by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$PSI_Assessment, by = c(dst = "openlattice.@id")) %>%
        select(-c(table_access.x, table_access.y)) %>%
        dplyr::rename(respondent_id = nc.SubjectIdentification, study = study.x)  %>%
        group_by(respondent_id) %>% slice(1) %>%
        mutate_at(vars(c(defensive_vars, pd_vars, pcdi_vars, dc_vars)), recode, !!!level_key_1) %>%
        mutate_at(vars(c(defensive_vars, pd_vars, pcdi_vars, dc_vars)), recode, !!!level_key_2) %>%
        mutate_at(vars(c(defensive_vars, pd_vars, pcdi_vars, dc_vars)), recode, !!!level_key_3) %>%
        mutate_at(vars(c(defensive_vars, pd_vars, pcdi_vars, dc_vars)), as.numeric) %>% 
        plyr::mutate(
            defensive_n_missing = apply(.[,defensive_vars], 1, function(x) sum(is.na(x))),
            defensive = apply(.[,defensive_vars], 1, function(x) mean(x)),
            defensive_cor = ifelse(defensive_n_missing < 2, defensive, NA),
            pd_n_missing = apply(.[,pd_vars], 1, function(x) sum(is.na(x))),
            pd = apply(.[,pd_vars], 1, function(x) mean(x)),
            pd_cor = ifelse(pd_n_missing < 2, pd, NA),
            pcdi_n_missing = apply(.[,pcdi_vars], 1, function(x) sum(is.na(x))),
            pcdi = apply(.[,pcdi_vars], 1, function(x) mean(x)),
            pcdi_cor = ifelse(pcdi_n_missing < 2, pcdi, NA),
            dc_n_missing = apply(.[,dc_vars], 1, function(x) sum(is.na(x))),
            dc = apply(.[,dc_vars], 1, function(x) mean(x)),
            dc_cor = ifelse(dc_n_missing < 2, dc, NA),
            psi_n_missing = apply(.[,c(pd_vars, pcdi_vars, dc_vars)], 1, function(x) sum(is.na(x))),
            psi = apply(.[,c(pd_vars, pcdi_vars, dc_vars)], 1, function(x) mean(x)),
            psi_cor = ifelse(dc_n_missing < 2, psi, NA)
        )


    maq = children %>%
        left_join(childrendetails, by = "child_id") %>%
        left_join(employment, by = 'respondent_id') %>%
        left_join(education, by = "respondent_id") %>%
        left_join(deviceuse, by = "child_id") %>%
        left_join(quality, by = "respondent_id") %>%
        left_join(incomes, by = "respondent_id") %>%
        left_join(psi, by = "respondent_id")
    
    maq <- maq %>% mutate(
        birthmonth = ifelse(is.na(ol.birthmonth), ol.birthmonth, ol.birthmonth),
        birthmonth_num = match(str_sub(birthmonth, 1, 3), month.abb),
        birthyear = as.numeric(ifelse(
            is.na(person.ageatevent),
            ifelse(is.na(ol.birthyear), ol.birthyear, ol.birthyear),
            2018 - as.numeric(person.ageatevent)
        )),
        birthmonth_num = ifelse(is.na(birthmonth_num), 1, birthmonth_num),
        age_months = as.numeric((ymd("2018-06-06") - ymd(
            paste0(birthyear, "-", birthmonth_num, "-", 1)
        )) / (365 / 12))
    ) %>%
        mutate(
            table_access = table_access.x,
            nc.SubjectIdentification = child_id,
            study = study_id
            ) %>% 
        select(
                       "education",
                       "age_months",
                       "employment",
                       "ethnicity",
                       'race',
                       'child_id',
                       "respondent_id",
                       "study",
                       "study_id",
                       "nc.SubjectIdentification",
                       "table_access",
                       'income',
                       n_quality,
                       mean_quality,
                       sf_Q1_mediahours_weekday,
                       sf_Q1_mediahours_weekend,
                       sf_Q2_no_media_bedtime,
                       sf_Q3_noscreenmediahours_weekday,
                       sf_Q3_noscreenmediahours_weekend,
                       defensive_cor,
                       pd_cor,
                       pcdi_cor,
                       dc_cor,
                       psi_cor
                   )
    
    # factor vars to factor
    ndist <- maq %>%
        summarise_all(funs(n_distinct(.)))
    colndistinct <- as_tibble(cbind(nms = names(ndist), t(ndist)))
    names(colndistinct) <- c("nms", "cnt")
    colndistinct <- colndistinct %>% mutate(nums = as.numeric(cnt))
    factcols <-
        colndistinct %>% filter(nums >= 4) %>% filter(nums <= 10)
    boolcols <- colndistinct %>% filter(nums <= 2)
    
    maq <- maq %>%
        mutate_at(factcols$nms, as.factor) %>%
        mutate_at(boolcols$nms, as.logical)
    
    maq <- maq %>%
        mutate_at(c("mean_quality"), as.numeric)

    return(maq)
}
