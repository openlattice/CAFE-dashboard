## combine everything

recombine <- function(nodes, rawdata, joinfnc = left_join) {
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
    
    if ("Children.nc.SubjectIdentification" %in% names(combined)) {
        combined = combined %>% rename(child_id = Children.nc.SubjectIdentification)
    }
    if ("Respondents.nc.SubjectIdentification" %in% names(combined)) {
        combined = combined %>% rename(respondent_id = Respondents.nc.SubjectIdentification)
    }
    
    return(combined)
    
}

process_maq <- function(rawdata) {
    if (rawdata$auth == FALSE) {
        return (tibble())
    }
    
    ######
    ## Children
    ######
    
    race_key <-
        c(
            'amindian' = "Native American / Pacific Islander",
            "black" = 'Black',
            "biracial" = 'Biracial',
            "asian" = 'Asian',
            "white" = 'White'
        )
    
    children_demographics = recombine(list("Respondents", "Children"), rawdata) %>%
        select(
            child_id,
            respondent_id,
            Respondents.nc.PersonRace,
            Respondents.nc.PersonEthnicity,
            study_id,
            Children.table_access,
            Children.nc.PersonSex
        ) %>%
        group_by(child_id,
                 study_id,
                 Children.table_access,
                 Children.nc.PersonSex) %>%
        summarise(
            race = paste(unique(Respondents.nc.PersonRace[!is.na(Respondents.nc.PersonRace)]), collapse =
                             ","),
            ethnicity = paste(unique(Respondents.nc.PersonEthnicity[!is.na(Respondents.nc.PersonEthnicity)]), collapse = ",")
        ) %>%
        mutate(
            race = ifelse(str_detect(race, ","), "biracial", race),
            race = ifelse(str_detect(race, "NA"), NA, race),
            ethnicity = ifelse(str_detect(ethnicity, ","), "multiple", ethnicity),
            ethnicity = ifelse(ethnicity == "", NA, ethnicity)
        ) %>%
        rename(table_access = Children.table_access,
               sex = Children.nc.PersonSex) %>%
        mutate_at('race', recode,!!!race_key)
    
    children = recombine(list("Respondents", "Children"), rawdata) %>%
        select(child_id, respondent_id, study_id)
    
    ######
    ## Devices
    ######
    
    devices = recombine(list("Devices", "Children"), rawdata)
    mediauseattitudes = recombine(list("Respondents", "MediaUseAttitudes"), rawdata) %>%
        left_join(children, by = 'respondent_id')
    devices_mediauseattitudes = devices %>%
        left_join(mediauseattitudes, by = "child_id") %>%
        group_by(child_id) %>%
        summarise(sf_maq_Q10_uses_videochat_a = sum(
            str_detect(Devices.ol.id, "videochat") &
                str_detect(
                    MediaUseAttitudes.ol.levelofagreement,
                    "Sometimes|Often|Very often|Very Frequently"
                )
        ) > 0) %>%
        select(child_id, sf_maq_Q10_uses_videochat_a)
    
    
    ######
    ## Videochat
    ######
    
    videochat = recombine(list("Respondents", "MediaUseAttitudes"), rawdata) %>%
        full_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarise(
            uses_videochat_relatives = sum(
                str_detect(MediaUseAttitudes.ol.reason, "distant relatives") &
                    str_detect(
                        MediaUseAttitudes.ol.levelofagreement,
                        "Sometimes|Often|Very often|Very Frequently"
                    )
            ) > 0,
            uses_videochat_relatives_text = paste0(MediaUseAttitudes.ol.levelofagreement[str_detect(MediaUseAttitudes.ol.reason, "distant relatives")], collapse = ", "),
            uses_videochat_see_parents = sum(
                str_detect(MediaUseAttitudes.ol.reason, "see parents") &
                    str_detect(
                        MediaUseAttitudes.ol.levelofagreement,
                        "Sometimes|Often|Very often|Very Frequently"
                    )
            ) > 0,
            uses_videochat_see_parents_text = paste0(MediaUseAttitudes.ol.levelofagreement[str_detect(MediaUseAttitudes.ol.reason, "see parents")], collapse = ", "),
            uses_videochat_connect_day = sum(
                str_detect(MediaUseAttitudes.ol.reason, "throughout the day") &
                    str_detect(
                        MediaUseAttitudes.ol.levelofagreement,
                        "Sometimes|Often|Very often|Very Frequently"
                    )
            ) > 0,
            uses_videochat_connect_day_text = paste0(MediaUseAttitudes.ol.levelofagreement[str_detect(MediaUseAttitudes.ol.reason, "throughout the day")], collapse = ", "),
            uses_videochat_connect_work = sum(
                str_detect(MediaUseAttitudes.ol.reason, "because of work schedule") &
                    str_detect(
                        MediaUseAttitudes.ol.levelofagreement,
                        "Sometimes|Often|Very often|Very Frequently"
                    )
            ) > 0,
            uses_videochat_connect_work_text = paste0(MediaUseAttitudes.ol.levelofagreement[str_detect(MediaUseAttitudes.ol.reason, "because of work schedule")], collapse = ", ")
        ) %>%
        select(
            child_id,
            uses_videochat_relatives,
            uses_videochat_relatives_text,
            uses_videochat_see_parents,
            uses_videochat_see_parents_text,
            uses_videochat_connect_day,
            uses_videochat_connect_day_text,
            uses_videochat_connect_work,
            uses_videochat_connect_work_text
        )
    
    ######
    ## Media device use/ Parent Attitudes
    ######
    
    mediadeviceuse = recombine(list("Respondents", "MediaDeviceUse"), rawdata) %>%
        full_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarise(
            sf_maq_Q11_avoid_media_for_calming = sum(
                str_detect(MediaDeviceUse.ol.reason, "calm child down") &
                    str_detect(MediaDeviceUse.general.frequency, "Never")
            ) > 0,
            calming_down = paste0(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "calm child down")], collapse = ", "),
            use_media_for_educating = sum(
                str_detect(MediaDeviceUse.ol.reason, "educate child") &
                    !str_detect(
                        MediaDeviceUse.general.frequency,
                        "Never|Less than once per week"
                    )
            ) > 0,
            educating_child = paste0(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "educate child")], collapse = ", "),
            avoid_media_for_keeping_child_busy = sum(
                str_detect(MediaDeviceUse.ol.reason, "keep child busy") &
                    str_detect(MediaDeviceUse.general.frequency,
                               "Never")
            ) > 0,
            keeping_child_busy = paste0(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "keep child busy")], collapse = ", "),
            use_media_for_communicating = sum(
                str_detect(MediaDeviceUse.ol.reason, "communicate") &
                    !str_detect(
                        MediaDeviceUse.general.frequency,
                        "Never|Less than once per week"
                    )
            ) > 0,
            communicating = paste0(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "communicate")], collapse = ", "),
            use_media_for_enjoying = sum(
                str_detect(MediaDeviceUse.ol.reason, "enjoys using") &
                    !str_detect(
                        MediaDeviceUse.general.frequency,
                        "Never|Less than once per week"
                    )
            ) > 0,
            enjoying = paste0(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "enjoys using")], collapse = ", ")
        ) %>%
        select(
            child_id,
            sf_maq_Q11_avoid_media_for_calming,
            calming_down,
            use_media_for_educating,
            educating_child,
            avoid_media_for_keeping_child_busy,
            keeping_child_busy,
            use_media_for_communicating,
            communicating,
            use_media_for_enjoying,
            enjoying
        )
    
    
    ######
    ## Metadata
    ######
    
    metadata = recombine(list("Respondents", "SurveyMetadata"), rawdata) %>%
        left_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarise(date_maq = first(ymd_hms(SurveyMetadata.ol.recordeddate))) %>%
        select(child_id, date_maq)
    
    ######
    ## Children details
    ######
    
    childrendetails = recombine(list("Children", "ChildrenDetails"), rawdata, joinfnc = full_join) %>%
        mutate(
            birthmonth = match(str_sub(Children.ol.birthmonth, 1, 3), month.abb),
            birthyear = ifelse(
                is.na(Children.ol.birthyear),
                2018 - as.numeric(ChildrenDetails.person.ageatevent),
                as.numeric(Children.ol.birthyear)
            ),
            age = ChildrenDetails.person.ageatevent
        ) %>% arrange(child_id, birthyear) %>%
        group_by(child_id) %>% slice(1) %>% ungroup() %>%
        select(birthmonth, birthyear, child_id, age) %>%
        left_join(metadata, by = 'child_id') %>%
        mutate(
            fulldate = paste(birthyear, birthmonth, "01", sep = "-"),
            fulldate = ifelse("NA"%in% fulldate, NA, fulldate),
            birthdate = ymd(fulldate),
            age_months = time_length(date(date_maq) - ymd(birthdate), unit =
                                         "month"),
            age_months = ifelse(is.na(age_months), as.numeric(age) * 12, age_months)
        ) %>%
        select(-c(date_maq))
    
    ######
    ## Children details health
    ######
    
    birthweight_gram_key <- c(
        "Less than 2 pounds" = '750',
        '2 lbs - 3 lbs, 5 oz' = "1250",
        "1000-1500 grams/2lbs 3oz-3lbs 5oz" = '1250',
        "3 lbs, 6 oz - 4 lbs, 7 oz" = '1750',
        '1501-2000 grams/3lbs 5oz-4lbs 6oz' = '1750',
        "4 lbs, 8 oz - 5 lbs, 8 oz" = '2250',
        "2001-2500 grams/4lbs 6oz-5lbs 8oz" = '2250',
        "5 lbs, 9 oz - 6 lbs, 9 oz" = '2750',
        "2501-3000 grams/5lbs 8oz-6lbs 9oz" = '2750',
        "6 lbs, 10 oz or above" = '3250',
        "more than 3000 grams/6lbs 9oz" = '3250'
    )
    
    birthweight_pound_key <- c(
        "Less than 2 pounds" = '1.65',
        '2 lbs - 3 lbs, 5 oz' = "2.76",
        "1000-1500 grams/2lbs 3oz-3lbs 5oz" = '2.76',
        "3 lbs, 6 oz - 4 lbs, 7 oz" = '3.86',
        '1501-2000 grams/3lbs 5oz-4lbs 6oz' = '3.86',
        "4 lbs, 8 oz - 5 lbs, 8 oz" = '4.96',
        "2001-2500 grams/4lbs 6oz-5lbs 8oz" = '4.96',
        "5 lbs, 9 oz - 6 lbs, 9 oz" = '6.06',
        "2501-3000 grams/5lbs 8oz-6lbs 9oz" = '6.06',
        "6 lbs, 10 oz or above" = '7.17',
        "more than 3000 grams/6lbs 9oz" = '7.17'
    )
    
    childrendetailshealth = recombine(list("Children", "ChildrenDetailsHealth"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            birthweight = first(ChildrenDetailsHealth.ol.birthweight),
            premature = first(ChildrenDetailsHealth.ol.prematurebirth),
            gestationalage = as.numeric(first(
                ChildrenDetailsHealth.ol.gestationalage
            ))
        ) %>%
        mutate(
            premature = ifelse(premature == "yes", TRUE, FALSE),
            birthweight_g_recoded = recode(birthweight,!!!birthweight_gram_key),
            birthweight_lbs_recoded = recode(birthweight,!!!birthweight_pound_key)
        ) %>%
        mutate(
            birthweight_g = as.numeric(birthweight_g_recoded),
            birthweight_lbs = as.numeric(birthweight_lbs_recoded)
        ) %>%
        select(
            child_id,
            birthweight,
            birthweight_lbs,
            birthweight_g,
            premature,
            gestationalage
        )
    
    ######
    ## Parent checks
    ######
    
    quality = recombine(list("Respondents", "QualityControl"), rawdata) %>%
        left_join(children, by = 'respondent_id') %>%
        mutate(correct = str_detect(
            QualityControl.ol.description,
            regex(QualityControl.ol.selection, ignore_case = TRUE)
        )) %>%
        group_by(child_id) %>%
        summarise(n_quality = n(),
                  mean_quality = mean(correct, na.rm = TRUE))
    
    
    respondentdetails <- respondentdetails_transform(rawdata, children)
    incomes <- incomes_transform(rawdata, children)
    public_assistance <- public_assistance_transform(rawdata, children)
    employment <- employment_transform(rawdata, children)
    education <- education_transform(rawdata, children)
    parentsmediause <- parentsmediause_transform(rawdata, children)
    parentsmediaexposure <- parentsmediaexposure_transform(rawdata, children)
    parentmediaattitudes <- parentsmediaattitudes_transform(rawdata, children)
    parentsmediation <- parentsmediation_transform(rawdata, children)
    deviceuse <- deviceuse_transform(rawdata)
    psi <- psi_transform(rawdata, children)
    pm <- pm_transform(rawdata, children)

    children_age_sex = childrendetails %>% left_join(children_demographics, by = 'child_id') %>% select(c(sex, age_months, child_id))
    childlanguage <-
        childlanguage_transform(rawdata, children_age_sex)
    
    maq <- children_demographics %>%
        left_join(devices_mediauseattitudes, by = "child_id") %>%
        left_join(videochat, by = "child_id") %>%
        left_join(mediadeviceuse, by = "child_id") %>%
        left_join(childrendetails, by = "child_id") %>%
        left_join(respondentdetails, by = "child_id") %>%
        left_join(incomes, by = "child_id") %>%
        left_join(public_assistance, by = "child_id") %>%
        left_join(metadata, by = "child_id") %>%
        left_join(employment, by = "child_id") %>%
        left_join(education, by = "child_id") %>%
        left_join(quality, by = "child_id") %>%
        left_join(parentsmediation, by = "child_id") %>%
        left_join(parentsmediause, by = "child_id") %>%
        left_join(parentsmediaexposure, by = "child_id") %>%
        left_join(parentmediaattitudes, by = "child_id") %>%
        left_join(deviceuse, by = "child_id") %>%
        left_join(psi, by = "child_id") %>%
        left_join(pm, by = "child_id")  %>%
        left_join(childrendetailshealth, by = "child_id") %>%
        left_join(childlanguage, by = "child_id")
    
    
    # factor vars to factor
    numericcols <- maq %>% select_if(is.numeric) %>% names()
    ndist <- lapply(maq, n_distinct)
    colndistinct <-
        as_tibble(cbind(nms = names(ndist), unlist(ndist)))
    names(colndistinct) <- c("nms", "cnt")
    colndistinct <- colndistinct %>% mutate(nums = as.numeric(cnt))
    factcols <-
        colndistinct %>% filter(nums >= 4) %>% filter(nums <= 10) %>% filter(!(nms %in% numericcols))
    boolcols <-
        colndistinct %>% filter(nums <= 3) %>% filter(!(nms %in% numericcols))
    
    maq <- maq %>%
        ungroup() %>%
        mutate_at(factcols$nms, as.factor) %>%
        mutate_at(boolcols$nms, as.logical) %>%
        mutate(study = study_id)
    
    maq <- maq %>%
        mutate_at(c("mean_quality"), as.numeric) %>%
        drop_na(child_id)
    
    return(maq)
}
