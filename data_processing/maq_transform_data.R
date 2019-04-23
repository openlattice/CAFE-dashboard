## combine everything
process_maq <- function(rawdata) {
    if (rawdata$auth == FALSE) {
        return (tibble())
    }
    
    ######
    ## Children variables
    ######
    
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
        select(-c(table_access.x, table_access.y, nc.PersonSex)) %>%
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
    
    children = children %>%
        left_join(childrendetails, by = "child_id") %>%
        left_join(deviceuse, by = "child_id")
    
    children_respondents = children %>% select(child_id, respondent_id)
    
    ######
    ## Respondent variables
    ######
    
    respondentdetails = rawdata$maq$edges$Respondents_RespondentDetails %>%
        full_join(rawdata$maq$nodes$Respondents, by = c(src = "openlattice.@id")) %>%
        rename(respondent_id = nc.SubjectIdentification) %>%
        full_join(rawdata$maq$nodes$RespondentDetails,
                  by = c(dst = "openlattice.@id")) %>%
        select(-c(table_access.x, table_access.y)) %>%
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>% 
        summarise(
            parental_mean_age = mean(as.numeric(person.ageatevent), na.rm=TRUE),
            parental_mean_numchildren = sum(as.numeric(ol.numberofchildren)),
            parental_marital = paste(unique(person.maritalstatus), collapse=", "),
            parental_nationality = paste(unique(ol.nationality), collapse = ", ")
        )
    
    incomelevels = list(
        "Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999", 
        "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $89,999", "$90,000 - $99,999", "$100,000 - $149,999",  "More than $150,000", "Don't Know/Prefer not to answer", "Prefer not to answer"
        )           

    incomes = rawdata$maq$edges$Respondents_Incomes %>%
        left_join(rawdata$maq$nodes$Respondents, by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$Incomes, by = c(dst = "openlattice.@id"))%>%
        rename(
            respondent_id = nc.SubjectIdentification,
            study = study.x,
            income = ol.type
        ) %>%
        mutate(
            income = as_factor(income),
            income = fct_relevel(income, levels=incomelevels)
        ) %>%
        select(-c(table_access.x, table_access.y)) %>%
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>% 
        arrange(income) %>%
        summarise(
            parental_highest_income = last(income)
            
        )
    
    assistancelevels = list("No - never", "Yes - in past", "Yes - current")
    
    public_assistance = rawdata$maq$edges$Respondents_PublicAssistance %>%
        left_join(rawdata$maq$nodes$Respondents, by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$PublicAssistance, by = c(dst = "openlattice.@id"))%>%
        rename(
            respondent_id = nc.SubjectIdentification,
            study = study.x,
            assistance = ol.type
        ) %>%
        mutate(
            assistance = as_factor(assistance),
            assistance = fct_relevel(assistance, levels=assistancelevels)
        ) %>%
        select(-c(table_access.x, table_access.y)) %>%
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>%
        arrange(assistance) %>%
        summarise(
            parental_least_public_assistance = first(assistance)
        )
    
    metadata = rawdata$maq$edges$Respondents_SurveyMetadata %>%
        left_join(rawdata$maq$nodes$Respondents, by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$SurveyMetadata, by = c(dst = "openlattice.@id")) %>%
        mutate(
            respondent_id = nc.SubjectIdentification,
            date_maq = ymd_hms(ol.recordeddate)
        ) %>%
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarise(
            date_maq = first(date_maq)

        )

    race_key <- c('amindian' = "Native American / Pacific Islander", 
                     "black" = 'Black', 
                     "biracial" = 'Biracial', 
                     "asian" = 'Asian', 
                     "white" = 'White')

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
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>% 
        summarise(
            race = paste(unique(race), collapse=","),
            ethnicity = paste(unique(ethnicity[!is.na(ethnicity)]), collapse = ","),
            employment = paste(unique(employment), collapse=",")
        ) %>%
        mutate(
            race = ifelse(str_detect(race, ","), "biracial", race),
            race = ifelse(str_detect(race, "NA"), NA, race),
            ethnicity = ifelse(str_detect(ethnicity, ","), "multiple", ethnicity),
            ethnicity = ifelse(ethnicity == "", NA, ethnicity),
            parental_employment = as_factor(ifelse(str_detect(employment, ","), NA, employment))
        ) %>%
        mutate(
            parental_employment = fct_relevel(parental_employment, levels=list("No", "Maternity / parental leave", "One part-time job", "One full-time job", "Multiple jobs"))
        ) %>%
        mutate_at('race', recode, !!!race_key)

    
    
    education_levels = list(
        "No formal school"  ,
        "Middle school",
        "High school or equivalent (e.g., GED)",
        "Some College or Vocational degree",
        "Bachelor's degree",
        "Master's degree",
        "Doctoral or Professional degree"
    )
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
               education = person.highesteducation)%>%
        mutate(
            parental_education = fct_relevel(as_factor(education), education_levels)
            ) %>%
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>% 
        arrange(parental_education) %>%
        summarise(
            parental_education = last(fct_relevel(parental_education, education_levels))
        )
                
    quality = rawdata$maq$edges$Respondents_QualityControl %>%
        left_join(rawdata$maq$nodes$Respondents,
                  by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$QualityControl, by = c(dst = "openlattice.@id")) %>%
        select(-c(table_access.x, table_access.y))%>%
        mutate(correct = str_detect(ol.description, regex(ol.selection, ignore_case=TRUE))) %>%
        rename(respondent_id = nc.SubjectIdentification,
               study = study.x) %>%
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>% 
        summarise(
            n_quality = n(),
            mean_quality = mean(correct, na.rm=TRUE)
        )
    
    ######
    ## PSI variables
    ######
    
    psi <- psi_transform(rawdata)
    pm <- pm_transform(rawdata)
    
         
    maq <- children %>%
        left_join(quality, by = "child_id") %>%
        left_join(respondentdetails, by = "child_id") %>%
        left_join(incomes, by = "child_id") %>%
        left_join(employment, by = 'child_id') %>%
        left_join(education, by = "child_id") %>%
        left_join(metadata, by = "child_id")  %>%
        left_join(public_assistance, by = "child_id")
        
    maq <- maq %>% mutate(
        birthyear = ol.birthyear, 
        birthdate = ymd( paste(birthyear, birthmonth, "01",sep="-")),
        maqdate = date(date_maq),
        age_from_dob = time_length(date(date_maq) - ymd(birthdate), unit="month"),
        age_months = ifelse(is.na(age_from_dob), as.numeric(person.ageatevent)*12, age_from_dob)
        ) %>%
        mutate(
            nc.SubjectIdentification = child_id,
            study = study_id
            ) %>% 
        select(
                       "parental_education",
                       parental_least_public_assistance,
                       "age_months",
                       "parental_employment",
                       parental_mean_age,
                       "ethnicity",
                       'race',
                       'child_id',
                       "respondent_id",
                       "study",
                       "nc.SubjectIdentification",
                       "table_access",
                       parental_highest_income,
                       n_quality,
                       mean_quality,
                       sf_Q1_mediahours_weekday,
                       sf_Q1_mediahours_weekend,
                       sf_Q2_no_media_bedtime,
                       sf_Q3_noscreenmediahours_weekday,
                       sf_Q3_noscreenmediahours_weekend
                   )
    
    maq <- maq %>%
        left_join(psi, by = "child_id") %>%
        left_join(pm, by = "child_id")
    
    # factor vars to factor
    numericcols <- maq %>% select_if(is.numeric) %>% names()
    ndist <- maq %>%
        summarise_all(funs(n_distinct(.)))
    colndistinct <- as_tibble(cbind(nms = names(ndist), t(ndist)))
    names(colndistinct) <- c("nms", "cnt")
    colndistinct <- colndistinct %>% mutate(nums = as.numeric(cnt))
    factcols <-
        colndistinct %>% filter(nums >= 4) %>% filter(nums <= 10) %>% filter(!(nms %in% numericcols))
    boolcols <- colndistinct %>% filter(nums <= 2) %>% filter(!(nms %in% numericcols))
    
    maq <- maq %>%
        mutate_at(factcols$nms, as.factor) %>%
        mutate_at(boolcols$nms, as.logical)
    
    maq <- maq %>%
        mutate_at(c("mean_quality"), as.numeric)

    return(maq)
}
