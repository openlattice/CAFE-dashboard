childdemographics_transform <- function(rawdata, children) {
    metadata <- metadata_transform(rawdata, children)
    
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
            Children.table_access,
            Children.nc.PersonSex
        ) %>%
        group_by(child_id,
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
        mutate_at('race', recode, !!!race_key)
    
    
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
            fulldate = ifelse("NA" %in% fulldate, NA, fulldate),
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
            birthweight_g_recoded = recode(birthweight, !!!birthweight_gram_key),
            birthweight_lbs_recoded = recode(birthweight, !!!birthweight_pound_key)
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
    
    children_demographics = children_demographics %>%
        full_join(childrendetails, by = "child_id") %>%
        full_join(childrendetailshealth, by = "child_id") %>%
        select(-c(
            table_access,
            birthdate,
            fulldate,
            birthweight,
            age
        ))
    
    childrenrole = recombine(list("Respondents", "Children"), rawdata, association = "RelatedTo") %>%
        rename(respondent_relationship = 'RelatedTo.ol.role') %>%
        select(child_id, respondent_relationship) %>% 
        group_by(child_id)  %>% slice(1) %>% ungroup()
    
    immigration = recombine(list("Respondents", "ImmigrationStatus"), rawdata) %>%
        left_join(children, by = "respondent_id") %>%
        rename(
            immigration_time_in_us = 'ImmigrationStatus.ol.status'
        ) %>%
        select(
            child_id, immigration_time_in_us
        )

    children_demographics = children_demographics %>% 
        left_join(childrenrole, by = "child_id") %>%
        left_join(immigration, by = "child_id")
    
    return(children_demographics)
}


