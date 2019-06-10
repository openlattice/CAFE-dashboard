childcare_transform <- function(rawdata) {
    childcare_week = recombine(list("Children", "ChildCare_Weekdays"), rawdata) %>%
        rowwise() %>%
        mutate(
            relationship = str_replace_all(ChildCare_Weekdays.ol.relationship,"\"|c\\(|\\)", ""),
            relationship = str_split(relationship, ",")
        ) %>% 
        unnest(relationship) %>%
        mutate(
            relationship = trimws(relationship)
        ) %>%
        group_by(child_id) %>%
        summarise(
            childcare_weekday_mother = sum(str_detect(relationship, regex("mother", ignore_case=T))) > 0,
            childcare_weekday_father = sum(str_detect(relationship, regex("father", ignore_case=T))) > 0,
            childcare_weekday_family = sum(str_detect(relationship, regex("family", ignore_case=T))) > 0,
            childcare_weekday_babysitter = sum(str_detect(relationship, regex("babysitter", ignore_case=T))) > 0,
            childcare_weekday_childcare_preschool = sum(str_detect(relationship, regex("childcare/preschool", ignore_case=T))) > 0,
            childcare_weekday_equally = sum(str_detect(relationship, regex("equally", ignore_case=T))) > 0,
            childcare_weekday_other = sum(str_detect(relationship, regex("other", ignore_case=T))) > 0,
            childcare_weekday_do_not_know = sum(str_detect(relationship, regex("do not know", ignore_case=T))) > 0,
            childcare_weekday_text = paste0(relationship, collapse = ","),
            childcare_type = first(ChildCare_Weekdays.ol.type),
            # childcare_duration = as.numeric(first(ChildCare_Weekdays.ol.durationhours)),
            childcare_firstage = first(ChildCare_Weekdays.ol.ageatonset)
        )

    childcare_weekend = recombine(list("Children", "Households"), rawdata) %>%
        group_by(child_id)
        rowwise() %>%
        mutate(
            relationship = str_replace_all(ChildCare_Weekends.ol.relationship,"\"|c\\(|\\)", ""),
            relationship = str_split(relationship, ",")
        ) %>% 
        unnest(relationship) %>%
        mutate(
            relationship = trimws(relationship)
        ) %>%
        group_by(child_id) %>%
        summarise(
            childcare_weekday_mother = sum(str_detect(relationship, regex("mother", ignore_case=T))) > 0,
            childcare_weekday_father = sum(str_detect(relationship, regex("father", ignore_case=T))) > 0,
            childcare_weekday_family = sum(str_detect(relationship, regex("family", ignore_case=T))) > 0,
            childcare_weekday_babysitter = sum(str_detect(relationship, regex("babysitter", ignore_case=T))) > 0,
            childcare_weekday_childcare_preschool = sum(str_detect(relationship, regex("childcare/preschool", ignore_case=T))) > 0,
            childcare_weekday_equally = sum(str_detect(relationship, regex("equally", ignore_case=T))) > 0,
            childcare_weekday_other = sum(str_detect(relationship, regex("other", ignore_case=T))) > 0,
            childcare_weekday_do_not_know = paste0(relationship, collapse=", ")
        )
        
        childcare = childcare_week %>% full_join(childcare_weekend)
    
    return(childcare)
}


household_transform <- function(rawdata) {
    household = recombine(list("Children", "Households"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            household_type = first(Households.demographics.hhtype)
        )

    return(household)
}

household_communication_transform <- function(rawdata) {
    householdcom = recombine(list("Children", "Household_Communication"), rawdata) %>%
        group_by(child_id) %>%
        mutate(
            children_language_num = first(Household_Communication.ol.number),
            children_language_main = first(Household_Communication.ol.language),
            children_language_percent_main = as.numeric(first(Household_Communication.ol.pcttime)),
            children_language_mother_speaks_main = sum(str_detect(Household_Communication.ol.status, "or mother")),
            children_language_father_speaks_main = sum(str_detect(Household_Communication.ol.status, "or father")),
            children_language_grandmother_speaks_main = sum(str_detect(Household_Communication.ol.status, "Grandmother")),
            children_language_grandfather_speaks_main = sum(str_detect(Household_Communication.ol.status, "GrandFather")),
            children_language_childcare_speaks_main = sum(str_detect(Household_Communication.ol.status, "Childcare")),
            children_language_siblings_speaks_main = sum(str_detect(Household_Communication.ol.status, "Sibling")),
            children_language_speaks_main_text = paste0(Household_Communication.ol.status, collapse=",")
        )
    
    # mediadevices = recombine(list("Devices", "Children"), rawdata) %>%
        
        
}






c("c(\"Child's mother or mother figure\", \"Child's father or father figure\")")
