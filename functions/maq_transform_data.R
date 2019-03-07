## combine everything
process_maq <- function(rawdata) {
    if (rawdata$auth == FALSE) {
        return (tibble())
    }
    
    employment = rawdata$maq$edges$Respondents_Employment %>% 
        left_join(rawdata$maq$nodes$Respondents, by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$Employment, by = c(dst = "openlattice.@id")) %>% 
        select('nc.PersonEthnicity', 'nc.PersonRace', 'nc.SubjectIdentification', 
               'study.x', 'ol.status', 'table_access.x', 'table_access.y') %>%
        rename(
            ethnicity = nc.PersonEthnicity,
            race = nc.PersonRace,
            study = study.x,
            employment = ol.status
        ) %>%
        mutate(
            table_access = (table_access.x & table_access.y)
        ) %>%
        select(-c(table_access.x, table_access.y))
        
    education = rawdata$maq$edges$Respondents_Education %>% 
        left_join(rawdata$maq$nodes$Respondents, by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$Education, by = c(dst = "openlattice.@id")) %>% 
        select('nc.SubjectIdentification', 'study.x', 'person.highesteducation',
               'table_access.x', 'table_access.y') %>%
        rename(
            study = study.x,
            education = person.highesteducation
        ) %>%
        mutate(table_access = (table_access.x & table_access.y)) %>%
        select(-c(table_access.x, table_access.y))
    
    maq = employment %>% left_join(education)
    return(maq)
}
    