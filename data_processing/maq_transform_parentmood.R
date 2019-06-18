parentingmood_transform <- function(rawdata, children) {
    parentingmood = recombine(list("Respondents", "ParentingMoodAssessment"), rawdata) %>%
        rename_at(vars(c("ParentingMoodAssessment.ol.mood", "ParentingMoodAssessment.ol.response" )), ~c("mood", "response")) %>%
        left_join(children, by = 'respondent_id') %>%
        select(child_id, mood, response) %>%
        group_by(child_id, mood) %>% slice(1) %>% ungroup() %>%
        spread( key = mood, value = response)
    
    oldnames = names(parentingmood[-1])
    newnames = paste0("respondent_present_mood_", oldnames) %>% tolower()
    parentingmood = parentingmood %>% rename_at(vars(oldnames), ~newnames)
    
    return(parentingmood)
}



