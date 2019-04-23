#########
## PSI ##
#########


defensive_vars = c('ol.overwhelmed',
                   'ol.parentalresponsibilities',
                   'ol.feelingtrapped',
                   'ol.dissatisfiedwithlife',
                   'ol.relationshipissues',
                   'ol.loneliness',
                   'ol.socialdisinterest')
pd_vars = c('ol.overwhelmed',
            'ol.parentalresponsibilities',
            'ol.feelingtrapped',
            'ol.opportunitycost',
            'ol.restrictedactivities',
            'ol.dissatisfiedwithpurchase',
            'ol.dissatisfiedwithlife',
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
            'ol.number',
            'ol.botheredbyactions' , 
            'ol.moreproblematic' , 
            'ol.demanding')
pd_replace = pd_vars
names(pd_replace) = pd_vars %>% str_replace("ol\\.", "PSI_parental_distress_")
pcdi_replace = pcdi_vars
names(pcdi_replace) = pcdi_vars %>% str_replace("ol\\.", "PSI_dysfunctional_interaction_")
dc_replace = dc_vars
names(dc_replace) = dc_vars %>% str_replace("ol\\.", "PSI_difficult_child_") %>% str_replace("neurological\\.", "PSI_difficult_child_")
defensive_replace = defensive_vars
names(defensive_replace) = defensive_vars %>% str_replace("ol\\.", "PSI_defensive_responses_")


level_key_1 <- c(
    "Strongly disagree" = '1',
    "Disagree" = '2',
    "Not Sure" = '3',
    "Not sure" = "3",
    "Agree" = '4',
    "Strongly Agree" = '5',
    "Strongly agree" = "5"
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


psi_transform <- function(rawdata, children_respondents) {
    psi_ungrouped = rawdata$maq$edges$Respondents_PSI_Assessment %>%
        left_join(rawdata$maq$nodes$Respondents,
                  by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$PSI_Assessment, by = c(dst = "openlattice.@id")) %>%
        select(-c(table_access.x, table_access.y)) %>%
        dplyr::rename(respondent_id = nc.SubjectIdentification, study = study.x) %>%
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>%
        mutate_at(vars(c(defensive_vars, pd_vars, pcdi_vars, dc_vars)), recode, !!!level_key_1) %>%
        mutate_at(vars(c(defensive_vars, pd_vars, pcdi_vars, dc_vars)), recode, !!!level_key_2) %>%
        mutate_at(vars(c(defensive_vars, pd_vars, pcdi_vars, dc_vars)), recode, !!!level_key_3) %>%
        mutate_at(vars(c(defensive_vars, pd_vars, pcdi_vars, dc_vars)), as.numeric) %>%
        plyr::mutate(
            defensive_n_missing = apply(.[,defensive_vars], 1, function(x) sum(is.na(x))),
            defensive = apply(.[,defensive_vars], 1, function(x) mean(x, na.rm=TRUE)),
            psi_defensive_responses = ifelse(defensive_n_missing < 2, defensive, NA),
            pd_n_missing = apply(.[,pd_vars], 1, function(x) sum(is.na(x))),
            pd = apply(.[,pd_vars], 1, function(x) mean(x, na.rm=TRUE)),
            psi_parental_distress = ifelse(pd_n_missing < 2, pd, NA),
            pcdi_n_missing = apply(.[,pcdi_vars], 1, function(x) sum(is.na(x))),
            pcdi = apply(.[,pcdi_vars], 1, function(x) mean(x, na.rm=TRUE)),
            psi_parentchild_dysfunctional_interaction = ifelse(pcdi_n_missing < 2, pcdi, NA),
            dc_n_missing = apply(.[,dc_vars], 1, function(x) sum(is.na(x))),
            dc = apply(.[,dc_vars], 1, function(x) mean(x, na.rm=TRUE)),
            psi_difficult_child = ifelse(dc_n_missing < 2, dc, NA),
            psi_n_missing = apply(.[,c(pd_vars, pcdi_vars, dc_vars)], 1, function(x) sum(is.na(x))),
            psi = apply(.[,c(pd_vars, pcdi_vars, dc_vars)], 1, function(x) mean(x, na.rm=TRUE)),
            psi_total = ifelse(psi_n_missing < 2, psi, NA)
        )
    pd_dat = psi_ungrouped %>% summarise_at(vars(pd_vars), mean) %>% rename_(.dots = pd_replace)
    pcdi_dat = psi_ungrouped %>% summarise_at(vars(pcdi_vars), mean) %>% rename_(.dots = pcdi_replace)
    dc_dat = psi_ungrouped %>% summarise_at(vars(dc_vars), mean) %>% rename_(.dots = dc_replace)
    defensive_dat = psi_ungrouped %>% summarise_at(vars(defensive_vars), mean) %>% rename_(.dots = defensive_replace)
    psi_summaries = psi_ungrouped %>% summarise(
        PSI_factor_defensive_responses = mean(psi_defensive_responses, na.rm=TRUE),
        PSI_factor_parental_distress = mean(psi_parental_distress, na.rm=TRUE),
        PSI_factor_difficult_child = mean(psi_difficult_child, na.rm=TRUE),
        PSI_factor_parentchild_dysfunctional_interaction = mean(psi_parentchild_dysfunctional_interaction, na.rm=TRUE),
        PSI_total = mean(psi_total, na.rm=TRUE)
    )
    psi = pd_dat %>% 
        left_join(pcdi_dat) %>%
        left_join(dc_dat) %>%
        left_join(defensive_dat) %>%
        left_join(psi_summaries)
    return (psi)
}
