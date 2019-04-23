#########
## PSI ##
#########

instructive_vars = c(
    'ol.comprehension',
    'ol.badactions',
    'ol.explainmeaning',
    'ol.explainmotives',
    'ol.positiveeffects'
)
restrictive_vars = c(
    'ol.setviewinghours',
    'ol.restrictions',
    'ol.inappropriatecontent',
    'ol.advancenotice',
    'sharing.programstoexclude'
)
social_vars = c(
    'ol.activityenjoyment',
    'ol.laughter',
    'ol.commoninterest',
    'ol.action',
    'ol.favoriteprogram'
)

instructive_replace = instructive_vars
names(instructive_replace) = instructive_vars %>% str_replace("ol.", "Valkenburg_instructive_mediation_")
restrictive_replace = restrictive_vars
names(restrictive_replace) = restrictive_vars %>% str_replace("ol.", "Valkenburg_restrictive_mediation_")
social_replace = social_vars
names(social_replace) = social_vars %>% str_replace("ol.", "Valkenburg_social_coviewing_")

pm_level_key = c(
    "Never" = '1',
    "Rarely" = '2',
    'Sometimes' = '3',
    'Often' = '4',
    'Not applicable' = NA
)

pm_transform <- function(rawdata) {
    pm_ungrouped = rawdata$maq$edges$Respondents_ParentMediationScale %>%
        left_join(rawdata$maq$nodes$Respondents,
                  by = c(src = "openlattice.@id")) %>%
        left_join(rawdata$maq$nodes$ParentMediationScale, by = c(dst = "openlattice.@id")) %>%
        filter(str_detect(ol.id, 'mediause_inhome')) %>%
        select(-c(table_access.x, table_access.y)) %>%
        dplyr::rename(respondent_id = nc.SubjectIdentification, study = study.x) %>%
        left_join(children_respondents, by = 'respondent_id') %>%
        group_by(child_id) %>%
        mutate_at(vars(c(instructive_vars, restrictive_vars, social_vars)), recode, !!!pm_level_key) %>%
        mutate_at(vars(c(instructive_vars, restrictive_vars, social_vars)), as.numeric) %>%
        select(c(instructive_vars, restrictive_vars, social_vars)) %>%
        plyr::mutate(
            instructive_n_missing = apply(.[,instructive_vars], 1, function(x) sum(is.na(x))),
            instructive = apply(.[,instructive_vars], 1, function(x) mean(x, na.rm=TRUE)),
            restrictive_n_missing = apply(.[,restrictive_vars], 1, function(x) sum(is.na(x))),
            restrictive = apply(.[,restrictive_vars], 1, function(x) mean(x, na.rm=TRUE)),
            social_n_missing = apply(.[,social_vars], 1, function(x) sum(is.na(x))),
            social = apply(.[,social_vars], 1, function(x) mean(x, na.rm=TRUE)),
            pm_n_missing = apply(.[,c(instructive_vars, restrictive_vars, social_vars)], 1, function(x) sum(is.na(x))),
            pm = apply(.[,c(instructive_vars, restrictive_vars, social_vars)], 1, function(x) mean(x, na.rm=TRUE)),
            pm_total = ifelse(pm_n_missing < 2, pm, NA)
        )
    

    instructive_dat = pm_ungrouped %>% summarise_at(vars(instructive_vars), mean) %>% rename_(.dots = instructive_replace)
    restrictive_dat = pm_ungrouped %>% summarise_at(vars(restrictive_vars), mean) %>% rename_(.dots = restrictive_replace)
    social_dat = pm_ungrouped %>% summarise_at(vars(social_vars), mean) %>% rename_(.dots = social_replace)

    pm_summaries = pm_ungrouped %>% summarise(
        Valkenburg_factor_instructive = mean(instructive, na.rm=TRUE),
        Valkenburg_factor_restrictive = mean(restrictive, na.rm=TRUE),
        Valkenburg_factor_social = mean(social, na.rm=TRUE),
        Valkenburg_total = mean(pm_total, na.rm=TRUE)
    )
    pm = instructive_dat %>% 
        left_join(restrictive_dat) %>%
        left_join(social_dat) %>%
        left_join(pm_summaries)
    return (pm)
}
