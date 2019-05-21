#########
## PSI ##
#########

instructive_vars = c(
    'ParentMediationScale.ol.comprehension',
    'ParentMediationScale.ol.badactions',
    'ParentMediationScale.ol.explainmeaning',
    'ParentMediationScale.ol.explainmotives',
    'ParentMediationScale.ol.positiveeffects'
)
restrictive_vars = c(
    'ParentMediationScale.ol.setviewinghours',
    'ParentMediationScale.ol.restrictions',
    'ParentMediationScale.ol.inappropriatecontent',
    'ParentMediationScale.ol.advancenotice',
    'ParentMediationScale.sharing.programstoexclude'
)
social_vars = c(
    'ParentMediationScale.ol.activityenjoyment',
    'ParentMediationScale.ol.laughter',
    'ParentMediationScale.ol.commoninterest',
    'ParentMediationScale.ol.action',
    'ParentMediationScale.ol.favoriteprogram'
)

instructive_replace = instructive_vars
names(instructive_replace) = instructive_vars %>% str_replace("ParentMediationScale.ol.",
                                                              "Valkenburg_instructive_mediation_")
restrictive_replace = restrictive_vars
names(restrictive_replace) = restrictive_vars %>% str_replace("ParentMediationScale.ol.",
                                                              "Valkenburg_restrictive_mediation_") %>% str_replace('ParentMediationScale.sharing.',
                                                                                                                   "Valkenburg_restrictive_mediation_")
social_replace = social_vars
names(social_replace) = social_vars %>% str_replace("ParentMediationScale.ol.", "Valkenburg_social_coviewing_")

pm_level_key = c(
    "Never" = '1',
    "Rarely" = '2',
    'Sometimes' = '3',
    'Often' = '4',
    'Not applicable' = NA
)

pm_transform <- function(rawdata, children) {
    pm_ungrouped = recombine(list("Respondents", "ParentMediationScale"), rawdata) %>%
        left_join(children, by = "respondent_id") %>%
        filter(str_detect(ParentMediationScale.ol.id, "mediause_inhome")) %>%
        group_by(child_id) %>%
        mutate_at(vars(c(
            instructive_vars, restrictive_vars, social_vars
        )), recode,!!!pm_level_key) %>%
        mutate_at(vars(c(
            instructive_vars, restrictive_vars, social_vars
        )), as.numeric) %>%
        select(c(instructive_vars, restrictive_vars, social_vars)) %>%
        plyr::mutate(
            instructive_n_missing = apply(.[, instructive_vars], 1, function(x)
                sum(is.na(x))),
            instructive = apply(.[, instructive_vars], 1, function(x)
                mean(x, na.rm = TRUE)),
            restrictive_n_missing = apply(.[, restrictive_vars], 1, function(x)
                sum(is.na(x))),
            restrictive = apply(.[, restrictive_vars], 1, function(x)
                mean(x, na.rm = TRUE)),
            social_n_missing = apply(.[, social_vars], 1, function(x)
                sum(is.na(x))),
            social = apply(.[, social_vars], 1, function(x)
                mean(x, na.rm = TRUE)),
            pm_n_missing = apply(.[, c(instructive_vars, restrictive_vars, social_vars)], 1, function(x)
                sum(is.na(x))),
            pm = apply(.[, c(instructive_vars, restrictive_vars, social_vars)], 1, function(x)
                mean(x, na.rm = TRUE)),
            pm_total = ifelse(pm_n_missing < 2, pm, NA)
        )
    
    instructive_dat = pm_ungrouped %>% summarise_at(vars(instructive_vars), mean) %>% rename_(.dots = instructive_replace)
    restrictive_dat = pm_ungrouped %>% summarise_at(vars(restrictive_vars), mean) %>% rename_(.dots = restrictive_replace)
    social_dat = pm_ungrouped %>% summarise_at(vars(social_vars), mean) %>% rename_(.dots = social_replace)
    
    pm_summaries = pm_ungrouped %>% summarise(
        Valkenburg_factor_instructive = mean(instructive, na.rm = TRUE),
        Valkenburg_factor_restrictive = mean(restrictive, na.rm = TRUE),
        Valkenburg_factor_social = mean(social, na.rm = TRUE),
        Valkenburg_total = mean(pm_total, na.rm = TRUE)
    )
    
    restrictions  = recombine(list("Respondents", "ParentMediationScale"), rawdata) %>%
        left_join(children, by = "respondent_id") %>%
        filter(
            str_detect(
                ParentMediationScale.ol.id,
                "parental_restriction_child|parental_limits_household"
            )
        ) %>%
        group_by(child_id) %>%
        summarise(
            mediarestrictions_parentalcontrols = first(ParentMediationScale.ol.parentalcontrols),
            mediarestrictions_useparentalmediawebsite = first(ParentMediationScale.ol.usesparentalmediawebsite),
            mediarestrictions_restrictschildmediause = first(ParentMediationScale.ol.restrictschildmediause),
            mediarestrictions_usesratingsystem = first(ParentMediationScale.ol.usesratingsystem),
            mediarestrictions_parentalcoviewing = first(ParentMediationScale.ol.parentalcoviewing),
            mediarestrictions_restrictions = first(ParentMediationScale.ol.restrictions),
            mediarestrictions_restrictedactivities = paste0(
                ParentMediationScale.ol.restrictedactivities[!is.na(ParentMediationScale.ol.restrictedactivities)],
                collapse = ","
            ),
            mediarestrictions_timelimits = first(ParentMediationScale.ol.timelimits),
            mediarestrictions_contentrestrictions = first(ParentMediationScale.ol.contentrestrictions)
        )
    
    # non_english_exposure = recombine(list("Respondents", "MediaExposure"), rawdata) %>%
    #     left_join(children, by = "respondent_id") %>%
    #     filter(str_detect(ParentMediationScale.ol.id, "nonenglish_mediaexposure")) %>%
    #     group_by(child_id) %>%
    #     summarise(
    #         non_english_proportion = mean(as.numeric(ol.duration))
    #     )
    
    restrictions_advice = recombine(list("Respondents", "ParentMediationScale"), rawdata) %>%
        left_join(children, by = "respondent_id") %>%
        filter(str_detect(ParentMediationScale.ol.id, "setlimits_advicesource")) %>%
        group_by(child_id) %>%
        summarise(
            restrictions_advice_ratings = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "ratings like PG-13"
                )
            ) > 0,
            restrictions_advice_socialmedia = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "Social Media"
                )
            ) > 0,
            restrictions_advice_aap = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "American Academy of Pediatrics"
                )
            ) > 0,
            restrictions_advice_research = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "own research"
                )
            ) > 0,
            restrictions_advice_partner = sum(
                str_detect(ParentMediationScale.ol.verificationsource, "partner")
            ) > 0,
            restrictions_advice_common_sense_media = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "Common Sense Media"
                )
            ) > 0,
            restrictions_advice_requests = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "child's requests"
                )
            ) > 0,
            restrictions_advice_otherparents = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "Other parents"
                )
            ) > 0,
            restrictions_advice_doctor = sum(
                str_detect(ParentMediationScale.ol.verificationsource, "doctor")
            ) > 0,
            restrictions_advice_magazines = sum(
                str_detect(ParentMediationScale.ol.verificationsource, "magazines")
            ) > 0,
            restrictions_advice_other_parents = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "Other parents"
                )
            ) > 0,
            restrictions_advice_popular_press = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "Popular press"
                )
            ) > 0,
            restrictions_advice_siblings = sum(
                str_detect(
                    ParentMediationScale.ol.verificationsource,
                    "Older siblings"
                )
            ) > 0,
            restrictions_advice_blogs = sum(
                str_detect(ParentMediationScale.ol.verificationsource, "Blogs")
            ) > 0,
        )
    
    pm = instructive_dat %>%
        full_join(restrictive_dat) %>%
        full_join(social_dat) %>%
        full_join(pm_summaries) %>%
        full_join(restrictions) %>%
        full_join(restrictions_advice)
    
    
    return (pm)
}
