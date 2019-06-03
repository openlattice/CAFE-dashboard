######
## PARENTS OWN MEDIA USE variables
######

parentsmediause_transform <- function(rawdata, children) {
    parentsmediause <-
        recombine(list("Respondents", "Device_Use"), rawdata) %>%
        left_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarize(
            parent_num_devices_tv_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "television_adultuse")]),
            parent_num_devices_dvr_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "dvr_adultuse")]),
            parent_num_devices_dvd_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "dvd_adultuse")]),
            parent_num_devices_computer_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "personalcomputer_adultuse")]),
            parent_num_devices_mobilephone_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "mobilephone_adultuse")]),
            parent_num_devices_smartphone_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "smartphone_adultuse")]),
            parent_num_devices_ipad_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "ipad_adultuse")]),
            parent_num_devices_mp3_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "mp3_adultuse")]),
            parent_num_devices_educationalgame_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "educationalgame_adultuse")]),
            parent_num_devices_consolegame_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "consolegame_adultuse")]),
            parent_num_devices_virtualassistant_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "virtualassistant_adultuse")]),
            parent_num_devices_videostreaming_2wks = first(Device_Use.ol.number[str_detect(Device_Use.ol.id, "videostreaming_adultuse")]),
            parent_num_devices_2wks_all = sum(
                str_detect(
                    Device_Use.ol.id,
                    "television_adultuse|dvr_adultuse|dvd_adultuse|personalcomputer_adultuse|mobilephone_adultuse|smartphone_adultuse|ipad_adultuse|mp3_adultuse|educationalgame_adultuse|consolegame_adultuse|virtualassistant_adultuse|videostreaming_adultuse"
                )
            ),
            parent_weekday_tv_dvd_avoid = sum(
                str_detect(Device_Use.ol.description, "TV or DVDs") &
                    str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(
                        Device_Use.ol.id,
                        "weekdays_adultuse|yesterday_adultuse"
                    ) &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|Not used"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekday_tv_dvd_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "TV or DVDs") &
                                                                          str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                                                                          str_detect(Device_Use.ol.subject, "adult") &
                                                                          str_detect(Device_Use.ol.id,
                                                                                     "weekdays_adultuse|yesterday_adultuse")]),
            parent_weekday_computer_avoid = sum(
                str_detect(Device_Use.ol.description, "computer") &
                    str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(
                        Device_Use.ol.id,
                        "weekdays_adultuse|yesterday_adultuse"
                    ) &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|Not used"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekday_computer_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "computer") &
                                                                            str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                                                                            str_detect(Device_Use.ol.subject, "adult") &
                                                                            str_detect(Device_Use.ol.id,
                                                                                       "weekdays_adultuse|yesterday_adultuse")]),
            parent_weekday_read_books_tries = sum(
                str_detect(
                    Device_Use.ol.description,
                    "reads books|reads traditional books"
                ) &
                    str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(
                        Device_Use.ol.id,
                        "weekdays_adultuse|yesterday_adultuse"
                    ) &
                    !str_detect(Device_Use.ol.duration,
                                "Never|Not used"),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekday_read_books_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description,
                                                                                     "reads traditional books|reads books") &
                                                                              str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                                                                              str_detect(Device_Use.ol.subject, "adult") &
                                                                              str_detect(Device_Use.ol.id,
                                                                                         "weekdays_adultuse|yesterday_adultuse")]),
            parent_weekday_ebooks_avoid = sum(
                str_detect(Device_Use.ol.description, "reads electronic books") &
                    str_detect(Device_Use.ol.relevantperiod, "weekday") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(Device_Use.ol.id, "weekdays_adultuse") &
                    str_detect(Device_Use.ol.duration,
                               "Never|Not used"),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekday_ebooks_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "reads electronic books") &
                                                                          str_detect(Device_Use.ol.relevantperiod, "weekday") &
                                                                          str_detect(Device_Use.ol.subject, "adult") &
                                                                          str_detect(Device_Use.ol.id, "weekdays_adultuse")]),
            
            parent_weekday_consolegame_avoid = sum(
                str_detect(Device_Use.ol.description, "console game player") &
                    str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(
                        Device_Use.ol.id,
                        "weekdays_adultuse|yesterday_adultuse"
                    ) &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Not used|Less than 30 minutes"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekday_consolegame_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "console game player") &
                                                                               str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                                                                               str_detect(Device_Use.ol.subject, "adult") &
                                                                               str_detect(Device_Use.ol.id,
                                                                                          "weekdays_adultuse|yesterday_adultuse")]),
            parent_weekday_ipad_avoid = sum(
                str_detect(Device_Use.ol.description, "iPad") &
                    str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(
                        Device_Use.ol.id,
                        "weekdays_adultuse|yesterday_adultuse"
                    ) &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Not used|Less than 30 minutes"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekday_ipad_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "iPad") &
                                                                        str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                                                                        str_detect(Device_Use.ol.subject, "adult") &
                                                                        str_detect(Device_Use.ol.id,
                                                                                   "weekdays_adultuse|yesterday_adultuse")]),
            parent_weekday_smartphone_avoid = sum(
                str_detect(Device_Use.ol.description, "smartphone") &
                    str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(
                        Device_Use.ol.id,
                        "weekdays_adultuse|yesterday_adultuse"
                    ) &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Not used|Less than 30 minutes"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekday_smartphone_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "smartphone") &
                                                                              str_detect(Device_Use.ol.relevantperiod, "weekday|yesterday") &
                                                                              str_detect(Device_Use.ol.subject, "adult") &
                                                                              str_detect(Device_Use.ol.id,
                                                                                         "weekdays_adultuse|yesterday_adultuse")]),
            parent_weekend_tv_dvd_avoid = sum(
                str_detect(Device_Use.ol.description, "TV or DVDs") &
                    str_detect(Device_Use.ol.relevantperiod, "weekend") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(Device_Use.ol.id, "weekend_adultuse") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|Not used"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekend_tv_dvd_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "TV or DVDs") &
                                                                          str_detect(Device_Use.ol.relevantperiod, "weekend") &
                                                                          str_detect(Device_Use.ol.subject, "adult") &
                                                                          str_detect(Device_Use.ol.id, "weekend_adultuse")]),
            parent_weekend_computer_avoid = sum(
                str_detect(Device_Use.ol.description, "computer") &
                    str_detect(Device_Use.ol.relevantperiod, "weekend") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(Device_Use.ol.id, "weekend_adultuse") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|Not used"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekend_computer_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "computer") &
                                                                            str_detect(Device_Use.ol.relevantperiod, "weekend") &
                                                                            str_detect(Device_Use.ol.subject, "adult") &
                                                                            str_detect(Device_Use.ol.id, "weekend_adultuse")]),
            parent_weekend_read_books_tries = sum(
                str_detect(Device_Use.ol.description, "reads traditional books") &
                    str_detect(Device_Use.ol.relevantperiod, "weekend") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(Device_Use.ol.id, "weekend_adultuse") &
                    !str_detect(Device_Use.ol.duration,
                                "Never|Not used"),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekend_read_books_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "reads traditional books") &
                                                                              str_detect(Device_Use.ol.relevantperiod, "weekend") &
                                                                              str_detect(Device_Use.ol.subject, "adult") &
                                                                              str_detect(Device_Use.ol.id, "weekend_adultuse")]),
            parent_weekend_ebooks_avoid = sum(
                str_detect(Device_Use.ol.description, "reads electronic books") &
                    str_detect(Device_Use.ol.relevantperiod, "weekend") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(Device_Use.ol.id, "weekend_adultuse") &
                    str_detect(Device_Use.ol.duration,
                               "Never|Not used"),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekend_ebooks_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "reads electronic books") &
                                                                          str_detect(Device_Use.ol.relevantperiod, "weekend") &
                                                                          str_detect(Device_Use.ol.subject, "adult") &
                                                                          str_detect(Device_Use.ol.id, "weekend_adultuse")]),
            
            parent_weekend_consolegame_avoid = sum(
                str_detect(Device_Use.ol.description, "console game player") &
                    str_detect(Device_Use.ol.relevantperiod, "weekend") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(Device_Use.ol.id, "weekend_adultuse") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Not used|Less than 30 minutes"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekend_consolegame_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "console game player") &
                                                                               str_detect(Device_Use.ol.relevantperiod, "weekend") &
                                                                               str_detect(Device_Use.ol.subject, "adult") &
                                                                               str_detect(Device_Use.ol.id, "weekend_adultuse")]),
            parent_weekend_ipad_avoid = sum(
                str_detect(Device_Use.ol.description, "iPad") &
                    str_detect(Device_Use.ol.relevantperiod, "weekend") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(Device_Use.ol.id, "weekend_adultuse") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Not used|Less than 30 minutes"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekend_ipad_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "iPad") &
                                                                        str_detect(Device_Use.ol.relevantperiod, "weekend") &
                                                                        str_detect(Device_Use.ol.subject, "adult") &
                                                                        str_detect(Device_Use.ol.id, "weekend_adultuse")]),
            parent_weekend_smartphone_avoid = sum(
                str_detect(Device_Use.ol.description, "smartphone") &
                    str_detect(Device_Use.ol.relevantperiod, "weekend") &
                    str_detect(Device_Use.ol.subject, "adult") &
                    str_detect(Device_Use.ol.id, "weekend_adultuse") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Not used|Less than 30 minutes"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            parent_weekend_smartphone_use = first(Device_Use.ol.duration[str_detect(Device_Use.ol.description, "smartphone") &
                                                                              str_detect(Device_Use.ol.relevantperiod, "weekend") &
                                                                              str_detect(Device_Use.ol.subject, "adult") &
                                                                              str_detect(Device_Use.ol.id, "weekend_adultuse")]),
            smartphonechecks = first(Device_Use.general.frequency[str_detect(Device_Use.ol.description, "times respondent checks")]),
            parent_smartphone_use_with_child_during_meals = first(Device_Use.ol.status[str_detect(Device_Use.ol.description, "during meals") &
                                                                                            str_detect(Device_Use.ol.subject, "adult")]),
            parent_smartphone_use_with_child_ready_for_school = first(Device_Use.ol.status[str_detect(Device_Use.ol.description, "child ready for school") &
                                                                                                str_detect(Device_Use.ol.subject, "adult")]),
            parent_smartphone_use_with_child_during_playtime = first(Device_Use.ol.status[str_detect(Device_Use.ol.description, "during playtime") &
                                                                                               str_detect(Device_Use.ol.subject, "adult")]),
            parent_smartphone_use_with_child_during_bedtime = first(Device_Use.ol.status[str_detect(Device_Use.ol.description, "during bedtime routine") &
                                                                                              str_detect(Device_Use.ol.subject, "adult")]),
            parent_smartphone_use_with_child_while_driving = first(Device_Use.ol.status[str_detect(Device_Use.ol.description, "while driving|when riding") &
                                                                                             str_detect(Device_Use.ol.subject, "adult")]),
            parent_smartphone_use_with_child_at_playground = first(Device_Use.ol.status[str_detect(Device_Use.ol.description, "at the playground") &
                                                                                             str_detect(Device_Use.ol.subject, "adult")]),
            parent_total_num_apps_using_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse") &
                    str_detect(
                        Device_Use.ol.description,
                        "what app usually using while with child"
                    )
            ),
            parent_work_email_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse_workemail"),
                na.rm = TRUE
            ) > 0,
            parent_personal_email_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse_personalemail"),
                na.rm = TRUE
            ) > 0,
            parent_social_media_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse_socialmedia"),
                na.rm = TRUE
            ) > 0,
            parent_reading_news_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse_readingnews"),
                na.rm = TRUE
            ) > 0,
            parent_looking_something_up_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse_lookingsomethingup"),
                na.rm = TRUE
            ) > 0,
            parent_watching_video_alone_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse_watchingvideojustme"),
                na.rm = TRUE
            ) > 0,
            parent_watching_video_together_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse_watchingvideotogether"),
                na.rm = TRUE
            ) > 0,
            parent_other_apps_with_child = sum(
                str_detect(Device_Use.ol.id, "apps_adultuse_other"),
                na.rm = TRUE
            ) > 0
            
        ) %>%
        select(
            child_id,
            parent_num_devices_tv_2wks,
            parent_num_devices_dvr_2wks,
            parent_num_devices_dvd_2wks,
            parent_num_devices_computer_2wks,
            parent_num_devices_mobilephone_2wks,
            parent_num_devices_smartphone_2wks,
            parent_num_devices_ipad_2wks,
            parent_num_devices_mp3_2wks,
            parent_num_devices_educationalgame_2wks,
            parent_num_devices_virtualassistant_2wks,
            parent_num_devices_videostreaming_2wks,
            parent_num_devices_2wks_all,
            parent_weekday_tv_dvd_avoid,
            parent_weekday_tv_dvd_use,
            parent_weekday_computer_avoid,
            parent_weekday_computer_use,
            parent_weekday_read_books_tries,
            parent_weekday_read_books_use,
            parent_weekday_ebooks_avoid,
            parent_weekday_ebooks_use,
            parent_weekday_consolegame_avoid,
            parent_weekday_consolegame_use,
            parent_weekday_ipad_avoid,
            parent_weekday_ipad_use,
            parent_weekday_smartphone_avoid,
            parent_weekday_smartphone_use,
            parent_weekend_tv_dvd_avoid,
            parent_weekend_tv_dvd_use,
            parent_weekend_computer_avoid,
            parent_weekend_computer_use,
            parent_weekend_read_books_tries,
            parent_weekend_read_books_use,
            parent_weekend_ebooks_avoid,
            parent_weekend_ebooks_use,
            parent_weekend_consolegame_avoid,
            parent_weekend_consolegame_use,
            parent_weekend_ipad_avoid,
            parent_weekend_ipad_use,
            parent_weekend_smartphone_avoid,
            parent_weekend_smartphone_use,
            smartphonechecks,
            parent_smartphone_use_with_child_during_meals,
            parent_smartphone_use_with_child_ready_for_school,
            parent_smartphone_use_with_child_during_playtime,
            parent_smartphone_use_with_child_during_bedtime,
            parent_smartphone_use_with_child_while_driving,
            parent_smartphone_use_with_child_at_playground,
            parent_total_num_apps_using_with_child,
            parent_work_email_with_child,
            parent_personal_email_with_child,
            parent_social_media_with_child,
            parent_reading_news_with_child,
            parent_looking_something_up_with_child,
            parent_watching_video_alone_with_child,
            parent_watching_video_together_with_child,
            parent_other_apps_with_child
        )
    return (parentsmediause)
}

parentsmediaexposure_transform <- function(rawdata, children) {
    parentmediaexposure = recombine(list("Respondents", "MediaExposure"), rawdata) %>%
        left_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarize(
            parent_non_english_media_exposure = sum(
                str_detect(MediaExposure.ol.id, "nonenglish_mediaexposure_adult") &
                    str_detect(MediaExposure.ol.status, "Yes"),
                na.rm = TRUE
            ) > 0,
            parent_non_english_media_duration = first(MediaExposure.ol.duration[str_detect(MediaExposure.ol.id, "nonenglish_mediaexposure_adult") &
                                                                                     str_detect(MediaExposure.ol.status, "Yes")])
        ) %>%
        select(parent_non_english_media_exposure,
               parent_non_english_media_duration,
               child_id)
    
    return(parentmediaexposure)
}

parentsmediaattitudes_transform <- function(rawdata, children) {
    parentmediaattitudes = recombine(list("Respondents", "MediaAttitudes"), rawdata) %>%
    left_join(children, by = 'respondent_id') %>%
    group_by(child_id) %>%
    summarize(
        need_to_stay_connected_to_work = first(MediaAttitudes.ol.workrequirement),
        need_to_stay_connected_to_friends_socialmedia = first(MediaAttitudes.ol.socialsupports),
        multitask_easy_between_using_mobile_children = first(MediaAttitudes.ol.multitaskingtendency),
        feeling_overwhelmed_by_mobile_device = first(MediaAttitudes.ol.overwhelmed),
        prefer_online_interaction_to_inperson = first(MediaAttitudes.ol.preferredmethod),
        escape_from_reality_while_with_children = first(MediaAttitudes.ol.escapefromreality),
        feel_addicted_mobile_media = first(MediaAttitudes.ol.feelsaddicted)
    ) %>%
    select(
        child_id,
        need_to_stay_connected_to_work,
        need_to_stay_connected_to_friends_socialmedia,
        multitask_easy_between_using_mobile_children,
        feeling_overwhelmed_by_mobile_device,
        prefer_online_interaction_to_inperson,
        escape_from_reality_while_with_children,
        feel_addicted_mobile_media
    )
    return(parentmediaattitudes)
}


######
## PARENTS MEDIATION variables
######

parentsmediation_transform <- function(rawdata, children) {
    parents_mediation_sf <-
    recombine(list("Respondents", "ParentMediationScale"), rawdata) %>%
    left_join(children, by = 'respondent_id') %>%
    group_by(child_id) %>%
    summarise(
        sf_maq_Q8_coview_lessthan24m = sum(
            ParentMediationScale.ol.parentalcoviewing == "Most of the time" &
                ParentMediationScale.ol.comprehension == "Often",
            na.rm = TRUE
        ) > 0,
        sf_maq_Q8_coview_over24m = sum(
            ParentMediationScale.ol.parentalcoviewing == "Occasionally|Frequently|Most of the time" &
                ParentMediationScale.ol.comprehension == "Often",
            na.rm = TRUE
        ) > 0,
        sf_maq_Q9_mediacontent = sum(
            str_detect(
                ParentMediationScale.ol.parentalcontrols,
                "Most of the time"
            ) &
                str_detect(
                    ParentMediationScale.ol.usesparentalmediawebsite,
                    "Most of the time"
                ) &
                str_detect(
                    ParentMediationScale.ol.restrictschildmediause,
                    "Most of the time"
                ) &
                str_detect(
                    ParentMediationScale.ol.usesratingsystem,
                    "Most of the time"
                ),
            na.rm = TRUE
        ) >= 2
    )
    return(parents_mediation_sf)
}

