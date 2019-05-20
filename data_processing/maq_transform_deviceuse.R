deviceuse_key = c(
    "Never" = 0,
    "Less than 30 minutes" = 0.25,
    "30 minutes to 1 hour" = 0.75,
    "1-2 hours" = 1.5,
    "2-3 hours" = 2.5,
    "3-4 hours" = 3.5,
    "4-5 hours" = 4.5,
    "More than 5 hours" = 5,
    "2 weeks" = 336,
    .default = NA_integer_
)

deviceuse_transform <- function(rawdata) {
    deviceuse = recombine(list("Children", "Device_Use"), rawdata) %>%
        mutate(duration = recode(Device_Use.ol.duration,!!!deviceuse_key)) %>%
        group_by(child_id) %>% summarise(
            sf_maq_Q1_nomorethan1h_weekday_TV_DVD = sum(
                str_detect(Device_Use.ol.description, "TV or DVDs") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekday_computer = sum(
                str_detect(Device_Use.ol.description, "computer") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekday_ebooks = sum(
                str_detect(Device_Use.ol.description, "electronic books") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekday_consolevideo = sum(
                str_detect(Device_Use.ol.description, "console video") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekday_mobiledevice = sum(
                str_detect(Device_Use.ol.description, "mobile device") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekday_smartphone = sum(
                str_detect(Device_Use.ol.description, "smartphone") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekday_virtualassistant = sum(
                str_detect(Device_Use.ol.description, "virtual assistant") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekend_TV_DVD = sum(
                str_detect(Device_Use.ol.description, "TV or DVDs") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekend_computer = sum(
                str_detect(Device_Use.ol.description, "computer") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekend_ebooks = sum(
                str_detect(Device_Use.ol.description, "electronic books") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekend_consolevideo = sum(
                str_detect(Device_Use.ol.description, "console video") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekend_mobiledevice = sum(
                str_detect(Device_Use.ol.description, "mobile device") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekend_smartphone = sum(
                str_detect(Device_Use.ol.description, "smartphone") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q1_nomorethan1h_weekend_virtualassistant = sum(
                str_detect(Device_Use.ol.description, "virtual assistant") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "Never|Less than 30 minutes|30 minutes to 1 hour"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            all_NA = sum(is.na(Device_Use.general.frequency[str_detect(Device_Use.ol.description,
                                                                       "hour before bedtime|while falling asleep")]))
            ==
                length(Device_Use.general.frequency[str_detect(Device_Use.ol.description,
                                                               "hour before bedtime|while falling asleep")]),
            
            hourbeforebedtimequants = paste0(Device_Use.general.frequency[str_detect(Device_Use.ol.description,
                                                                                     "hour before bedtime|while falling asleep")], collapse = ","),
            hourbeforebedtime = sum(
                str_detect(
                    Device_Use.ol.description,
                    "hour before bedtime|while falling asleep"
                ),
                na.rm = TRUE
            ),
            hourbeforebedtimenever = sum(
                str_detect(
                    Device_Use.ol.description,
                    "hour before bedtime|while falling asleep"
                ) &
                    str_detect(Device_Use.general.frequency, "Never|Less than once a week"),
                na.rm = TRUE
            ),
            sf_maq_Q2_avoid_screen_bedtime = hourbeforebedtimenever == hourbeforebedtime,
            sf_maq_Q2_avoid_screen_bedtime = if_else(all_NA, NA, sf_maq_Q2_avoid_screen_bedtime),
            sf_maq_Q3_balancemedia_reading_weekday = sum(duration[str_detect(Device_Use.ol.description, "paper books|electronic books") &
                                                                      str_detect(Device_Use.ol.description, "weekday") &
                                                                      str_detect(Device_Use.ol.description, "Time spent") &
                                                                      str_detect(
                                                                          Device_Use.ol.duration,
                                                                          "30 minutes to 1 hour|1-2 hours|2-3 hours|3-4 hours|4-5 hours|More than 5 hours"
                                                                      )], na.rm =
                                                             TRUE) > 0,
            sf_maq_Q3_balancemedia_reading_weekend = sum(duration[str_detect(Device_Use.ol.description, "paper books|electronic books") &
                                                                      str_detect(Device_Use.ol.description, "weekend") &
                                                                      str_detect(Device_Use.ol.description, "Time spent") &
                                                                      str_detect(
                                                                          Device_Use.ol.duration,
                                                                          "30 minutes to 1 hour|1-2 hours|2-3 hours|3-4 hours|4-5 hours|More than 5 hours"
                                                                      )], na.rm =
                                                             TRUE) > 0,
            sf_maq_Q5_minimize_background_play = sum(
                str_detect(Device_Use.ol.id, "television_in_home") &
                    str_detect(Device_Use.general.frequency, "Never|Hardly ever"),
                na.rm = TRUE
            ) > 0,
            sf_maq_Q6_avoid_media_during_mealtimes = sum(
                str_detect(Device_Use.ol.id, "mediause_duringmeals") &
                    str_detect(Device_Use.ol.status, "Not very likely|I never do this"),
                na.rm = TRUE
            ) > 0,
            sf_maq_Q7_avoid_media_during_play = sum(
                str_detect(
                    Device_Use.ol.id,
                    "mediause_playground|mediause_playtime"
                )
                &
                    str_detect(Device_Use.ol.status, "Not very likely|I never do this"),
                na.rm = TRUE
            ) > 0,
            number_devices_2weeks = sum(str_detect(Device_Use.ol.id, "recent_childuse"))
        ) %>%
        select(
            child_id,
            sf_maq_Q1_nomorethan1h_weekday_TV_DVD,
            sf_maq_Q1_nomorethan1h_weekday_computer,
            sf_maq_Q1_nomorethan1h_weekday_ebooks,
            sf_maq_Q1_nomorethan1h_weekday_consolevideo,
            sf_maq_Q1_nomorethan1h_weekday_mobiledevice,
            sf_maq_Q1_nomorethan1h_weekday_smartphone,
            sf_maq_Q1_nomorethan1h_weekday_virtualassistant,
            sf_maq_Q1_nomorethan1h_weekend_TV_DVD,
            sf_maq_Q1_nomorethan1h_weekend_computer,
            sf_maq_Q1_nomorethan1h_weekend_ebooks,
            sf_maq_Q1_nomorethan1h_weekend_consolevideo,
            sf_maq_Q1_nomorethan1h_weekend_mobiledevice,
            sf_maq_Q1_nomorethan1h_weekend_smartphone,
            sf_maq_Q1_nomorethan1h_weekend_virtualassistant,
            sf_maq_Q2_avoid_screen_bedtime,
            hourbeforebedtimequants,
            sf_maq_Q3_balancemedia_reading_weekday,
            sf_maq_Q3_balancemedia_reading_weekend,
            sf_maq_Q5_minimize_background_play,
            sf_maq_Q6_avoid_media_during_mealtimes,
            sf_maq_Q7_avoid_media_during_play,
            number_devices_2weeks
        )
    return(deviceuse)
}
