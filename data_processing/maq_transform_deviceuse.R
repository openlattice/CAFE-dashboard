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

deviceuse_transform <- function(rawdata, children) {
    deviceuse = recombine(list("Children", "Device_Use"), rawdata) %>%
        mutate(duration = recode(Device_Use.ol.duration, !!!deviceuse_key)) %>%
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
            sf_maq_Q1_nomorethan1h_weekday_all = sf_maq_Q1_nomorethan1h_weekday_TV_DVD &
                sf_maq_Q1_nomorethan1h_weekday_computer &
                sf_maq_Q1_nomorethan1h_weekday_ebooks &
                sf_maq_Q1_nomorethan1h_weekday_consolevideo &
                sf_maq_Q1_nomorethan1h_weekday_mobiledevice &
                sf_maq_Q1_nomorethan1h_weekday_smartphone &
                sf_maq_Q1_nomorethan1h_weekday_virtualassistant,
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
            sf_maq_Q1_nomorethan1h_weekend_all = sf_maq_Q1_nomorethan1h_weekend_TV_DVD &
                sf_maq_Q1_nomorethan1h_weekend_computer &
                sf_maq_Q1_nomorethan1h_weekend_ebooks &
                sf_maq_Q1_nomorethan1h_weekend_consolevideo &
                sf_maq_Q1_nomorethan1h_weekend_mobiledevice &
                sf_maq_Q1_nomorethan1h_weekend_smartphone &
                sf_maq_Q1_nomorethan1h_weekday_virtualassistant,
            all_NA = sum(is.na(Device_Use.general.frequency[str_detect(Device_Use.ol.description,
                                                                       "hour before bedtime|while falling asleep")]))
            ==
                length(Device_Use.general.frequency[str_detect(Device_Use.ol.description,
                                                               "hour before bedtime|while falling asleep")]),
            
            hourbeforebedtimequants = first(Device_Use.general.frequency[str_detect(Device_Use.ol.description,
                                                                                     "hour before bedtime|while falling asleep")]),
            
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
            sf_maq_Q2_avoid_screen_bedtime = ifelse(all_NA, NA, sf_maq_Q2_avoid_screen_bedtime),
            sf_maq_Q3_balancemedia_reading_weekday = sum(
                str_detect(Device_Use.ol.description, "paper books|electronic books") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "30 minutes to 1 hour|1-2 hours|2-3 hours|3-4 hours|4-5 hours|More than 5 hours"
                    ),
                na.rm =
                    TRUE
            ) > 0,
            sf_maq_Q3_balancemedia_reading_weekend = sum(
                str_detect(Device_Use.ol.description, "paper books|electronic books") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent") &
                    str_detect(
                        Device_Use.ol.duration,
                        "30 minutes to 1 hour|1-2 hours|2-3 hours|3-4 hours|4-5 hours|More than 5 hours"
                    ),
                na.rm =
                    TRUE
            ) > 0,
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
            all_NA_recent2wks = sum(is.na(Device_Use.ol.name[str_detect(Device_Use.ol.status,
                                                                        "used very recently")]))
            ==
                length(Device_Use.ol.name[str_detect(Device_Use.ol.status,
                                                     "used very recently")]),
            num_devices_recent2wks = sum(str_detect(Device_Use.ol.id, "recent_childuse") &
                str_detect(Device_Use.ol.status, "used very recently"),
                na.rm =
                    TRUE),
            num_devices_recent2wks = ifelse(all_NA_recent2wks == TRUE, NA, num_devices_recent2wks),
            devices_recent2wks = paste0(Device_Use.ol.name[str_detect(Device_Use.ol.status, "used very recently")], collapse = ", "),
            num_devices_start2wks = sum(
                str_detect(Device_Use.ol.id, "start_childuse") &
                    str_detect(
                        Device_Use.ol.status,
                        "Started using in last 2 weeks|Started using in the last 2 weeks"
                    )
            ),
            devices_start2wks = paste0(Device_Use.ol.name[str_detect(Device_Use.ol.id, "start_childuse") &
                                                              str_detect(
                                                                  Device_Use.ol.status,
                                                                  "Started using in last 2 weeks|Started using in the last 2 weeks"
                                                              )], collapse = ", "),
            num_devices_start6mos = sum(
                str_detect(Device_Use.ol.id, "start_childuse") &
                    str_detect(
                        Device_Use.ol.status,
                        "Started using in the last 6 months|Started using in last 6 months"
                    )
            ),
            devices_start6mos = paste0(Device_Use.ol.name[str_detect(Device_Use.ol.id, "start_childuse") &
                                                              str_detect(
                                                                  Device_Use.ol.status,
                                                                  "Started using in the last 6 months|Started using in last 6 months"
                                                              )], collapse = ", "),
            internet_access = paste0(Device_Use.ol.status[str_detect(Device_Use.ol.id, "internet_access")], collapse=", "),
            transitmedia = paste0(Device_Use.ol.status[str_detect(Device_Use.ol.id, "media_in_transit")], collapse=", "),
            television_on_household = paste0(Device_Use.general.frequency[str_detect(Device_Use.ol.id, "television_in_home")], collapse=", "),
        )
    
    start_childuse = recombine(list("Children", "Device_Use"), rawdata) %>%
        rowwise() %>%
        filter(str_detect(Device_Use.ol.id, "_start_childuse") & !is.na(Device_Use.ol.status)) %>%
        mutate(
            id = strsplit(Device_Use.ol.id, "-"),
            id = paste0(id[length(id)], collapse="_")
        ) %>%
        group_by(child_id) %>% slice(1) %>% ungroup() %>%
        select(child_id, id, Device_Use.ol.status) %>%
        spread( key = id, value = Device_Use.ol.status)
    
    hourbeforebed_childuse = recombine(list("Children", "Device_Use"), rawdata) %>%
        rowwise() %>%
        filter(str_detect(Device_Use.ol.id, "_hourbeforebed_childuse") & !is.na(Device_Use.general.frequency)) %>%
        mutate(
            id = strsplit(Device_Use.ol.id, "-"),
            id = paste0(id[length(id)], collapse="_")
        ) %>%
        group_by(child_id) %>% slice(1) %>% ungroup() %>%
        select(child_id, id, Device_Use.general.frequency) %>%
        spread( key = id, value = Device_Use.general.frequency)
    
    fallingasleep_childuse = recombine(list("Children", "Device_Use"), rawdata) %>%
        rowwise() %>%
        filter(str_detect(Device_Use.ol.id, "_fallingasleep_childuse") & !is.na(Device_Use.general.frequency)) %>%
        mutate(
            id = strsplit(Device_Use.ol.id, "-"),
            id = paste0(id[length(id)], collapse="_")
        ) %>%
        group_by(child_id) %>% slice(1) %>% ungroup() %>%
        select(child_id, id, Device_Use.general.frequency) %>%
        spread( key = id, value = Device_Use.general.frequency)
    
    timespent = recombine(list("Children", "Device_Use"), rawdata) %>%
        rowwise() %>%
        filter(str_detect(Device_Use.ol.id, "Time spent") & !is.na(Device_Use.ol.duration)) %>%
        mutate(
            id = strsplit(Device_Use.ol.id, "-"),
            id = paste0(id[length(id)], collapse="_")
        ) %>%
        group_by(child_id) %>% slice(1) %>% ungroup() %>%
        select(child_id, id, Device_Use.general.frequency) %>%
        spread( key = id, value = Device_Use.general.frequency)
    
    adultuse = recombine(list("Respondents", "Device_Use"), rawdata) %>%
        rowwise() %>%
        filter(str_detect(Device_Use.ol.id, "_adultuse") & !is.na(Device_Use.ol.number)) %>%
        mutate(
            id = strsplit(Device_Use.ol.id, "-"),
            id = paste0(id[length(id)], collapse="_"),
            number = as.numeric(Device_Use.ol.number)
        ) %>%
        full_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>% slice(1) %>% ungroup() %>%
        select(child_id, id, number) %>%
        filter(!is.na(id)) %>%
        spread( key = id, value = number)
    
    deviceuse = deviceuse %>%
        full_join(start_childuse, by = "child_id") %>%
        full_join(hourbeforebed_childuse, by = "child_id") %>%
        full_join(fallingasleep_childuse, by = "child_id") %>%
        full_join(timespent, by = "child_id") %>%
        full_join(adultuse, by = "child_id") %>%
        select(
            -c(
                "all_NA",
                "hourbeforebedtime",
                "hourbeforebedtimequants",
                "hourbeforebedtimenever"
            )
        )
    
    return(deviceuse)
}








