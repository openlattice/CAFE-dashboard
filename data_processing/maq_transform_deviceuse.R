deviceuse_key = c(
    "Never" = 0,
    "Less than 30 minutes" = 0.25,
    "30 minutes to 1 hour" = 0.75,
    "1-2 hours" = 1.5,
    "2-3 hours" = 2.5,
    "3-4 hours" = 3.5,
    "4-5 hours" = 4.5,
    "More than 5 hours" = 5,
    .default = NA_integer_
)

deviceuse_transform <- function(rawdata) {
    deviceuse = recombine(list("Children", "Device_Use"), rawdata) %>%
        mutate(duration = recode(Device_Use.ol.duration, !!!deviceuse_key)) %>%
        group_by(child_id)%>% summarise(
            sf_maq_Q1_nomorethan1h_weekday = sum(
                duration[
                    str_detect(Device_Use.ol.description, "TV or DVDs|computer|smartphone|mobile device|console video|iPad|virtual assistant") &
                        str_detect(Device_Use.ol.description, "weekday") &
                        str_detect(Device_Use.ol.description, "Time spent")
                    ], na.rm=TRUE
            ),
            sf_maq_Q1_nomorethan1h_weekend = sum(
                duration[
                    str_detect(Device_Use.ol.description, "TV or DVDs|computer|smartphone|mobile device|console video|iPad|virtual assistant") &
                        str_detect(Device_Use.ol.description, "weekend") &
                        str_detect(Device_Use.ol.description, "Time spent")
                    ], na.rm=TRUE
            ),
            sf_maq_Q2_avoid_screen_bedtime = sum(str_detect(Device_Use.ol.description, "hour before bedtime") &
                                                     str_detect(Device_Use.general.frequency, "Never|Less than once a week"),na.rm=TRUE)>0,
            sf_maq_Q3_balancemedia_reading_weekday = sum(duration[
                str_detect(Device_Use.ol.description, "paper books|electronic books") &
                    str_detect(Device_Use.ol.description, "weekday") &
                    str_detect(Device_Use.ol.description, "Time spent")
                ], na.rm=TRUE),
            sf_maq_Q3_balancemedia_reading_weekend = sum(duration[
                str_detect(Device_Use.ol.description, "paper books|electronic books") &
                    str_detect(Device_Use.ol.description, "weekend") &
                    str_detect(Device_Use.ol.description, "Time spent")
                ], na.rm=TRUE),
            sf_maq_Q5_minimize_background_play = sum(str_detect(Device_Use.ol.id, "television_in_home") &
                                                         str_detect(Device_Use.general.frequency, "Never|Less than once a week"),na.rm=TRUE)>0,
            sf_maq_Q6_avoid_media_during_mealtimes = sum(str_detect(Device_Use.ol.id, "mediause_duringmeals") &
                                                             str_detect(Device_Use.ol.status, "Not very likely|I never do this"), na.rm=TRUE)>0,
            sf_maq_Q7_avoid_media_during_playtime = sum(str_detect(Device_Use.ol.id, "mediause_playtime") &
                                                            str_detect(Device_Use.ol.status, "Not very likely|I never do this"), na.rm=TRUE)>0
            
        ) %>%
        select(
            child_id,
            sf_maq_Q1_nomorethan1h_weekday,
            sf_maq_Q1_nomorethan1h_weekend,
            sf_maq_Q2_avoid_screen_bedtime,
            sf_maq_Q3_balancemedia_reading_weekday,
            sf_maq_Q3_balancemedia_reading_weekend,
            sf_maq_Q5_minimize_background_play,
            sf_maq_Q6_avoid_media_during_mealtimes,
            sf_maq_Q7_avoid_media_during_playtime
        )
    return(deviceuse)
}
