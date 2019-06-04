videochat_transform <- function(rawdata, children) {
    videochat = recombine(list("Respondents", "MediaUseAttitudes"), rawdata) %>%
        full_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarise(
            uses_videochat_relatives = sum(
                str_detect(MediaUseAttitudes.ol.reason, "distant relatives") &
                    str_detect(
                        MediaUseAttitudes.ol.levelofagreement,
                        "Sometimes|Often|Very often|Very Frequently"
                    )
            ) > 0,
            uses_videochat_relatives_text = paste0(MediaUseAttitudes.ol.levelofagreement[str_detect(MediaUseAttitudes.ol.reason, "distant relatives")], collapse = ", "),
            uses_videochat_see_parents = sum(
                str_detect(MediaUseAttitudes.ol.reason, "see parents") &
                    str_detect(
                        MediaUseAttitudes.ol.levelofagreement,
                        "Sometimes|Often|Very often|Very Frequently"
                    )
            ) > 0,
            uses_videochat_see_parents_text = paste0(MediaUseAttitudes.ol.levelofagreement[str_detect(MediaUseAttitudes.ol.reason, "see parents")], collapse = ", "),
            uses_videochat_connect_day = sum(
                str_detect(MediaUseAttitudes.ol.reason, "throughout the day") &
                    str_detect(
                        MediaUseAttitudes.ol.levelofagreement,
                        "Sometimes|Often|Very often|Very Frequently"
                    )
            ) > 0,
            uses_videochat_connect_day_text = paste0(MediaUseAttitudes.ol.levelofagreement[str_detect(MediaUseAttitudes.ol.reason, "throughout the day")], collapse = ", "),
            uses_videochat_connect_work = sum(
                str_detect(MediaUseAttitudes.ol.reason, "because of work schedule") &
                    str_detect(
                        MediaUseAttitudes.ol.levelofagreement,
                        "Sometimes|Often|Very often|Very Frequently"
                    )
            ) > 0,
            uses_videochat_connect_work_text = paste0(MediaUseAttitudes.ol.levelofagreement[str_detect(MediaUseAttitudes.ol.reason, "because of work schedule")], collapse = ", ")
        )
    
    
    videochat2 = recombine(list("Respondents", "Device_Use"), rawdata) %>%
        filter(str_detect(Device_Use.ol.id, "videochat")) %>%
        full_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarise(
            uses_videochat = first(Device_Use.ol.status),
            videochat_weeklyhours_adult = first(Device_Use.ol.duration[str_detect(Device_Use.ol.id, "videochat_hoursweek")])
        )
    
    videochat_reason = recombine(list("Respondents", "MediaUseAttitudes"), rawdata) %>%
        rowwise() %>%
        filter(str_detect(MediaUseAttitudes.ol.id, "videochat") & !is.na(MediaUseAttitudes.ol.levelofagreement)) %>%
        mutate(
            id = strsplit(MediaUseAttitudes.ol.id, "-"),
            id = paste0(id[length(id)], collapse="_")
        ) %>%
        full_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>% slice(1) %>% ungroup() %>%
        select(child_id, id, MediaUseAttitudes.ol.levelofagreement) %>%
        filter(!is.na(id)) %>%
        spread( key = id, value = MediaUseAttitudes.ol.levelofagreement)

    devices = recombine(list("Devices", "Children"), rawdata)
    mediauseattitudes = recombine(list("Respondents", "MediaUseAttitudes"), rawdata) %>%
        left_join(children, by = 'respondent_id')
    devices_mediauseattitudes = devices %>%
        left_join(mediauseattitudes, by = "child_id") %>%
        group_by(child_id) %>%
        summarise(sf_maq_Q10_uses_videochat_a = sum(
            str_detect(Devices.ol.id, "videochat") &
                str_detect(
                    MediaUseAttitudes.ol.levelofagreement,
                    "Sometimes|Often|Very often|Very Frequently"
                )
        ) > 0) %>%
        select(child_id, sf_maq_Q10_uses_videochat_a)

    videochat = videochat %>% 
        full_join(videochat2, by= "child_id") %>%
        full_join(videochat_reason, by= "child_id") %>%
        full_join(devices_mediauseattitudes, by = "child_id")

    return(videochat)
}





    
    
    
    
    