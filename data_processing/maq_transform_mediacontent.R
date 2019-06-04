mediacontent_transform <- function(rawdata, children) {
    mediacontent = recombine(list("Respondents", "MediaDeviceUse"), rawdata) %>%
        full_join(children, by = 'respondent_id') %>%
        group_by(child_id) %>%
        summarise(
            sf_maq_Q11_avoid_media_for_calming = sum(
                str_detect(MediaDeviceUse.ol.reason, "calm child down") &
                    str_detect(MediaDeviceUse.general.frequency, "Never")
            ) > 0,
            calming_down = first(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "calm child down")]),
            use_media_for_educating = sum(
                str_detect(MediaDeviceUse.ol.reason, "educate child") &
                    !str_detect(
                        MediaDeviceUse.general.frequency,
                        "Never|Less than once per week"
                    )
            ) > 0,
            educating_child = first(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "educate child")]),
            avoid_media_for_keeping_child_busy = sum(
                str_detect(MediaDeviceUse.ol.reason, "keep child busy") &
                    str_detect(MediaDeviceUse.general.frequency,
                               "Never")
            ) > 0,
            keeping_child_busy = first(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "keep child busy")]),
            use_media_for_communicating = sum(
                str_detect(MediaDeviceUse.ol.reason, "communicate") &
                    !str_detect(
                        MediaDeviceUse.general.frequency,
                        "Never|Less than once per week"
                    )
            ) > 0,
            communicating = first(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "communicate")]),
            use_media_for_enjoying = sum(
                str_detect(MediaDeviceUse.ol.reason, "enjoys using") &
                    !str_detect(
                        MediaDeviceUse.general.frequency,
                        "Never|Less than once per week"
                    )
            ) > 0,
            enjoying = first(MediaDeviceUse.general.frequency[str_detect(MediaDeviceUse.ol.reason, "enjoys using")])
        )
    return(mediacontent)
}

# mediauseconcerns = recombine(list("Respondents", "MediaUseConcerns"), rawdata) %>%
#     rowwise() %>%
#     filter(str_detect(MediaDeviceUse.ol.id, "mediaconcerns")) %>%
#     group_by(child_id) %>% slice(1) %>% ungroup() %>%
#     select(child_id, id, Device_Use.general.frequency) %>%
#     spread( key = id, value = Device_Use.general.frequency)
# 


# services = recombine(list("Children", "MediaDeviceUse"), rawdata) %>%
#     rowwise() %>%
#     filter(str_detect(MediaDeviceUse.ol.id, "_tvservices") & MediaDeviceUse.ol.subject == "child") %>%
#     mutate(
#         id = strsplit(Device_Use.ol.id, "-"),
#         id = paste0(id[length(id)], collapse="_")
#     ) %>%
#     group_by(child_id) %>% slice(1) %>% ungroup() %>%
#     select(child_id, id, Device_Use.general.frequency) %>%
#     spread( key = id, value = Device_Use.general.frequency)

    