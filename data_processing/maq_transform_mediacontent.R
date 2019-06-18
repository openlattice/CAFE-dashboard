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


mediadeviceuse_transform <- function(rawdata) {
    services = recombine(list("Children", "MediaDeviceUse"), rawdata) %>%
        rowwise() %>%
        filter(str_detect(MediaDeviceUse.ol.id, "_tvservices") & MediaDeviceUse.ol.subject == "child") %>%
        mutate(
            id = str_extract(MediaDeviceUse.ol.id, "[a-z]*use")
        ) %>% 
        group_by(child_id,id) %>% slice(1) %>% ungroup() %>%
        select(child_id, id, MediaDeviceUse.general.frequency) %>%
        spread( key = id, value = MediaDeviceUse.general.frequency)

    oldnames = names(services[-1])
    newnames = paste0("tv_source_", oldnames) %>% tolower()
    services = services %>% rename_at(vars(oldnames), ~newnames)

    services_other = recombine(list("Children", "MediaDeviceUse"), rawdata) %>%
        rowwise() %>%
        filter(str_detect(MediaDeviceUse.ol.id, "_tvservices") & MediaDeviceUse.ol.subject == "child") %>%
        mutate(tv_source_other_sources = MediaDeviceUse.ol.description) %>% 
        group_by(child_id) %>% slice(1) %>% ungroup() %>%
        select(child_id, tv_source_other_sources) 
    
    services = services %>% left_join(services_other, by = "child_id")

    
    return(services)
}

    