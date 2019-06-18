devices_transform <- function(rawdata, children) {
    childreninhousehold = recombine(list("Children", "Households"), rawdata)
    hh_devices = recombine(list("Devices", "Households"), rawdata) %>%
        left_join(childreninhousehold, by = 'dst') %>%
        rowwise() %>%
        mutate(
            owns=TRUE,
            device = str_trim(str_split(Devices.ol.name, "\\(")[[1]][1]),
            device = str_replace_all(device, " |\\(|\\)|/", "_") %>% tolower()
            ) %>%
        group_by(child_id, device) %>% slice(1) %>% ungroup() %>%
        select(child_id, device, owns) %>%
        spread( key = device, value = owns)
    
    oldnames = names(hh_devices[-1])
    newnames = paste0("household_owns_", oldnames)
    hh_devices = hh_devices %>% rename_at(vars(oldnames), ~newnames)
    
    resp_devices = recombine(list("Devices", "Children"), rawdata) %>%
        left_join(children, by = 'child_id') %>%
        rowwise() %>%
        mutate(
            owns=TRUE,
            device = str_trim(str_split(Devices.ol.name, "\\(")[[1]][1]),
            device = str_replace_all(device, " |\\(|\\)|/", "_") %>% tolower()
        ) %>%
        group_by(child_id, device) %>% slice(1) %>% ungroup() %>%
        select(child_id, device, owns) %>%
        spread( key = device, value = owns)

    oldnames = names(resp_devices[-1])
    newnames = paste0("respondent_owns_", oldnames)
    resp_devices = resp_devices %>% rename_at(vars(oldnames), ~newnames)
    
    devices = hh_devices %>% left_join(resp_devices, by = "child_id")

    return(devices)
}


devicelocations_transform <- function(rawdata) {
    device_locations = recombine(list("RegularlyLocatedDevices", "DeviceLocations"), rawdata)
    children_locations = recombine(list("Children", "DeviceLocations"), rawdata) %>% select(-c(study_id))
    device_locations = device_locations %>%
        left_join(children_locations, by = c("dst", 
                                             "DeviceLocations.ol.id",
                                             "DeviceLocations.table_access",
                                             "DeviceLocations.ol.subject",
                                             "DeviceLocations.general.id",
                                             "DeviceLocations.ol.description",
                                             "DeviceLocations.ol.status",
                                             "DeviceLocations.study")) %>%
        rowwise() %>%
        mutate(
            device = str_trim(str_split(RegularlyLocatedDevices.ol.name, "\\(")[[1]][1]),
            device = str_replace_all(device, " |\\(|\\)|/|-", "_") %>% tolower(),
            frequency = RegularlyLocatedDevices.ol.description
        ) %>% select(child_id, device, frequency) %>%
        group_by(child_id, device) %>% slice(1) %>% ungroup() %>%
        spread(key = device, value= frequency)
    
    oldnames = names(device_locations[-1])
    newnames = paste0("childsroom_device_", oldnames)
    device_locations = device_locations %>% rename_at(vars(oldnames), ~newnames)
    
    return(device_locations)
}
    


