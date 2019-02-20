library(tidyverse)
library(lubridate)

## combine everything
process_activities <- function(rawdata){

  if (rawdata$n_act == 0) {
    return (tibble())
  }

  relatives <- process_relatives(rawdata)
  device_by_activity <- process_devices(rawdata)
  media_exposure_by_activity <- process_media_exposure(rawdata)
  adults_by_activity <- process_adult_use(rawdata)
  locations_by_activity <- process_locations(rawdata)
  activity <- process_activity(rawdata)

  activity <- activity %>%
    left_join(relatives, by = "primary_activity_id") %>%
    left_join(device_by_activity, by = "primary_activity_id") %>%
    left_join(media_exposure_by_activity, by = "primary_activity_id") %>%
    left_join(adults_by_activity, by = "primary_activity_id")

  # factor vars to factor
  ndist <- activity %>%
    summarise_all(funs(n_distinct(.)))
  colndistinct <- as_tibble(cbind(nms = names(ndist), t(ndist)))
  names(colndistinct) <- c("nms", "cnt")
  colndistinct <- colndistinct %>% mutate(nums = as.numeric(cnt))
  factcols <- colndistinct %>% filter(nums >= 4) %>% filter(nums <= 6)
  boolcols <- colndistinct %>% filter(nums <= 3)

  activity <- activity %>%
    mutate_at(factcols$nms, as.factor) %>%
    mutate_at(boolcols$nms, as.logical)

  return(activity)
}

# summarise relatives related to primary activities

process_relatives <- function(rawdata) {
  if (dim(rawdata$edges$relatives_primary_activity)[1] == 0){
    return (
      tibble(primary_activity_id=as.character(),
             adults_present_num=as.character(),
             adult_present=as.character(),
             otherkids_present=as.character(),
             alone=as.character()
             ))
  }
  adults_by_activity <- rawdata$edges$relatives_primary_activity %>%
    # add relatives
    left_join(rawdata$nodes$relatives, by = c(src = "openlattice.@id")) %>%
    # add activities
    left_join(rawdata$nodes$primary_activity, by = c(dst = "openlattice.@id")) %>%
    # summarise
    rename(primary_activity_id = dst) %>%
    group_by(primary_activity_id) %>%
    summarise(
      adults_present_num = sum(str_detect(ol.name, "Mother|Father|Grandparent|Childcare") , na.rm=TRUE),
      adult_present = sum(str_detect(ol.name, "Mother|Father|Grandparent|Childcare")) > 0,
      otherkids_present = sum(str_detect(ol.name, "Sibling|kids"), na.rm=TRUE),
      alone = sum(str_detect(ol.name, "Alone"), na.rm=TRUE)
    )
  return(adults_by_activity)
}

# summarise devices by media exposures by activity

process_devices <- function(rawdata){
  if (dim(rawdata$edges$devices_media_exposure)[1] == 0){
    return (tibble(
      primary_activity_id=as.character(),
      primary_tv = as.character(),
      primary_computer = as.character(),
      primary_smartphone = as.character(),
      primary_tablet = as.character(),
      primary_book = as.character(),
      primary_video = as.character(),
      primary_console = as.character(),
      primary_handheldgame = as.character(),
      primary_radio = as.character(),
      primary_theater = as.character(),
      primary_other = as.character()
    ))
  }
  device_by_activity <- rawdata$edges$devices_media_exposure %>%
    # add devices
    left_join(rawdata$nodes$devices, by = c(src = "openlattice.@id")) %>%
    # add media_exposure
    left_join(rawdata$nodes$media_exposure, by = c(dst = "openlattice.@id")) %>%
    rename(media_exposure_id = dst) %>%
    # add activities
    left_join(rawdata$edges$media_exposure_primary_activity, by = c(media_exposure_id = "src")) %>%
    left_join(rawdata$nodes$primary_activity, by = c(dst = "openlattice.@id")) %>%
    rename(primary_activity_id = dst) %>%
    # summarise
    group_by(primary_activity_id) %>%
    summarise(
      primary_number_devices = n(),
      primary_tv = sum(str_detect(ol.name.x, 'TV') > 0, na.rm=TRUE),
      primary_computer = sum(str_detect(ol.name.x, 'Computer') > 0, na.rm=TRUE),
      primary_smartphone = sum(str_detect(ol.name.x, 'Smartphone') > 0, na.rm=TRUE),
      primary_tablet = sum(str_detect(ol.name.x, 'Tablet') > 0, na.rm=TRUE),
      primary_book = sum(str_detect(ol.name.x, 'Book') > 0, na.rm=TRUE),
      primary_video = sum(str_detect(ol.name.x, 'Video') > 0, na.rm=TRUE),
      primary_console = sum(str_detect(ol.name.x, 'Console') > 0, na.rm=TRUE),
      primary_handheldgame = sum(str_detect(ol.name.x, 'HandheldGamingDevice') > 0, na.rm=TRUE),
      primary_radio = sum(str_detect(ol.name.x, 'RadioCD') > 0, na.rm=TRUE),
      primary_theater = sum(str_detect(ol.name.x, 'Theater') > 0, na.rm=TRUE),
      primary_other = sum(str_detect(ol.name.x, 'Other') > 0, na.rm=TRUE)
    )
  return(device_by_activity)
}

# summarise media exposures by activity

process_media_exposure <- function(rawdata){
  if (dim(rawdata$edges$media_exposure_primary_activity)[1] == 0){
    return (tibble(
      primary_activity_id=as.character(),
      background_media = as.character(),
      background_media_tv = as.character(),
      background_media_audio = as.character(),
      background_media_other = as.character(),
      primary_media = as.character(),
      primary_media_age_child = as.character(),
      primary_media_age_younger = as.character(),
      primary_media_age_older = as.character(),
      primary_media_age_adult = as.character(),
      secondary_media_age_child = as.character(),
      secondary_media_age_younger = as.character(),
      secondary_media_age_older = as.character(),
      secondary_media_age_adult = as.character(),
      screen = as.character()
      ))
  }
  media_exposure_by_activity <- rawdata$edges$media_exposure_primary_activity %>%
    # add media_exposure
    left_join(rawdata$nodes$media_exposure, by = c(src = "openlattice.@id")) %>%
    # add primary activities
    left_join(rawdata$nodes$primary_activity, by = c(dst = "openlattice.@id")) %>%
    # summarise
    rename(primary_activity_id = dst) %>%
    group_by(primary_activity_id) %>%
    summarise(
      background_media = sum(str_detect(ol.priority, "secondary")) > 0,
      background_media_tv = str_detect(ol.type, "television") && str_detect(ol.priority, "secondary"),
      background_media_audio = str_detect(ol.type, "audio") && str_detect(ol.priority, "secondary"),
      background_media_other = str_detect(ol.type, "other") && str_detect(ol.priority, "secondary"),
      primary_media = sum(str_detect(ol.priority, "primary")) > 0,
      primary_media_age = ifelse(str_detect(ol.priority, "primary"), ol.category, NA),
      primary_media_age_child = str_detect(ol.category, "Child's age") && str_detect(ol.priority, "primary"),
      primary_media_age_younger = str_detect(ol.category, "Younger") && str_detect(ol.priority, "primary"),
      primary_media_age_older = str_detect(ol.category, "Older") && str_detect(ol.priority, "primary"),
      primary_media_age_adult = str_detect(ol.category, "Adults") && str_detect(ol.priority, "primary"),
      secondary_media_age = ifelse(str_detect(ol.priority, "secondary"), ol.category, NA),
      secondary_media_age_child = str_detect(ol.category, "Child's age") && str_detect(ol.priority, "secondary"),
      secondary_media_age_younger = str_detect(ol.category, "Younger") && str_detect(ol.priority, "secondary"),
      secondary_media_age_older = str_detect(ol.category, "Older") && str_detect(ol.priority, "secondary"),
      secondary_media_age_adult = str_detect(ol.category, "Adults") && str_detect(ol.priority, "secondary"),
      screen = sum(str_detect(ol.type, "television|video|Video|internet")) > 0
    )
  return(media_exposure_by_activity)
}

# summarise adult coviewing by activity

process_adult_use <- function(rawdata){
  if (dim(rawdata$edges$primary_activity_adult_use)[1] == 0){
    return (
      tibble(
        primary_activity_id=as.character(),
        adult_use = as.character(),
        adult_no_use=as.character()
        )
      )
  }
  adult_use_by_activity <- rawdata$edges$primary_activity_adult_use %>%
    # add media_exposure
    left_join(rawdata$nodes$primary_activity, by = c(src = "openlattice.@id")) %>%
    # add primary activities
    left_join(rawdata$nodes$adult_use, by = c(dst = "openlattice.@id")) %>%
    # summarise
    rename(primary_activity_id = src) %>%
    group_by(primary_activity_id) %>%
    summarise(
      adult_use = str_detect(ol.status, "Yes"),
      adult_no_use = str_detect(ol.status, "No")
    )
  return(adult_use_by_activity)
}

# clean up activity rawdata (need to add child ID to find next sleep !)

process_activity <- function(rawdata){
  if (dim(rawdata$edges$people_primary_activity)[1] == 0){
    return (tibble(
      primary_activity_id=as.character(),
      child_id=as.character(),
      time_to_sleep=as.character(),
      duration = as.character(),
      ol.activity=as.character()
      ))
  }
  activity <- rawdata$edges$people_primary_activity %>%
    left_join(rawdata$nodes$people, by = c(src = "openlattice.@id")) %>%
    left_join(rawdata$nodes$primary_activity, by = c(dst = "openlattice.@id")) %>%
    rename(primary_activity_id = dst, child_id = src) %>%
    mutate(
      starttime = ymd_hms(ol.datetimestart),
      endtime = ymd_hms(ol.datetimeend)
      ) %>%
    mutate(duration = as.numeric(endtime - starttime) / 60) %>%
    mutate(duration = ifelse(duration < 0, duration + 24*60, duration)) %>% 
    arrange(child_id, starttime) %>%
    mutate(table_access = (table_access.x & table_access.y))
      

  ## add time to sleep (OMG THIS WAS A DIFFICULT FUNCTION :-o )
  activity <- activity %>%
    group_by(child_id) %>%
    nest() %>%
    mutate(data = map(data, add_time_to_sleep)) %>%
    unnest() %>%
    select("primary_activity_id", "child_id", "time_to_sleep", "starttime", "endtime", "duration", "ol.activity", "nc.SubjectIdentification", "table_access")
  return(activity)
}

## locations

process_locations <- function(rawdata) {
    if (dim(rawdata$edges$primary_activity_locations)[1] == 0){
        return (
            tibble(primary_activity_id=as.character(),
                   site=as.character()
            ))
    }
    adults_by_activity <- rawdata$edges$primary_activity_locations %>%
        # add relatives
        left_join(rawdata$nodes$primary_activity, by = c(src = "openlattice.@id")) %>%
        # add activities
        left_join(rawdata$nodes$locations, by = c(dst = "openlattice.@id")) %>%
        # summarise
        rename(primary_activity_id = dst) %>%
        group_by(primary_activity_id) %>%
        summarise(site = names(which.max(table(ol.id.y))))

    return(adults_by_activity)
}



## HELPER FUNCTIONS

add_time_to_sleep <- function(df){
  sleeps <- df %>% filter(str_detect(ol.activity, "Sleep")) %>% pull(starttime)
  df['time_to_sleep'] <- df$starttime %>% lapply(get_closest, sleeps) %>% unlist()
  return(df)
}

get_closest <- function(startdt, slaapkes) {
  difs <- difftime(slaapkes, startdt, units = "hours")
  if (length(difs) == 1){return (difs)}
  closest_sleep <- min(difs[difs>=0], na.rm=TRUE)
  return(closest_sleep)
}
