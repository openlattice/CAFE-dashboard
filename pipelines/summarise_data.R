library(tidyverse)

source("pipelines/load_data.R")
source("pipelines/transform_data.R")

summarise_data <- function(activitydata) {

    if (dim(activitydata)[1] == 0) {
    return (tibble())
  }

    summarydata <- activitydata %>% 
    group_by(child_id) %>%
    summarise(
      total_time = sum(duration),
      total_blocks = n(),
      
      background_media_on_hours = sum( duration[background_media_tv | background_media_audio | background_media_other], na.rm=TRUE),
      background_media_blocks = sum(background_media_tv | background_media_audio | background_media_other, na.rm=TRUE),
      
      background_tv_hours = sum(duration[background_media_tv], na.rm = TRUE),
      background_tv_blocks = sum(background_media_tv, na.rm=TRUE),
      
      total_tv_hours = sum(duration[background_media_tv | primary_tv], na.rm=TRUE),
      total_tv_blocks = sum(background_media_tv | primary_tv, na.rm=TRUE),
      
      primary_media_hours = sum(duration[str_detect(ol.activity, "Media use")], na.rm=TRUE),
      primary_media_blocks = sum(str_detect(ol.activity, "Media use"), na.rm=TRUE),
      primary_tv_hours = sum(duration[primary_tv], na.rm=TRUE),
      primary_computer_hours = sum(duration[primary_computer], na.rm=TRUE),
      primary_smartphone_hours = sum(duration[primary_smartphone], na.rm=TRUE),
      primary_tablet_hours = sum(duration[primary_tablet], na.rm=TRUE),
      primary_book_hours = sum(duration[primary_book], na.rm=TRUE),
      primary_video_hours = sum(duration[primary_video], na.rm=TRUE),
      primary_console_hours = sum(duration[primary_console], na.rm=TRUE),
      primary_handheld_hours = sum(duration[primary_handheldgame], na.rm=TRUE),
      primary_radio_hours = sum(duration[primary_radio], na.rm=TRUE),
      primary_theater_hours = sum(duration[primary_theater], na.rm=TRUE),
      
      screen_hours = sum(duration[screen], na.rm=TRUE),
      screen_blocks = sum(screen, na.rm=TRUE),
      screen_mean_hours = mean(duration[screen], na.rm=TRUE),
      screen_median_hours = median(duration[screen], na.rm=TRUE),
      screen_1hfromsleeping_hours = sum(duration[screen & time_to_sleep<1], na.rm=TRUE),
      screen_1hfromsleeping_blocks = sum(screen & time_to_sleep<1, na.rm=TRUE),
      screen_while_feeding_hors = sum(duration[screen & str_detect(ol.activity, "Eating")], na.rm=TRUE),
      
      screen_adult_coviewing_hours = sum(duration[screen & adult_present], na.rm=TRUE),
      sleeping_hours = sum(duration[str_detect(ol.activity, "Sleeping")], na.rm=TRUE),
      sleeping_blocks = sum(str_detect(ol.activity, "Sleeping"), na.rm=TRUE),
      sleeping_btv_hours = sum(duration[str_detect(ol.activity, "Sleeping") & background_media_tv], na.rm=TRUE),
      
      feeding_hours = sum(duration[str_detect(ol.activity, "Eating")], na.rm=TRUE),
      feeding_blocks = sum(str_detect(ol.activity, "Eating"), na.rm=TRUE),
      feeding_btv_hours = sum(duration[str_detect(ol.activity, "Eating") & background_media_tv], na.rm=TRUE),
      
      bathroom_hours = sum(duration[str_detect(ol.activity, "Grooming")], na.rm=TRUE),
      childcare_hours = sum(duration[str_detect(ol.activity, "Childcare")], na.rm=TRUE),
      play_inside_hours = sum(duration[str_detect(ol.activity, "Play_recreating inside")], na.rm=TRUE),
      play_outside_hours = sum(duration[str_detect(ol.activity, "Play_recreating outside")], na.rm=TRUE),
      play_hours = sum(duration[str_detect(ol.activity, "Play")], na.rm=TRUE),
      chores_hours = sum(duration[str_detect(ol.activity, "chores")], na.rm=TRUE),
      traveling_hours = sum(duration[str_detect(ol.activity, "traveling")], na.rm=TRUE),
      
      bathroom_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "Grooming")], na.rm=TRUE),
      childcare_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "Childcare")], na.rm=TRUE),
      play_bmedia_inside_hours = sum(duration[background_media & str_detect(ol.activity, "Play_recreating inside")], na.rm=TRUE),
      play_bmedia_outside_hours = sum(duration[background_media & str_detect(ol.activity, "Play_recreating outside")], na.rm=TRUE),
      play_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "Play")], na.rm=TRUE),
      chores_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "chores")], na.rm=TRUE),
      traveling_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "traveling")], na.rm=TRUE),
      
      age_child_primary_media = sum(duration[primary_media_age_child], na.rm=TRUE),
      age_older_primary_media = sum(duration[primary_media_age_older], na.rm=TRUE),
      age_younger_primary_media = sum(duration[primary_media_age_younger], na.rm=TRUE),
      age_adults_primary_media = sum(duration[primary_media_age_adult], na.rm=TRUE),
      
      age_child_secondary_media = sum(duration[secondary_media_age_child], na.rm=TRUE),
      age_older_secondary_media = sum(duration[secondary_media_age_older], na.rm=TRUE),
      age_younger_secondary_media = sum(duration[secondary_media_age_younger], na.rm=TRUE),
      age_adults_secondary_media = sum(duration[secondary_media_age_adult], na.rm=TRUE)
    )
  return(summarydata)
}
