summarise_data <- function(activitydata) {
    
    if (dim(activitydata)[1]==0) {
    return (tibble())
  }

    summarydata <- activitydata %>% 
    # group_by(recruitment, site, nc.SubjectIdentification, day_id) %>%
    group_by(site, nc.SubjectIdentification, day_id) %>%
    summarise(
      progress = mean(as.numeric(progress), na.rm=TRUE),
      total_time = sum(duration)/60,
      total_blocks = n(),
      
      background_media_on_hours = sum( duration[background_media_tv | background_media_audio | background_media_other], na.rm=TRUE)/60,
      background_media_blocks = sum(background_media_tv | background_media_audio | background_media_other, na.rm=TRUE),
      
      background_tv_hours = sum(duration[background_media_tv], na.rm = TRUE)/60,
      background_tv_blocks = sum(background_media_tv, na.rm=TRUE),
      
      total_tv_hours = sum(duration[background_media_tv | primary_tv], na.rm=TRUE)/60,
      total_tv_blocks = sum(background_media_tv | primary_tv, na.rm=TRUE),
      
      primary_media_hours = sum(duration[str_detect(ol.activity, "Media use")], na.rm=TRUE)/60,
      primary_media_blocks = sum(str_detect(ol.activity, "Media use"), na.rm=TRUE),
      primary_tv_hours = sum(duration[primary_tv], na.rm=TRUE)/60,
      primary_computer_hours = sum(duration[primary_computer], na.rm=TRUE)/60,
      primary_smartphone_hours = sum(duration[primary_smartphone], na.rm=TRUE)/60,
      primary_tablet_hours = sum(duration[primary_tablet], na.rm=TRUE)/60,
      primary_book_hours = sum(duration[primary_book], na.rm=TRUE)/60,
      primary_video_hours = sum(duration[primary_video], na.rm=TRUE)/60,
      primary_console_hours = sum(duration[primary_console], na.rm=TRUE)/60,
      primary_handheld_hours = sum(duration[primary_handheldgame], na.rm=TRUE)/60,
      primary_radio_hours = sum(duration[primary_radio], na.rm=TRUE)/60,
      primary_theater_hours = sum(duration[primary_theater], na.rm=TRUE)/60,

      screen_hours = sum(duration[screen], na.rm=TRUE)/60,
      screen_blocks = sum(screen, na.rm=TRUE),
      screen_mean_hours = mean(duration[screen], na.rm=TRUE)/60,
      screen_median_hours = median(duration[screen], na.rm=TRUE)/60,
      screen_1hfromsleeping_hours = sum(duration[screen & time_to_sleep<1], na.rm=TRUE)/60,
      screen_1hfromsleeping_blocks = sum(screen & time_to_sleep<1, na.rm=TRUE),
      screen_while_feeding_hours = sum(duration[screen & str_detect(ol.activity, "Eating")], na.rm=TRUE)/60,
      
      screen_adult_coviewing_hours = sum(duration[screen & adults_present==TRUE], na.rm=TRUE)/60,
      screen_adult_coviewing_over_screen = (sum(duration[screen & adults_present==TRUE], na.rm=TRUE)/60 / sum(duration[screen], na.rm=TRUE)/60),
      sleeping_hours = sum(duration[str_detect(ol.activity, "Sleeping")], na.rm=TRUE)/60,
      sleeping_blocks = sum(str_detect(ol.activity, "Sleeping"), na.rm=TRUE),
      sleeping_btv_hours = sum(duration[str_detect(ol.activity, "Sleeping") & background_media_tv==TRUE], na.rm=TRUE)/60,
      
      feeding_hours = sum(duration[str_detect(ol.activity, "Eating")], na.rm=TRUE)/60,
      feeding_blocks = sum(str_detect(ol.activity, "Eating"), na.rm=TRUE),
      feeding_btv_hours = sum(duration[str_detect(ol.activity, "Eating") & background_media_tv==TRUE], na.rm=TRUE)/60,

      bathroom_hours = sum(duration[str_detect(ol.activity, "Grooming")], na.rm=TRUE)/60,
      childcare_hours = sum(duration[str_detect(ol.activity, "Childcare")], na.rm=TRUE)/60,
      play_inside_hours = sum(duration[str_detect(ol.activity, "Play_recreating inside")], na.rm=TRUE)/60,
      play_outside_hours = sum(duration[str_detect(ol.activity, "Play_recreating outside")], na.rm=TRUE)/60,
      play_hours = sum(duration[str_detect(ol.activity, "Play")], na.rm=TRUE)/60,
      chores_hours = sum(duration[str_detect(ol.activity, "chores")], na.rm=TRUE)/60,
      traveling_hours = sum(duration[str_detect(ol.activity, "traveling")], na.rm=TRUE)/60,
      
      bathroom_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "Grooming")], na.rm=TRUE)/60,
      childcare_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "Childcare")], na.rm=TRUE)/60,
      play_bmedia_inside_hours = sum(duration[background_media & str_detect(ol.activity, "Play_recreating inside")], na.rm=TRUE)/60,
      play_bmedia_outside_hours = sum(duration[background_media & str_detect(ol.activity, "Play_recreating outside")], na.rm=TRUE)/60,
      play_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "Play")], na.rm=TRUE)/60,
      chores_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "chores")], na.rm=TRUE)/60,
      traveling_bmedia_hours = sum(duration[background_media & str_detect(ol.activity, "traveling")], na.rm=TRUE)/60,
      
      age_child_primary_media = sum(duration[primary_media_age_child], na.rm=TRUE),
      age_older_primary_media = sum(duration[primary_media_age_older], na.rm=TRUE),
      age_younger_primary_media = sum(duration[primary_media_age_younger], na.rm=TRUE),
      age_adults_primary_media = sum(duration[primary_media_age_adult], na.rm=TRUE),
      
      age_child_secondary_media = sum(duration[secondary_media_age_child], na.rm=TRUE),
      age_older_secondary_media = sum(duration[secondary_media_age_older], na.rm=TRUE),
      age_younger_secondary_media = sum(duration[secondary_media_age_younger], na.rm=TRUE),
      age_adults_secondary_media = sum(duration[secondary_media_age_adult], na.rm=TRUE),
      
      table_access = mean(table_access)==1,
      
      SBP_TV_lessthan_1h = total_tv_hours<= 1,
      SBP_avoid_screen_before_bedtime = screen_1hfromsleeping_hours==0,
      SBP_balance_media_with_reading = primary_book_hours>=0.5,
      SBP_balance_media_with_play = primary_media_hours <= play_hours,
      SBP_minimise_background_media_play = play_bmedia_hours == 0,
      SBP_avoid_media_meals = feeding_btv_hours == 0,
      SBP_coview = screen_adult_coviewing_hours/screen_hours >= 0.5,
      SBP_content = ((age_child_primary_media + age_younger_primary_media)/
          (age_child_primary_media + age_older_primary_media+ age_younger_primary_media + age_adults_primary_media))==1
    )
  return(summarydata)
}
