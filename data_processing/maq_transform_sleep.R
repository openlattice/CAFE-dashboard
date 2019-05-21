put_date <- function(x, offset = 0){
    year(x) = 1999
    month(x) = 12
    day(x) = 31 + offset
    return(x)
}

childsleep_transform <- function(rawdata) {
    sleeptimes = recombine(list("Children", "SleepTimes"), rawdata) %>%
        mutate(
            sleep = ymd_hms(SleepTimes.ol.datetimestart),
            sleep = put_date(sleep, offset = -1)
            ) %>% 
        select(child_id, sleep)
    awaketimes = recombine(list("Children", "AwakeTimes"), rawdata) %>%
        mutate(
            awake = ymd_hms(AwakeTimes.ol.datetimestart),
            awake = put_date(awake)
        ) %>% 
        select(child_id, awake)
    sleepawake = sleeptimes %>% 
        left_join(awaketimes, by = "child_id") %>%
        mutate(
                sleep_total_asleep = as.numeric(awake - sleep)
            ) %>% 
        group_by(child_id) %>%
        slice(1) %>%
        ungroup()
 
    interruptedtimes = recombine(list("Children", "InterruptedSleep"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_interruptions = mean(as.numeric(InterruptedSleep.ol.interruptioncount))
        )
    
    sleeppatterns = recombine(list("Children", "SleepPatterns"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_normal_pattern = first(SleepPatterns.neurological.sleeppattern)
        )
    
    sleeparrangements = recombine(list("Children", "SleepArrangements"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_arrangement_own_room = sum(str_detect(SleepArrangements.ol.arrangement, "separate room"), na.rm=TRUE)>0,
            sleep_arrangement_parents_room = sum(str_detect(SleepArrangements.ol.arrangement, "parent's room"), na.rm=TRUE)>0,
            sleep_arrangement_siblings_room = sum(str_detect(SleepArrangements.ol.arrangement, "sibling"), na.rm=TRUE)>0
        )
    
    sleeppositions = recombine(list("Children", "SleepPositions"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_positions_back = sum(str_detect(SleepPositions.ol.position, "Back")) > 0,
            sleep_positions_side = sum(str_detect(SleepPositions.ol.position, "Side")) > 0,
            sleep_positions_belly = sum(str_detect(SleepPositions.ol.position, "Belly")) > 0
        )
    
    sleepnight = recombine(list("Children", "SleepNight"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_hours_night = mean(as.numeric(SleepNight.ol.durationhours))
        )
    
    sleepday = recombine(list("Children", "SleepDay"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_hours_day = mean(as.numeric(SleepDay.ol.durationhours))
        )

    naps = recombine(list("Children", "NapDuringDay"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_napped_yesterday = sum(str_detect(NapDuringDay.ol.status, "Yes"))>0,
            sleep_napped_yesterday_duration = mean(as.numeric(NapDuringDay.ol.durationhours)),
            sleep_napped_yesterday_times = mean(as.numeric(NapDuringDay.ol.eventcount))
        )
    
    fallasleep = recombine(list("Children", "FallAsleep"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_duration_to_sleep = mean(as.numeric(FallAsleep.ol.durationhours)),
            sleep_falls_asleep_feeding = sum(str_detect(FallAsleep.ol.method, "feeding"))>0,
            sleep_falls_asleep_rocked = sum(str_detect(FallAsleep.ol.method, "rocked"))>0,
            sleep_falls_asleep_held = sum(str_detect(FallAsleep.ol.method, "held"))>0,
            sleep_falls_asleep_bed_alone = sum(str_detect(FallAsleep.ol.method, "bed alone"))>0,
            sleep_falls_asleep_bed_parent = sum(str_detect(FallAsleep.ol.method, "bed near parent"))>0,
            sleep_falls_asleep_parents_bed = sum(str_detect(FallAsleep.ol.method, "parents' bed"))>0
        )
    
    
    sleepconcerns = recombine(list("Children", "SleepConcerns"), rawdata) %>%
        group_by(child_id) %>%
        summarise(
            sleep_concerns = paste0(SleepConcerns.ol.concerns, collapse=", ")
        )
    
    sleep = sleepawake %>%
        left_join(interruptedtimes, by = "child_id") %>%
        left_join(sleeppatterns, by = "child_id") %>%
        left_join(sleeparrangements, by = "child_id") %>%
        left_join(sleeppositions, by = "child_id") %>%
        left_join(sleepnight, by = "child_id") %>%
        left_join(sleepday, by = "child_id") %>%
        left_join(naps, by = "child_id") %>%
        left_join(fallasleep, by = "child_id") %>%
        left_join(sleepconcerns, by = "child_id")
        
    
    
        return(sleep)
}