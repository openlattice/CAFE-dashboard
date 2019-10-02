empty_plot <- function() {
    plot(0,
         type = 'n',
         axes = FALSE,
         ann = FALSE)
}

pie_hours_by_activity <- function(activitydata) {
    act_by_child <- activitydata %>%
        group_by(day_id, ol.activity) %>%
        summarise(hours = sum(duration) / 60) %>%
        ungroup() %>%
        complete_(cols = c("day_id", "ol.activity")) %>% replace_na(list(hours =
                                                                             0)) %>%
        group_by_('ol.activity') %>%
        summarise(hours = mean(hours))
    ggplot(act_by_child, aes(x = '', y = hours, fill = ol.activity)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        # geom_text(aes(label = paste0(round(hours), "%")), position = position_stack(vjust = 0.5))+
        scale_fill_manual(values = cols) +
        labs(
            x = NULL,
            y = NULL,
            fill = NULL,
            title = "Activities distribution per day"
        ) +
        theme_classic() + theme(
            axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666")
        )
}

plot_hours_by_activity <- function(activitydata, grouper = NULL) {
    if ('day_id' %in% names(activitydata)) {
        if (is.null(grouper)) {
            act_by_child <- activitydata %>%
                group_by(day_id, ol.activity) %>%
                summarise(hours = sum(duration) / 60) %>%
                ungroup() %>%
                complete_(cols = c("day_id", "ol.activity")) %>% replace_na(list(hours =
                                                                                     0)) %>%
                group_by_('ol.activity') %>%
                summarise(hours = mean(hours))
            
            plt <- ggplot(act_by_child,
                          aes(x = ol.activity, y = hours))
        } else {
            act_by_child <- activitydata %>%
                group_by_('day_id', 'ol.activity', grouper) %>%
                summarise(hours = sum(duration) / 60) %>%
                ungroup() %>%
                complete_(cols = c("day_id", "ol.activity", grouper)) %>% replace_na(list(hours =
                                                                                              0)) %>%
                group_by_('ol.activity', grouper) %>%
                summarise(hours = mean(hours))
            
            plt <- ggplot(act_by_child,
                          aes_string(
                              x = 'ol.activity',
                              y = 'hours',
                              fill = grouper
                          ))
        }
        
        plt <- plt +
            geom_bar(stat = "summary",
                     fun.y = "sum",
                     position = 'stack') +
            theme_light() +
            coord_flip() +
            scale_fill_manual(values = cols,
                              aesthetics = "fill",
                              na.value = nacol) +
            labs(y = "Average hours per day", x = "Activity")
        plt
    }
}

plot_total_hour_distribution <- function(activitydata) {
    if ('day_id' %in% names(activitydata)) {
        dur_by_child <- activitydata %>%
            group_by(child_id, day_id) %>%
            summarise(duration = sum(duration) / 60)
        
        plot <- ggplot(dur_by_child, aes(x = duration)) +
            geom_histogram(binwidth = 2, fill = cols[1]) +
            theme_light() +
            labs(x = "Total hours per day", y = "Frequency")
        
        plot
    }
}

plot_total_age_distribution <- function(maqdata) {
    if ('age_months' %in% names(maqdata)) {
        plot <-
            ggplot(maqdata,
                   aes(
                       x = age_months,
                       fill = study,
                       colour = study
                   )) +
            geom_histogram(alpha = 0.2, bw = 7) +
            theme_light() +
            labs(x = "Age in months", y = "Frequency") +
            scale_fill_manual(values = cols) +
            scale_color_manual(values = cols)
        
        
        plot
    }
}







plot_subjects_by_site <- function(rawdata) {
    act_by_child <- rawdata$tud$processed %>%
        group_by(child_id, study) %>% count() %>% mutate(source = "TUD")
    maq_by_child <-
        rawdata$maq$processed %>% mutate(study = replace(study, study == "BYU", "PM")) %>%
        group_by(child_id, study) %>% count() %>% mutate(source = "MAQ")
    all_by_child = rbind(act_by_child, maq_by_child) %>%
        group_by(child_id, study) %>%
        summarise(source = paste0(source, collapse = " + "))
    ggplot(all_by_child, aes(study, fill = source)) + geom_bar() +
        theme_light() +
        scale_fill_manual(values = cols[c(5, 4, 1)],
                          aesthetics = "fill",
                          na.value = nacol)
}


plot_barchart_activities <-
    function(activitydata, grouper1, grouper2) {
        if (is.null(grouper1) | is.null(grouper2)) {
            return(NULL)
        }
        if (grouper1 %in% names(activitydata) &
            grouper2 %in% names(activitydata) &
            (grouper1 != grouper2)) {
            act_by_child <- activitydata %>%
                group_by_('day_id', grouper1, grouper2) %>%
                summarise(hours = sum(duration) / 60) %>%
                complete_(cols = c("day_id", grouper1, grouper2)) %>% replace_na(list(hours =
                                                                                          0)) %>%
                ungroup() %>%
                group_by_(grouper1, grouper2) %>%
                summarise(hours = mean(hours))
            
            ggplot(activitydata,
                   aes_string(x = grouper1,
                              fill = grouper2)) +
                geom_bar() + theme_light() +
                scale_fill_manual(values = cols,
                                  aesthetics = "fill",
                                  na.value = nacol)
        }
    }



# FUNCTIONS






