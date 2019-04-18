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
            geom_density(alpha = 0.2, bw = 7) +
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
# plot_chronicle_histogram <- function(chronicle) {
#     if (column %in% names(summarydata)) {
#         ggplot(summarydata,
#                aes_string(x = column)) +
#             geom_histogram(binwidth = 1,
#                            fill = "#4c14c4") +
#             scale_fill_manual(values = cols,
#                               aesthetics = "fill",
#                               na.value = nacol)
#     }
# }

plot_crossplot <- function(summarydata, crosscols) {
    if (length(crosscols) > 0) {
        ggpairs(
            summarydata[, crosscols],
            diag = list(continuous = wrap("densityDiag", fill = cols[1])),
            lower = list(continuous = wrap("smooth", colour = cols[2]))
        )
    }
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

plot_tud_chron <-
    function(summarydata, chronicle, var1, var2) {
        new <-
            merge(
                summarydata,
                chronicle,
                how = "inner",
                by.x = "child_id",
                by.y = "pid"
            )
        plt <- ggplot(new, aes_string(x = var1, y = var2)) +
            geom_point() + theme_light() +
            stat_smooth(method = "lm", col = cols[1]) +
            labs(x = "Chronicle: average usage per day",
                 y = paste("Tud:", var2))
        return(plt)
    }

plot_by_study <-
    function(summarydata, var1, var2) {
        plt <-
            ggplot(summarydata, aes_string(x = var1, y = var2, color = 'study')) +
            geom_point() + theme_light() +
            stat_smooth(method = "lm") +
            scale_fill_manual(values = cols,
                              aesthetics = "fill",
                              na.value = nacol) +
            scale_colour_manual(values = cols,
                                aesthetics = "colour",
                                na.value = nacol) + guides(color = guide_legend(override.aes =
                                                                                    list(fill = NA)))
        return(plt)
    }

# FUNCTIONS

qa_plot <- function(summarydata) {
    plt <- ggplot(summarydata, aes(x = study, y = progress)) +
        theme_light() +
        geom_bar(stat = "summary",
                 fun.y = "mean",
                 fill = cols[4])
    return(plt)
}


# FUNCTIONS

# #cat
# tudcol = "SBP_avoid_media_meals"
# maqcol = "education"
# 
# # cont
# maqcol = "age_months"
# tudcol = "progress"

plot_maq <- function(rawdata, tudcol, maqcol) {
    summarydata <- rawdata$tud$summarised
    maqdata <- rawdata$maq$processed
    if (is.null(tudcol) |
        is.null(maqcol) | tudcol == "" | maqcol == "") {
        return(NULL)
    }
    togdata <-
        summarydata %>% inner_join(maqdata, by = "child_id")
    
    tudnum = (typeof(summarydata[[tudcol]]) == "double" | typeof(summarydata[[tudcol]]) == "integer")
    maqnum = (typeof(maqdata[[maqcol]]) == "double" | typeof(maqdata[[maqcol]]) == "integer")
    if (tudnum & maqnum) {
        plt <- ggplot(togdata, aes_string(x = maqcol, y = tudcol)) +
            geom_point(color=cols[9]) + geom_smooth(method = lm, color=cols[1])
    } else if (tudnum & !maqnum) {
        plt <- ggplot(togdata, aes_string(x = maqcol, y = tudcol, fill = maqcol)) +
            geom_violin(bw = "nrd", scale='count') + coord_flip() +
            stat_summary(fun.y=median, geom="point", size=2, color="black")
    } else if (!tudnum & maqnum) {
        plt <- ggplot(togdata, aes_string(x = tudcol, y = maqcol, fill = tudcol)) +
            geom_violin(bw = "nrd", scale='count') + coord_flip() +
            stat_summary(fun.y=median, geom="point", size=2, color="black")
    } else {
        plt <- ggplot(togdata, aes_string(x = maqcol, y = tudcol)) +
            geom_bar(stat = "summary",
                              fun.y = "mean",
                              fill = cols[5])
    }
    plt <- plt + 
        scale_fill_manual(values = cols,
                          aesthetics = "fill",
                          na.value = nacol) +
        theme_minimal() + theme(legend.position = "none")
    return(plt)
}









