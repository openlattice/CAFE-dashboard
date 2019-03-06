library(shiny)
library(GGally)
library(ggcorrplot)

cols <-
  c("#ff3c5d",
    "#6124e2",
    "#ffe671",
    "#ff9a58",
    "#dd9e00",
    "#00be84")
nacol <- "#dcdce7"


empty_plot <- function() {
  plot(0,type='n',axes=FALSE,ann=FALSE)
}

plot_hours_by_activity <- function(activitydata, grouper = NULL) {
  if ('day_id' %in% names(activitydata)) {
    if (is.null(grouper)) {
      act_by_child <- activitydata %>%
        group_by(day_id, ol.activity) %>%
        summarise(hours = sum(duration) / 60) %>%
        ungroup() %>%
        complete_(cols = c("day_id", "ol.activity")) %>% replace_na(list(hours=0)) %>%
          group_by_( 'ol.activity') %>%
          summarise(hours = mean(hours))
      
      plt <- ggplot(act_by_child,
                    aes(x = ol.activity, y = hours))
    } else {
      act_by_child <- activitydata %>%
        group_by_('day_id', 'ol.activity', grouper) %>%
        summarise(hours = sum(duration) / 60) %>%
        ungroup() %>%
        complete_(cols = c("day_id", "ol.activity", grouper)) %>% replace_na(list(hours=0)) %>%
        group_by_( 'ol.activity', grouper) %>%
        summarise(hours = mean(hours))
    
      plt <- ggplot(act_by_child,
                    aes_string(x = 'ol.activity', y = 'hours', fill = grouper))
    }
    
    plt <- plt +
      geom_bar(stat = "summary", fun.y = "sum", position='stack') +
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
      group_by(day_id) %>%
      summarise(duration = sum(duration) / 60)
    
    plot <- ggplot(dur_by_child, aes(x = duration)) +
      geom_histogram(binwidth = 2, fill = cols[1]) +
      theme_light() +
      labs(x = "Total hours per day", y = "Frequency")
    
    plot
  }
}

plot_summary_histogram <- function(summarydata, column) {
  if (column %in% names(summarydata)) {
    ggplot(summarydata,
           aes_string(x = column)) +
      geom_histogram(binwidth = 1,
                     fill = "#4c14c4") +
      scale_fill_manual(values = cols,
                        aesthetics = "fill",
                        na.value = nacol)
  }
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

plot_sbp <- function(summarydata) {
    cols <- c('SBP_TV_lessthan_1h', 'SBP_avoid_screen_before_bedtime',
      'SBP_balance_media_with_reading', 'SBP_balance_media_with_play', 'SBP_minimise_background_media_play',
      'SBP_avoid_media_meals', 'SBP_coview', 'SBP_content')
    corr <- round(cor(summarydata[cols]),1)
    ggcorrplot(
        corr,
        ggtheme = theme_light(),
        colors = c("#e9a3c9", "#f7f7f7", "#a1d76a")
        )
}

plot_barchart_activities <-
  function(activitydata, grouper1, grouper2) {
    if (grouper1 %in% names(activitydata) &
        grouper2 %in% names(activitydata) & (grouper1 != grouper2)) {
    
        act_by_child <- activitydata %>%
            group_by_('day_id', grouper1, grouper2) %>%
            summarise(hours = sum(duration) / 60) %>%
            complete_(cols = c("day_id", grouper1, grouper2)) %>% replace_na(list(hours=0)) %>%
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
    function(summarydata, chronicle, var1, var2){
        new <- merge(summarydata, chronicle, how="inner", by.x = "nc.SubjectIdentification", by.y = "pid")
        plt <- ggplot(new, aes_string(x = var1, y = var2)) +
            geom_point() + theme_light() +
            stat_smooth(method="lm", col = cols[1]) +
            labs(
                x = "Chronicle: average usage per day",
                y = paste("Tud:", var2)
            )
        return(plt)
    }

