library(shiny)
library(GGally)

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
  if ('child_id' %in% names(activitydata)) {
    if (is.null(grouper)) {
      act_by_child <- activitydata %>%
        group_by(child_id, ol.activity) %>%
        summarise(hours = sum(duration) / 60) %>%
        complete(child_id, ol.activity) %>% replace_na(list(hours=0))
        
      plt <- ggplot(act_by_child,
                    aes(x = ol.activity, y = hours))
    } else {
      act_by_child <- activitydata %>%
        group_by_('child_id', 'ol.activity', grouper) %>%
        summarise(hours = sum(duration) / 60) %>%
        complete_(cols = c("child_id", "ol.activity", grouper)) %>% replace_na(list(hours=0))
      
      plt <- ggplot(act_by_child,
                    aes_string(x = 'ol.activity', y = 'hours', fill = grouper))
    }
    
    plt <- plt +
      geom_bar(stat = "summary", fun.y = "mean", position="identity") +
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
  if ('child_id' %in% names(activitydata)) {
    dur_by_child <- activitydata %>%
      group_by(child_id) %>%
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
    if (grouper1 %in% names(activitydata) &
        grouper2 %in% names(activitydata)) {
      ggplot(activitydata,
             aes_string(x = grouper1,
                        fill = grouper2)) +
        geom_bar() + theme_light() +
        scale_fill_manual(values = cols,
                          aesthetics = "fill",
                          na.value = nacol)
    }
  }
