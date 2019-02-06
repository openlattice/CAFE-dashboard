library(shiny)

cols <-
  c("#ff3c5d",
    "#6124e2",
    "#ffe671",
    "#ff9a58",
    "#dd9e00",
    "#00be84")
nacol <- "#dcdce7"

plot_hours_by_activity <- function(activitydata, grouper = NULL) {
  if ('child_id' %in% names(activitydata)){
    if (is.null(grouper)) {
      act_by_child <- activitydata %>%
        group_by(child_id, ol.activity) %>%
        summarise(duration = sum(duration) / 60)
      plt <- ggplot(act_by_child,
                     aes(x = ol.activity, y = duration))
    } else {
      act_by_child <- activitydata %>%
        group_by_('child_id', 'ol.activity', grouper) %>%
        summarise(duration = sum(duration) / 60)
      plt <- ggplot(act_by_child,
                     aes_string(x = 'ol.activity', y = 'duration', fill = grouper))
    }
    
    plt <- plt +
      geom_bar(stat = "summary", fun.y = "mean") +
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
  if ('child_id' %in% names(activitydata)){
    dur_by_child <- activitydata %>%
      group_by(child_id) %>%
      summarise(duration = sum(duration) / 60)
    
    plot <- ggplot(dur_by_child, aes(x=duration)) +
      geom_histogram(binwidth=2, fill = cols[1]) +
      theme_light() + 
      labs(x = "Total hours per day", y = "Frequency")
    
    plot
  }
}

plot_summary_histogram <- function(summarydata, column){
  ggplot(summarydata,
         aes_string(x = column)) +
    geom_histogram(binwidth = 1,
                   fill = "#4c14c4") +
    scale_fill_manual(values = cols,
                      aesthetics = "fill",
                      na.value = nacol)
}


plot_crossplot <- function(summarydata, crosscols){
  if (length(crosscols) > 0) {
    ggpairs(
      summarydata[, crosscols],
      color = "black",
      diag = list(continuous = wrap("densityDiag", fill = cols[1])),
      lower = list(continuous = wrap("smooth", colour = cols[2]))
    )
  }
}

plot_barchart_activities <- function(activitydata, grouper1, grouper2) {
  ggplot(
    activitydata,
    aes_string(
      x = grouper1,
      fill = grouper2
    )
  ) +
    geom_bar() + theme_light() +
    scale_fill_manual(values = cols,
                      aesthetics = "fill",
                      na.value = nacol)
}


