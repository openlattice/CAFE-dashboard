empty_plot <- function() {
    plot(0,
         type = 'n',
         axes = FALSE,
         ann = FALSE)
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
    cols <- c(
        'SBP_TV_lessthan_1h',
        'SBP_avoid_screen_before_bedtime',
        'SBP_balance_media_with_reading',
        'SBP_balance_media_with_play',
        'SBP_minimise_background_media_play',
        'SBP_avoid_media_meals',
        'SBP_coview',
        'SBP_content'
    )
    corr <- round(cor(summarydata[cols]), 1)
    ggcorrplot(corr,
               ggtheme = theme_light(),
               colors = c("#e9a3c9", "#f7f7f7", "#a1d76a"))
}

plot_barchart_activities <-
    function(activitydata, grouper1, grouper2) {
        if (is.null(grouper1) | is.null(grouper2)){
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
                by.x = "nc.SubjectIdentification",
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
        plt <- ggplot(summarydata, aes_string(x = var1, y = var2, color='site')) +
            geom_point() + theme_light() +
            stat_smooth(method = "lm") +
            scale_fill_manual(values = cols,
                              aesthetics = "fill",
                              na.value = nacol) +
            scale_colour_manual(values = cols,
                              aesthetics = "colour",
                              na.value = nacol) + guides(color=guide_legend(override.aes=list(fill=NA)))
        return(plt)
    }

# FUNCTIONS

qa_plot <- function(summarydata) {
    plt <- ggplot(summarydata, aes(x = site, y = progress)) +
        theme_light() +
        geom_bar(stat = "summary",
                 fun.y = "mean",
                 fill = cols[4])
    return(plt)
}


# FUNCTIONS

plot_maq <- function(summarydata, maqdata, tudcol, maqcol) {
    if (is.null(tudcol) | is.null(maqcol) | tudcol == "" | maqcol == ""){
        return(NULL)
    }
    togdata <- summarydata %>% inner_join(maqdata, by = "nc.SubjectIdentification")
    plt <- ggplot(togdata, aes_string(x = maqcol, y = tudcol)) +
        theme_light() +
        geom_bar(stat = "summary",
                 fun.y = "mean",
                 fill = cols[5]) +
        coord_flip()
    return(plt)
}

venn_plot <- function(rawdata){
    chronpi = unique(rawdata$chronicle$raw$pid)
    tudpi = unique(rawdata$tud$processed$nc.SubjectIdentification)
    maqpi = unique(rawdata$maq$processed$nc.SubjectIdentification)
    
    nC = length(chronpi)
    nT = length(tudpi)
    nM = length(maqpi)
    
    nCT = length(intersect(chronpi, tudpi))
    nCM = length(intersect(chronpi, maqpi))
    nTM = length(intersect(maqpi, tudpi))
    
    nCTM = length(intersect(intersect(chronpi, tudpi), maqpi))
    
    grid.newpage()
    draw.triple.venn(
        area1 = nC,
        area2 = nT,
        area3 = nM,
        n12 = nCT,
        n13 = nCM,
        n23 = nTM,
        n123 = nCTM,
        
        category = c("Chronicle", "TUD", "MAQ"),
        lty = 1,
        lwd = 2,
        fill = c(cols[1], cols[2], cols[4]),
        fontfamily = "sans",
        cex = 1.5,
        fontface = "bold",
        cat.fontfamily = "sans",
        cat.fontface = "bold",
        cat.col = c(cols[1], cols[2], cols[4]),
        scaled=TRUE,
        euler.D = TRUE,
        cat.dist = 0.03,
        cat.pos = c(-10,5,5),
        label.col = "white",
        alpha = 0.8
    )
    
}





