process_chronicle <- function(chronicledata) {
    chronicle = chronicledata %>% 
        mutate(ol.datetimestart = as_date(ol.datetimestart)) %>%
        mutate(date = round_date(ol.datetimestart, 'day')) %>% 
        group_by(study, pid, date) %>%
        summarise(
            totaltime = sum(general.Duration) / 3600
        ) %>%
        group_by(study, pid) %>%
        summarise(
            meantime = mean(totaltime)
        )
    return(chronicle)
}











                     