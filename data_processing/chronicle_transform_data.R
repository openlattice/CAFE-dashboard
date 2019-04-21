process_chronicle <- function(rawdata) {
    if (rawdata$auth == FALSE) {
        return (tibble())
    }
    chronicle = rawdata$chronicle$raw %>% 
        rename(child_id = pid) %>%
        mutate(date = as_date(as_datetime(ol.datetimestart))) %>%
        group_by(study, child_id, date) %>%
        summarise(
            totaltime = sum(general.Duration) / 3600
        ) %>%
        group_by(study, child_id) %>%
        summarise(
            meantime = mean(totaltime)
        ) %>% select(-c(study))
    return(chronicle)
}











                     