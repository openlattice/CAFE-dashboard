process_chronicle <- function(rawdata) {
    if (rawdata$auth == FALSE) {
        return (tibble())
    }
    chronicle = rawdata$chronicle$raw %>% 
        rename(child_id = pid) %>%
        mutate(date = as_date(as_datetime(ol.datetimestart))) %>%
        group_by(study, child_id, date, table_access) %>%
        summarise(
            totaltime = sum(general.Duration) / 3600
        ) %>%
        group_by(study, child_id, table_access) %>%
        summarise(
            meantime = mean(totaltime),
        )  %>%
        mutate(
            study_id = str_sub(child_id, start = 1, end = 2)
            ) %>% select(-c(study))
    return(chronicle)
}










                     