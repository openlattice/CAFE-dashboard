# out = subset_data(rawdata, hourbool=TRUE, hourrange = c(18,24), agebool = TRUE, agerange=c(0,100),
#             sitesbool = TRUE, sitesrange = c("PM", "UM"), progressbool = TRUE, progressrange = c(0,100),
#             qualitybool = TRUE, qualityrange = c(0, 100)
#             )

subset_data <-
    function(rawdata,
             hourbool=FALSE,
             hourrange=NULL,
             agebool=FALSE,
             agerange=NULL,
             sitesbool=FALSE,
             sitesrange=NULL,
             progressbool=FALSE,
             progressrange=NULL,
             qualitybool=FALSE,
             qualityrange=NULL) {
        output = list(tud = rawdata$tud$preprocessed,
                      maq = rawdata$maq$preprocessed,
                      alldata = rawdata$alldata_complete,
                      chronicle = rawdata$chronicle$preprocessed
                      )
        
        # subset hours
        if (hourbool) {
            print("hours")
            subset <- rawdata$tud$summarised %>%
                filter(total_time > hourrange[1] &
                           total_time < hourrange[2]) %>% select("child_id", "day_id")
            output$tud <- output$tud %>%
                filter(child_id %in% subset$child_id) %>%
                filter(day_id %in% subset$day_id)
            output$maq <- output$maq %>%
                filter(child_id %in% subset$child_id)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$child_id)
            outpu$chronicle <- output$chronicle %>%
                filter(child_id %in% subset$child_id)
        }
        print(dim(output$tud))
        
        # subset sites
        if (sitesbool) {
            print("sites")
            sites = paste0(sitesrange, collapse = "|")
            output$tud <-
                output$tud %>% filter(str_detect(study, sites))
            output$maq <-
                output$maq %>% filter(str_detect(study, sites))
            output$alldata <- output$alldata %>%
                filter(str_detect(study, sites))
            output$chronicle <- output$chronicle %>%
                filter(str_detect(study, sites))
        }
        print(dim(output$tud))
        
        if (progressbool) {
            print("progress")
            subset <- rawdata$tud$summarised %>%
                filter(progress > progressrange[1] &
                           progress < progressrange[2]) %>% select("child_id", "day_id")
            output$tud <- output$tud %>%
                filter(child_id %in% subset$child_id) %>%
                filter(day_id %in% subset$day_id)
            output$maq <- output$maq %>%
                filter(child_id %in% subset$child_id)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$child_id)
            outpu$chronicle <- output$chronicle %>%
                filter(child_id %in% subset$child_id)
        }
        
        if (agebool) {
            print("age")
            subset <- rawdata$maq$preprocessed %>%
                filter(age_months > agerange[1] &
                           age_months < agerange[2]) %>% select("child_id")
            output$tud <- output$tud %>%
                filter(child_id %in% subset$child_id)
            output$maq <- output$maq %>%
                filter(child_id %in% subset$child_id)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$child_id)
            outpu$chronicle <- output$chronicle %>%
                filter(child_id %in% subset$child_id)
        }
        
        if (qualitybool) {
            print("quality")
            subset <- rawdata$maq$preprocessed %>%
                filter(mean_quality > qualityrange[1] &
                           mean_quality < qualityrange[2]) %>% select("child_id")
            output$tud <- output$tud %>%
                filter(child_id %in% subset$child_id)
            output$maq <- output$maq %>%
                filter(child_id %in% subset$child_id)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$child_id)
            output$chronicle <- output$chronicle %>%
                filter(child_id %in% subset$child_id)
            
        }
        
        output[['n_act']] = dim(output$tud)[1]
        output[['n_child']] = dim(output$alldata)[1]
        output[['n_nodes']] = length(names(rawdata$tud$nodes)) + length(names(rawdata$maq$edges))
        output$summary = summarise_data(output$tud)

        return(output)
    }
