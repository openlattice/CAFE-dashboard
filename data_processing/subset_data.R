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
                      alldata = rawdata$alldata
                      )
        
        # subset hours
        if (hourbool) {
            print("hours")
            subset <- rawdata$tud$summarised %>%
                filter(total_time > hourrange[1] &
                           total_time < hourrange[2]) %>% select("nc.SubjectIdentification", "day_id")
            output$tud <- output$tud %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification) %>%
                filter(day_id %in% subset$day_id)
            output$maq <- output$maq %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$nc.SubjectIdentification)
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
        }
        print(dim(output$tud))
        
        if (progressbool) {
            print("progress")
            subset <- rawdata$tud$summarised %>%
                filter(progress > progressrange[1] &
                           progress < progressrange[2]) %>% select("nc.SubjectIdentification", "day_id")
            output$tud <- output$tud %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification) %>%
                filter(day_id %in% subset$day_id)
            output$maq <- output$maq %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$nc.SubjectIdentification)
        }
        
        if (agebool) {
            print("age")
            subset <- rawdata$maq$preprocessed %>%
                filter(age_months > agerange[1] &
                           age_months < agerange[2]) %>% select("nc.SubjectIdentification")
            output$tud <- output$tud %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            output$maq <- output$maq %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$nc.SubjectIdentification)
            
        }
        
        if (qualitybool) {
            print("quality")
            subset <- rawdata$maq$preprocessed %>%
                filter(mean_quality > qualityrange[1] &
                           mean_quality < qualityrange[2]) %>% select("nc.SubjectIdentification")
            output$tud <- output$tud %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            output$maq <- output$maq %>%
                filter(nc.SubjectIdentification %in% subset$nc.SubjectIdentification)
            output$alldata <- output$alldata %>%
                filter(child_id %in% subset$nc.SubjectIdentification)
            
        }
        
        output$summary = summarise_data(output$tud)
        
        return(output)
    }
