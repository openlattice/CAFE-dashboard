TUD_entities <- c(
  "primary_activity", 
  "survey_metadata", 
  "people", 
  "relatives", 
  "devices", 
  "adult_use", 
  "media_exposure", 
  "locations",
  "sites",
  "survey_respondents",
  "survey_visits",
  'survey_recruitment',
  "recorded"
  )

MAQ_entities <- c(
    
    # SUBJECTS
    
    "Children",
    "HouseHolds",
    "ChildCare_Weekdays",
    "ChildCare_Weekends",
    "Household_Communication",
    "Respondents",
    "ChildrenDetails",
    "ChildrenDetailsHealth",
    "Caregiver_Communication",

    # RESPONDENTS
    
    "RespondentDetails", 
    "Employment", 
    "Education", 
    "Incomes", 
    "ImmigrationStatus"
)

MAQ_associations <- list(
    
    # SUBJECTS
    
    list(
        src = "Children",
        dst = "Households",
        edge = "PartOf"
    ),
    list(
        src = "Respondents",
        dst = "Households",
        edge = "PartOf"
    ),
    list(
        src = "Children",
        dst = "ChildCare_Weekdays",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "Household_Communication",
        edge = "InvolvedIn"
    ),
    list(
        src = "Respondents",
        dst = "Children",
        edge = "RelatedTo"
    ),
    list(
        src = "Children",
        dst = "ChildrenDetails",
        edge = "Has"
    ),
    list(
        src = "Children",
        dst = "ChildrenDetailsHealth",
        edge = "Has"
    ),
    list(
        src = "Respondents",
        dst = "ChildCare_Weekends",
        edge = "InvolvedIn"
    ),
    list(
        src = "Subjects",
        dst = "Caregiver_Communication",
        edge = "InvolvedIn"
    ),
    
    # RESPONDENTS
    
    list(
        src = "Respondents",
        dst = "RespondentDetails",
        edge = "Reported"
    ),
    list(
        src = "Respondents",
        dst = "Employment",
        edge = "Reported"
    ),
    list(
        src = "Respondents",
        dst = "Education",
        edge = "Reported"
    ),
    list(
        src = "Respondents",
        dst = "Incomes",
        edge = "Reported"
    ),
    list(
        src = "Respondents",
        dst = "ImmigrationStatus",
        edge = "Reported"
    ),
    
    # DEVICES
    
    list(
        src = "Devices",
        dst = "Respondents",
        edge = "UsedBy"
    ),
    list(
        src = "Respondents",
        dst = "Activities",
        edge = "EngagedIn"
    ),
    list(
        src = "Respondents",
        dst = "MediaAttitudes",
        edge = "Reported"
    ),
    list(
        src = "Activities",
        dst = "Device_Use",
        edge = "ConcurrentTo"
    ),
    list(
        src = "Respondents",
        dst = "Device_Use",
        edge = "InvolvedIn"
    )
    
    
)


TUD_associations <- list(
    list(
    src = 'devices',
    dst = 'media_exposure',
    edge = 'involvedin'
  ),
    list(
        src = "primary_activity",
        dst = "survey_metadata",
        edge = "recorded"
    ),
  list(
      src = 'survey_respondents',
      dst = 'primary_activity',
      edge = 'involvedin'
  ),
  list(
      src = 'survey_respondents',
      dst = 'primary_activity',
      edge = 'involvedin'
  ),
  list(
      src = 'survey_recruitment',
      dst = 'primary_activity',
      edge = 'involvedin'
  ),
  list(
      src = 'survey_visits',
      dst = 'primary_activity',
      edge = 'involvedin'
  ),
  list(
    src = 'media_exposure',
    dst = 'primary_activity',
    edge = 'involvedin'
  ),
  list(
    src = 'relatives',
    dst = 'primary_activity',
    edge = 'involvedin'
  ),
  list(
    src = 'media_exposure',
    dst = 'adult_use',
    edge = 'concurrentto'
  ),
  list(
    src = 'primary_activity',
    dst = 'adult_use',
    edge = 'concurrentto'
  ),
  list(
    src = 'primary_activity',
    dst = 'locations',
    edge = 'located'
  ),
  list(
      src = 'primary_activity',
      dst = 'sites',
      edge = 'located'
  ),
  list(
    src = 'primary_activity',
    dst = 'survey_metadata',
    edge = 'recorded'
  ),
  list(
    src = 'people',
    dst = 'primary_activity',
    edge = 'engagedin'
  )
  
)
