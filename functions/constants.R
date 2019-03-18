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
    "Respondents",
    "ChildrenDetails",

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
        src = "Respondents",
        dst = "Children",
        edge = "RelatedTo"
    ),
    list(
        src = "Children",
        dst = "ChildrenDetails",
        edge = "Has"
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
