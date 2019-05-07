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
    "RespondentDetails",

    # RESPONDENTS
    
    "RespondentDetails", 
    "Employment", 
    "Education", 
    "Incomes", 
    "ImmigrationStatus",
    
    # Device Use
    "Device_Use",
    
    # quality
    "QualityControl",
    "PSI_Assessment",
    "SurveyMetadata",
    "ChildrenDetailsHealth",
    "PublicAssistance",
    "ParentMediationScale",
    "Devices",
    "MediaUseAttitudes",
    "MediaDeviceUse",
    "VocabularyAssessment_WG_8_18",
    "VocabularyAssessment_WS_18_30",
    "VocabularyAssessment_WS_30_38"
)

MAQ_associations <- list(
    
    # SUBJECTS
    
    list(
        src = "Respondents",
        dst = "Children",
        edge = "RelatedTo"
    ),
    list(
        src = "Respondents",
        dst = "VocabularyAssessment_WG_8_18",
        edge = "Reported"
    ),
    list(
        src = "Respondents",
        dst = "VocabularyAssessment_WS_18_30",
        edge = "Reported"
    ),
    list(
        src = "Respondents",
        dst = "VocabularyAssessment_WS_30_38",
        edge = "Reported"
    ),
    list(
        src = "Children",
        dst = "ChildrenDetails",
        edge = "Has"
    ),
    list(
        src = "Respondents",
        dst = "SurveyMetadata",
        edge = "ParticipatedIn"
    ),
    list(
        src = "Respondents",
        dst = "RespondentDetails",
        edge = "Has"
    ),
    list(
        src = "Children",
        dst = "ChildrenDetailsHealth",
        edge = "Has"
    ),
    list(
        src= "Respondents",
        dst = "PublicAssistance",
        edge = "Reported"
    ),
    # RESPONDENTS
    
    list(
        src = "Respondents",
        dst = "RespondentDetails",
        edge = "Reported"
    ),
    list(
        src = "Respondents",
        edge = "Reported",
        dst = "MediaUseAttitudes"
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
    list(
        src = "Respondents",
        dst = "QualityControl",
        edge = "Reported"
    ),
    
    # DEVICES
    
    list(
        src = "Children",
        dst = "Device_Use",
        edge = "InvolvedIn"
    ),
    list(
        src = "Devices",
        dst = "Children",
        edge = "UsedBy"
    ),
    list(
        src = "Respondents",
        dst = "PSI_Assessment",
        edge = "ScreenedWith"
    ),
    list(
      src = "Respondents",
      dst = "MediaDeviceUse",
      edge = "Reported"
    ),
    list(
        src = "Respondents",
        dst = "ParentMediationScale",
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
    src = 'people',
    dst = 'primary_activity',
    edge = 'engagedin'
  )
  
)
