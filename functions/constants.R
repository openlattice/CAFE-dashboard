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
    "ChildCare_Weekdays",
    "ChildCare_Weekends",
    "Households",
    "RelatedTo",
    "Household_Communication",

    # Device Use
    "Device_Use",

    # quality
    "QualityControl",
    "ChildLanguageAssessment",
    "PSI_Assessment",
    "SurveyMetadata",
    "ChildrenDetailsHealth",
    "PublicAssistance",
    "ParentMediationScale",
    "Devices",
    "MediaUseAttitudes",
    "MediaDeviceUse",
    "MobileDeviceUse",
    "VocabularyAssessment_WG_8_18",
    "VocabularyAssessment_WS_18_30",
    "VocabularyAssessment_WS_30_38",
    "MediaAttitudes",
    "RegularlyLocatedDevices",
    "DeviceLocations",
    "MediaUseConcerns",
    "SleepTimes",
    "AwakeTimes",
    "InterruptedSleep",
    "SleepPatterns",
    "SleepArrangements",
    "SleepPositions",
    "SleepNight",
    "SleepDay",
    "NapDuringDay",
    "FallAsleep",
    "SleepConcerns",
    "MediaExposure",
    "ParentingMoodAssessment"
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
        dst = "ParentingMoodAssessment",
        edge = "AssessedBy"
    ),
    list(
        src = "Children",
        dst = "MediaUseConcerns",
        edge = "SubjectOf"
    ),
    list(
        src = "RegularlyLocatedDevices",
        dst = "DeviceLocations",
        edge = "LocatedAt"
    ),
    list(
        src = "MediaDevices",
        dst = "Households",
        edge = "OwnedBy"
    ),
    list(
        src = "Children",
        dst = "MediaExposure",
        edge = "SubjectOf"
    ),
    list(
        src = "Children",
        dst = "MobileDeviceUse",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "ChildCare_Weekdays",
        edge = "SubjectOf"
    ),
    list(
        src = "Children",
        dst = "ChildCare_Weekends",
        edge = "SubjectOf"
    ),
    list(
        src = "Children",
        dst = "Households",
        edge = "PartOf"
    ),
    list(
        src = "Children",
        dst = "Household_Communication",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "ChildLanguageAssessment",
        edge = "ScreenedWith"
    ),
    list(
        src = "Children",
        dst = "VocabularyAssessment_WG_8_18",
        edge = "ScreenedWith"
    ),
    list(
        src = "Children",
        dst = "VocabularyAssessment_WS_18_30",
        edge = "ScreenedWith"
    ),
    list(
        src = "Children",
        dst = "VocabularyAssessment_WS_30_38",
        edge = "ScreenedWith"
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
    list(
        src = "Children",
        dst = "SleepTimes",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "AwakeTimes",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "InterruptedSleep",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "SleepPatterns",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "SleepArrangements",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "SleepPositions",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "SleepNight",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "SleepDay",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "NapDuringDay",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "FallAsleep",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "SleepConcerns",
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
    list(
        src = "Respondents",
        dst = "Device_Use",
        edge = "InvolvedIn"
    ),
    list(
        src = "Respondents",
        dst = "MediaAttitudes",
        edge = "Reported"
    ),
    list(
        src = "Respondents",
        dst = "MediaExposure",
        edge = "Reported"
    ),

    # DEVICES

    list(
        src = "Children",
        dst = "Device_Use",
        edge = "InvolvedIn"
    ),
    list(
        src = "Children",
        dst = "Device_Use",
        edge = "UsedBy"
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
    ),
    list(
        src = "Children",
        dst = "MediaDeviceUse",
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
    src = 'people',
    dst = 'primary_activity',
    edge = 'engagedin'
  )

)
