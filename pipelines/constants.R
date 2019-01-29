TUD_entities <- c(
  "primary_activity", 
  "survey_metadata", 
  "people", 
  "relatives", 
  "devices", 
  "adult_use", 
  "media_exposure", 
  "locations"
  )

TUD_associations <- list(
  list(
    src = 'devices',
    dst = 'media_exposure',
    edge = 'involved_in'
  ),
  list(
    src = 'media_exposure',
    dst = 'primary_activity',
    edge = 'involved_in'
  ),
  list(
    src = 'relatives',
    dst = 'primary_activity',
    edge = 'involved_in'
  ),
  list(
    src = 'media_exposure',
    dst = 'adult_use',
    edge = 'concurrent_to'
  ),
  list(
    src = 'primary_activity',
    dst = 'adult_use',
    edge = 'concurrent_to'
  ),
  list(
    src = 'primary_activity',
    dst = 'locations',
    edge = 'located'
  ),
  list(
    src = 'primary_activity',
    dst = 'survey_metadata',
    edge = 'recorded_via'
  ),
  list(
    src = 'people',
    dst = 'primary_activity',
    edge = 'engaged_in'
  )
)
