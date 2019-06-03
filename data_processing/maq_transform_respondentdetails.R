######
## Respondent details
######

respondentdetails_transform <- function(rawdata, children) {
    respondentdetails = recombine(list("Respondents", "RespondentDetails"), rawdata) %>%
    left_join(children, by = 'respondent_id') %>%
    group_by(child_id) %>%
    summarise(
        parental_mean_age = mean(as.numeric(
            RespondentDetails.person.ageatevent
        )),
        parental_mean_numchildren = sum(as.numeric(
            RespondentDetails.ol.numberofchildren
        )),
        parental_marital = paste(
            unique(RespondentDetails.person.maritalstatus),
            collapse = ", "
        ),
        parental_nationality = paste(unique(RespondentDetails.ol.nationality), collapse = ", ")
    ) %>%
    select(
        child_id,
        parental_mean_age,
        parental_mean_numchildren,
        parental_marital,
        parental_nationality
    )
    return(respondentdetails)
}

######
## Incomes
######

incomes_transform <- function(rawdata, children) {
    incomelevels = list(
    "Less than $10,000",
    "$10,000 - $19,999",
    "$20,000 - $29,999",
    "$30,000 - $39,999",
    "$40,000 - $49,999",
    "$50,000 - $59,999",
    "$60,000 - $69,999",
    "$70,000 - $79,999",
    "$80,000 - $89,999",
    "$90,000 - $99,999",
    "$100,000 - $149,999",
    "More than $150,000",
    "Don't Know/Prefer not to answer"
)

incomes = recombine(list("Respondents", "Incomes"), rawdata) %>%
    left_join(children, by = 'respondent_id') %>%
    mutate(
        income = as.factor(Incomes.ol.type),
        income = fct_relevel(income, levels = incomelevels)
    ) %>%
    arrange(income) %>%
    group_by(child_id) %>%
    summarise(parental_highest_income = last(income))
return(incomes)
}

######
## Public Assistance
######

public_assistance_transform <- function(rawdata, children) {
    assistancelevels = list("No - never", "Yes - in past", "Yes - current")

public_assistance = recombine(list("Respondents", "PublicAssistance"), rawdata) %>%
    left_join(children, by = 'respondent_id') %>%
    mutate(
        assistance = as_factor(PublicAssistance.ol.type),
        assistance = fct_relevel(assistance, levels = assistancelevels)
    ) %>%
    arrange(assistance) %>%
    group_by(child_id) %>%
    summarise(parental_least_public_assistance = first(assistance)) %>%
    select(child_id, parental_least_public_assistance)
return(public_assistance)
}

######
## Employment
######

employment_transform <- function(rawdata, children) {
    employment = recombine(list("Respondents", "Employment"), rawdata) %>%
    left_join(children, by = 'respondent_id') %>%
    group_by(child_id) %>%
    summarise(employment = paste(unique(Employment.ol.status), collapse =
                                     ",")) %>%
    mutate(
        parental_employment = as_factor(ifelse(
            str_detect(employment, ","), NA, employment
        )),
        parental_employment = fct_relevel(
            parental_employment,
            levels = list(
                "No",
                "Maternity / parental leave",
                "One part-time job",
                "One full-time job",
                "Multiple jobs"
            )
        )
    ) %>%
    select(child_id, parental_employment)
    return(employment)
}

######
## Education
######

education_transform <- function(rawdata, children) {
    education_levels = list(
    "No formal school"  ,
    "Middle school",
    "High school or equivalent (e.g., GED)",
    "Some College or Vocational degree",
    "Bachelor's degree",
    "Master's degree",
    "Doctoral or Professional degree"
)
education = recombine(list("Respondents", "Education"), rawdata) %>%
    left_join(children, by = 'respondent_id') %>%
    mutate(parental_education = fct_relevel(
        as_factor(Education.person.highesteducation),
        education_levels
    )) %>%
    group_by(child_id) %>%
    summarise(parental_education = last(fct_relevel(
        parental_education, education_levels
    ))) %>% select(parental_education, child_id)
return(education)
}

