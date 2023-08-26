## Original Drinking Dataset ----

# Cummings, J. R., Ray, L. A., Nooteboom, P. & Tomiyama, A. J. (2020). Acute effect of eating sweets on alcohol cravings in a sample with at-risk drinking. Annals of Behavioral Medicine, 54(2), 132-138. doi: 10.1093/abm/kaz031
# Original Title: FACT_No age.sav

library(tidyverse)
origdf <- haven::read_sav('cummings2020-drinking-data.sav')


## Simplify dataset ----
cleandf <- origdf |>
  mutate(
    Gender = haven::as_factor(Sex),
    Race = haven::as_factor(Race) |>
      str_remove_all(pattern = '[:digit:]\\)\\t|\\(a\\)'),
    Condition = haven::as_factor(Condition),
    CravingDiff = Craving_T2 - Craving_T1,
    Alc_Pref = haven::as_factor(Alc_Pref)
  ) |>
  dplyr::select(
    ID,
    Gender,
    Race,
    AUDIT,
    Height,
    Weight,
    BMI,
    Condition,
    Craving_Base,
    Craving_T1,
    Craving_T2,
    CravingDiff,
    AlcoholPref = Alc_Pref
  )


## Reduce dataset ----
# Only consider the bland vs. high fat food
cleandf <- cleandf |>
  mutate(
    Condition =
      case_match(
        Condition,
        'Bland food x small dose' ~ 'Bland',
        'Sweet high fat food x small dose' ~ 'Sweet',
        'Bland food x large dose' ~ 'Bland',
        'Sweet high-fat food x large dose' ~ 'Sweet'
      )
  ) |>
  filter(!is.na(Condition))

drinking <- cleandf
usethis::use_data(drinking, overwrite = TRUE)
