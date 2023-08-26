## Original Consent Dataset ----

# Byers, E. S., MacDougall, A., & Goldsmith, K. (2022, December 1). Sexual Consent Attitudes and Behaviour: Associations with Sexual Health Education, Sexual Consent Education, and Sexual Attitudes. Retrieved from osf.io/k4ydw
# Original Title: Data for Sexual Consent Attitudes and Behaviour Manuscript.csv

library(tidyverse)
origdf <- haven::read_sav('macdougall2022-consent-data.sav')


## Simplify dataset ----
cleandf <- origdf |>
  mutate(
    Gender =
      Gender |>
      haven::as_factor(),
    Ethnicity =
      Ethnicity |>
      haven::as_factor(),
    SexualOrientation =
      SexOrient |>
      haven::as_factor(),
    RelationshipStatus =
      RelStat |>
      haven::as_factor(),
    SexPartners =
      NumberSexPartR,
    ATSS = ATSSTotR,
    SexEd =
      SexEd_Consent |>
      haven::as_factor(),
    IndirectScore =
      IndirectM,
    Consent =
      Gave.ReceivedPercent
  ) |>
  select(
    SubjID,
    Gender,
    Age,
    Ethnicity,
    SexualOrientation,
    RelationshipStatus,
    SexPartners,
    ATSS,
    SexEd,
    IndirectScore,
    Consent
  )


## Reduce dataset ----
# Keep records for which there are a positive number of sexual partners so that
# 'consent' variables make sense.
cleandf <- cleandf |>
  filter(SexPartners > 0)


consent <- cleandf
usethis::use_data(consent, overwrite = TRUE)
