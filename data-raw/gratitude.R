## Original Gratitude Dataset ----

# mead, jessica. (2021, June 11). Protectors of Wellbeing During the Pandemic. Retrieved from osf.io/ap2rj
# Original Title: Regression information for public online.sav

library(tidyverse)
origdf <- haven::read_sav('mead2021-journals-data.sav')


## Simplify dataset ----
cleandf <- origdf |>
  mutate(
    Gender =
      case_match(
        Gender,
        1 ~ 'Male',
        2 ~ 'Female'
      ),
    Activity =
      case_when(
        PA_Low == 1 ~ 'Low',
        PA_Moderate == 1 ~ 'Moderate',
        PA_High == 1 ~ 'High'
      ),
    Status =
      case_when(
        SSS_Low == 1 ~ 'Low',
        SSS_Middle == 1 ~ 'Middle',
        SSS_High == 1 ~ 'High'
      )
  ) |>
  dplyr::select(
    Age,
    Gender,
    Activity,
    Status,
    Wellbeing = WEMWBS_TOTAL,
    Gratitude = GQ_6_Total,
    Optimism = LAM_Total,
    Support = MSPSS_Total,
    Nature = NC_1_3_4
  )


## Reduce dataset ----
gratitude <- cleandf
usethis::use_data(gratitude, overwrite = TRUE)
