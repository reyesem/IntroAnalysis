## Original Cellphones Dataset ----

# McElroy, T. (2023, March 9). Cell-phone possession and time. https://doi.org/10.17605/OSF.IO/SWXH7
# Original Title: Data for cell phone Affect TIme study.xlsx

library(tidyverse)
origdf <- readxl::read_xlsx('mcelroy2023-cellphones-data.xlsx')


## Simplify dataset ----
cleandf <- origdf |>
  filter(is.na(`...10`)) |>
  mutate(
    Condition =
      case_match(
        Condition,
        1 ~ 'On',
        2 ~ 'Off',
        3 ~ 'Removed'
      )
  ) |>
  dplyr::select(
    Participant,
    Condition,
    PANAS1pos = `PANAS 1 +`,
    PANAS2pos = `PANAS 2 +`,
    PANAS1neg = `PANAS 1 -`,
    PANAS2neg = `PANAS 2 -`,
    Duration = `Minutes Estimation`
  )


## Reduce dataset ----
cellphones <- cleandf
usethis::use_data(cellphones, overwrite = TRUE)
