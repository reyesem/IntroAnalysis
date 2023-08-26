## Original Notes Dataset ----

# Mueller, P. A. (2018, March 7). Laptop and Longhand Note-Taking. Retrieved from osf.io/crsiz
# Original Title: Study_1_Upload_Data.sav

library(tidyverse)
origdf <- haven::read_sav('mueller2014-notes-data.sav')


## Simplify dataset ----
cleandf <- origdf |>
  mutate(
    condition = haven::as_factor(condition),
    preference = haven::as_factor(laptopornotebook),
    class = haven::as_factor(year),
    talk =
      case_match(
        whichtalk,
        1 ~ 'Islam',
        2 ~ 'Inequality',
        3 ~ 'Ideas',
        4 ~ 'Indus',
        5 ~ 'Algorithms'
      )
  ) |>
  dplyr::select(
    wordcount,
    condition,
    preference,
    class,
    talk,
    GPA,
    SATreading,
    SATmath,
    SATwriting,
    SATcombined,
    perception = betterlorn,
    objective = rawobjective,
    conceptual = rawopen
  )


## Reduce dataset ----
notes <- cleandf
usethis::use_data(notes, overwrite = TRUE)
