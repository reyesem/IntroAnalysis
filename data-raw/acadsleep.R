## Original Sleep Dataset ----

# Bernard, K., & Daros, A. R. (2018, July 7). Relationships between daily sleep ratings, well-being, and academic success over the course of a college semester: A StudentLife study. https://doi.org/10.17605/osf.io/hpwvt
# Original Title: combinedCSV3_july5.csv

library(tidyverse)
origdf <- read_csv('bernard2018-sleep-data.csv')


## Simplify dataset ----
cleandf <- origdf |>
  mutate(
    gender =
      case_match(
        gender,
        'F' ~ 'Female',
        'M' ~ 'Male'
      )
  ) |>
  select(
    participantID,
    gender,
    PSS = pssPre,
    PSQI = psqiPre,
    PHQ9 = phq9Pre,
    FS = fsPre,
    cumulGPA,
    springGPA,
    sleepHours,
    sleepRate
  )


## Correct dataset ----
# PSQI values larger than 21 (only the value 28) should be removed.
# Coding for dataset given at https://studentlife.cs.dartmouth.edu/dataset.html#sec:ema
cleandf <- cleandf |>
  mutate(
    PSQI =
      case_when(
        PSQI <= 21 ~ PSQI
      ),
    sleepHours =
      seq(from = 2.5, to = 12, by = 0.5)[sleepHours + 1],
    sleepHours =
      case_when(
        sleepHours >= 3 ~ sleepHours
      ),
    sleepRate =
      case_match(
        sleepRate,
        1 ~ 'Very Good',
        2 ~ 'Fairly Good',
        3 ~ 'Fairly Bad',
        4 ~ 'Very Bad'
      )
  )


sleep <- cleandf
usethis::use_data(sleep, overwrite = TRUE)
