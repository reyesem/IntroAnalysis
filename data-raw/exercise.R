## Original Exercise Dataset ----

# Kroencke, L., Harari, G. M., & Gosling, S. D. (2019, May 27). Exercise Patterns and Mental Well-Being Among College Students: A Longitudinal Experience Sampling Study. https://doi.org/10.17605/OSF.IO/SWFEG
# Original Title: Data_Spring2016Fall2016Spring2017.csv

library(tidyverse)
origdf <- read_csv('kroencke2018-exercise-data.csv')


## Simplify dataset ----
# Reduce to only first two weeks.
# The BFI totals were not coded as totals, so this is corrected.
# Only keep undergraduate students.
revscore <- function(u) {
  rev(seq(5))[u]
}

cleandf <- origdf |>
  mutate(
    exercise =
      exercise_wk1 + exercise_wk2,
    gender =
      case_match(
        sex,
        0 ~ 'Male',
        1 ~ 'Female'
      ),
    class =
      case_match(
        academicclass,
        1 ~ 'Freshman',
        2 ~ 'Sophomore',
        3 ~ 'Junior',
        4 ~ 'Senior'
      ),
    perceivedSES =
      case_match(
        perceivedSES,
        1 ~ 'Lower',
        2 ~ 'Lower-Middle',
        3 ~ 'Middle',
        4 ~ 'Upper-Middle',
        5 ~ 'Upper'
      ),
    extraversion =
      bfi_1 + revscore(bfi_6) + bfi_11 + bfi_16 + revscore(bfi_21) + bfi_26 + revscore(bfi_31) + bfi_36,
    agreeableness =
      revscore(bfi_2) + bfi_7 + revscore(bfi_12) + bfi_17 + bfi_22 + revscore(bfi_27) + bfi_32 + revscore(bfi_37) + bfi_42,
    conscientiousness =
      bfi_3 + revscore(bfi_8) + bfi_13 + revscore(bfi_18) + revscore(bfi_23) + bfi_28 + bfi_33 + bfi_38 + revscore(bfi_43),
    neuroticism =
      bfi_4 + revscore(bfi_9) + bfi_14 + bfi_19 + revscore(bfi_24) + bfi_29 + revscore(bfi_34) + bfi_39,
    openness =
      bfi_5 + bfi_10 + bfi_15 + bfi_20 + bfi_25 + bfi_30 + revscore(bfi_35) + bfi_40 + revscore(bfi_41) + bfi_44
  ) |>
  select(
    exercise,
    gender,
    age,
    class,
    perceivedSES,
    extraversion,
    agreeableness,
    conscientiousness,
    neuroticism,
    openness
  ) |>
  filter(!is.na(class))


## Correct dataset ----


exercise <- cleandf
usethis::use_data(exercise, overwrite = TRUE)
