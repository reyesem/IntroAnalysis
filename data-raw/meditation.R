## Original Meditation Dataset ----

# Schlosser, M., Jones, R., Demnitz-King, H., & Marchant, N. L. (2020, February 4). Meditation Experience is Associated with Lower Levels Repetitive Negative Thinking: The Key Role of Self-Compassion. Retrieved from osf.io/uq46g
# Original Title: MeditationRNT_data.xlsx

library(tidyverse)
origdf <- readxl::read_xlsx('schlosser2022-meditation-data.xlsx',
                            na = '.')


## Simplify dataset ----
cleandf <- origdf |>
  mutate(
    gender =
      case_match(
        sex,
        0 ~ 'Male',
        1 ~ 'Female'
      ),
    residence =
      case_match(
        residence,
        1 ~ 'Europe',
        2 ~ 'North America',
        3 ~ 'South America',
        4 ~ 'Asia',
        5 ~ 'Africa',
        6 ~ 'Australia / New Zealand'
      ),
    university =
      case_match(
        education,
        0 ~ 'No University Degree',
        1 ~ 'University Degree'
      ),
    religious =
      case_match(
        religious,
        0 ~ 'Not Religious',
        1 ~ 'Religious'
      )
  ) |>
  dplyr::select(
    respondentID = RespondentId_opinio,
    age,
    gender,
    residence,
    university,
    religious,
    experience = years_regular_practice,
    frequency = frequency_session,
    duration = duration_session_average,
    negthinking = ptq,
    compassion = scs,
    mindfulness = mindsens
  )


## Correct dataset ----

  meditation <- cleandf
usethis::use_data(meditation, overwrite = TRUE)
