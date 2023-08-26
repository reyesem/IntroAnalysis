## Original RoleModel Dataset ----

# O’Brien, L., & Van Camp, A. (2021, September 24). Testing the effects of a role model intervention on women’s STEM outcomes. Retrieved from osf.io/5a7bm
# Original Title: College Role Model Study.sav

library(tidyverse)
origdf <- haven::read_sav('vancamp2019-mentor-data.sav')


## Simplify dataset ----
cleandf <- origdf |>
  mutate(
    condition = haven::as_factor(condition),
    race = haven::as_factor(s1_race)
  ) |>
  dplyr::select(
    participant = Participant,
    condition,
    race,
    stemintent1 = s1stemintentions_new,
    stemintent2 = ws4stemintentions_new,
    gpa = GPA,
    stemgpa = STEM_GPA,
    othergpa = NONSTEM_GPA,
    gnat1 = S1dwomen_science,
    gnat2 = S4dwomen_science,
    stembelong1 = s1stemsob,
    stembelong2 = s4stemsob,
    stemid1 = s1stemid,
    stemid2 = s4stemid,
    explicit1 = s1stendorse,
    explicit2 = s4stendorse
  )


## Reduce dataset ----
rolemodel <- cleandf
usethis::use_data(rolemodel, overwrite = TRUE)
