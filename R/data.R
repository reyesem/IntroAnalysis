#' Sexual Consent Dataset
#'
#' A subset of the data from MacDougall (2022) related to the sexual consent behavior of undergraduate students.
#'
#' This dataset only includes the records for which the number of sexual partners exceeds 0. For the data we obtained from OSF, there were 32 participants who had 0 sexual partners, which exceeds the number reported in the original manuscript, hence some results are slightly different.
#'
#' @format A data frame with 164 rows and 11 variables:
#' \describe{
#'   \item{SubjID}{unique subject identifier.}
#'   \item{Gender}{indicator of participant's self-identified gender. While several categories were initially offered on the survey, the only options selected were male/female.}
#'   \item{Age}{age of participant, years.}
#'   \item{Ethnicity}{indicator of participant's self-identified race/ethnicity.}
#'   \item{SexualOrientation}{indicator of participant's self-identified sexual orientation.}
#'   \item{RelationshipStatus}{indicator of participant's current relationship status.}
#'   \item{SexPartners}{number of previous sexual partners.}
#'   \item{ATSS}{total sexual attitudes score, with higher values indicating more liberal views on sexuality. Scores can range from 13 to 65.}
#'   \item{SexEd}{indicator of the coverage of "consent" in participant's high school sexual education.}
#'   \item{IndirectScore}{average of responses to 6-item Indirect Behavioral Approach to Consent subscale of the SCS-R. Values can range from 1 to 7, with higher values indicating appropriate consent practices.}
#'   \item{Consent}{average of the percent of time the participant has given and received verbal/nonverbal consent in past sexual encounters.}
#' }
#'
#' @source <https://osf.io/vz2qh>
"consent"



#' Sleep and Academic Success Dataset
#'
#' Data from a study examining the relationships between daily sleep ratings, well-being, and academic success over the course of a college semester.
#'
#' There were several records where the PSQI was noted as 28; since the maximum value for the PSQI is 21, these values were set to missing in this dataset. There were a few individuals who recorded "0" for amount of sleep, but this was not an option in the survey; so, these were set to missing in this dataset.
#'
#' @format A data frame with 1338 rows and 10 variables:
#' \describe{
#'   \item{participantID}{unique participant identification.}
#'   \item{gender}{indicator of participant's self-identified gender.}
#'   \item{PSS}{score on the Perceived Stress Scale (0-40). Higher values indicate higher stress levels.}
#'   \item{PSQI}{score on the Pittsburgh Sleep Quality Index (0-21). Higher values indicate worse sleep quality.}
#'   \item{PHQ9}{score on the Patient Health Questionnaire (0-27) for assessing depression. Higher values indicate more severe depression.}
#'   \item{FS}{score on the Flourishing Scale (8-56) assessing psychological well-being. Higher values indicate healthier well-being.}
#'   \item{cumulGPA}{participant's cumulative GPA on a 4-point scale.}
#'   \item{springGPA}{participant's GPA (4-point scale) during only the semester for which the study was conducted.}
#'   \item{sleepHours}{the number of hours participant slept the previous night.}
#'   \item{sleepRate}{indicator rating of the sleep quality for the previous night.}
#' }
#'
#' @source <https://osf.io/nrd4g>
"acadsleep"



#' Student Exercise Dataset
#'
#' Subset of data from a study examining the exercise habits of college students over a semester.
#'
#' We only consider undergraduate students. The original dataset did not code the BFI domains as totals across the questions, but we have done that here to be consistent with scoring guides.
#'
#' @format A data frame with 2397 rows and 10 variables:
#' \describe{
#'   \item{exercise}{total number of days student exercised during the first two weeks of class.}
#'   \item{gender}{self-identified gender of student.}
#'   \item{age}{age of student (years).}
#'   \item{class}{academic class of student.}
#'   \item{perceivedSES}{self-perceived socio-economic status.}
#'   \item{extraversion}{extraversion score from the Big Five Inventory (8-40). Larger values indicate tendency toward extraversion instead of intraversion.}
#'   \item{agreeableness}{agreeableness score from the Big Five Inventory (9-45). Larger values indicate tendency toward agreeableness instead of antagonism.}
#'   \item{conscientiousness}{conscientiousness score from the Big Five Inventory (9-45). Larger values indicate tendency toward direction in life instead of lack of direction.}
#'   \item{neuroticism}{neuroticism score from the Big Five Inventory (8-40). Larger values indicate a tendency toward neuroticism instead of emotional stability.}
#'   \item{openness}{openness score from the Big Five Inventory (10-50). Larger values indicate an openness instead of closedness towards new experiences.}
#' }
#'
#' @source <https://osf.io/37fgy>
"exercise"



#' Meditation Dataset
#'
#' Data from a study examining the impact of meditation on repeated negative thinking.
#'
#' @format A data frame with 1281 rows and 12 variables:
#' \describe{
#'   \item{respondentID}{unique participant identifier.}
#'   \item{age}{age of participant (years).}
#'   \item{gender}{self-identified gender.}
#'   \item{residence}{content of residence for participant.}
#'   \item{university}{indicator of whether participant has a university degree.}
#'   \item{religious}{indicator of whether participant considers themselves to be religious.}
#'   \item{experience}{years of experience participant has with meditation.}
#'   \item{frequency}{typical number of times per week participant practices meditation.}
#'   \item{duration}{typical length of a meditation session for the participant.}
#'   \item{negthinking}{score from Perseverative Thinking Questionnaire (integers, 0-60). Higher values indicate a higher frequency of repetitive negative thoughts.}
#'   \item{compassion}{score from Self-Compassion Scale (real number, 1-5). Higher values are indicative of higher levels of self-compassion.}
#'   \item{mindfulness}{score from Mindsens (real number, 1-5). Higher scores are indicative of higher levels of self-reported mindfulness.}
#' }
#'
#' @source <https://osf.io/gme94>
"meditation"



#' Drinking and Sweets Dataset
#'
#' Subset of data from a study examining the impact of sweet foods in delaying an alcohol craving among at-risk drinkers.
#'
#' This dataset combined the two "sweet" groups and the two "bland" groups from the original study and eliminated the control group.
#'
#' @format A data frame with 150 rows and 13 variables:
#' \describe{
#'   \item{ID}{unique participant identification.}
#'   \item{Gender}{patient self-identified gender.}
#'   \item{Race}{patient self-identified race.}
#'   \item{AUDIT}{score on the Alcohol Use Disorder Identification Test (0-40). Higher values indicate more harmful patterns of alcohol intake. Participants had to score at least an 8 to participate in the study, which indicates they were "at-risk drinkers."}
#'   \item{Height}{height (inches).}
#'   \item{Weight}{weight (lbs).}
#'   \item{BMI}{body mass index.}
#'   \item{Condition}{indicator of treatment group. This is the type of food they were given after receiving an alcohol cue.}
#'   \item{Craving_Base}{index of alcohol craving on a sliding visual scale (0-100) prior to receiving an alcohol cue. Higher values indicate stronger desire for alcohol.}
#'   \item{Craving_T1}{index of alcohol craving on a sliding visual scale (0-100) immediately after receiving an alcohol cue. Higher values indicate stronger desire for alcohol.}
#'   \item{Craving_T2}{index of alcohol craving on a sliding visual scale (0-100) after consuming food following the receipt of an alcohol cue. Higher values indicate stronger desire for alcohol.}
#'   \item{CravingDiff}{difference in \code{Craving_T2} and \code{Craving_T1}.}
#'   \item{AlcoholPref}{indicator of preferred alcoholic beverage.}
#' }
#'
#' @source <https://osf.io/4qnvh>
"drinking"



#' Gratitude and Mental Health Dataset
#'
#' Data from a study examining the insights from the COVID-19 pandemic on strategies for protecting mental well-being.
#'
#'
#' @format A data frame with 123 rows and 9 variables:
#' \describe{
#'   \item{Age}{age of participant, in years.}
#'   \item{Gender}{self-identified gender of participant.}
#'   \item{Activity}{indicator of physical activity during lockdown protocols.}
#'   \item{Status}{indicator of socioeconomic status.}
#'   \item{Wellbeing}{score on Warwick-Edinburgh Mental Well-being Scale (14-70). Higher values indicate a higher level of wellbeing.}
#'   \item{Gratitude}{score on Gratitude Questionnaire-Six-Item Form (6-42). Higher values indicate higher level of gratitude.}
#'   \item{Optimism}{score on Life Acceptance Measure (9-45). Higher values indicate higher levels of optimism in the face of tragedy.}
#'   \item{Support}{score on the Multidimensional Scale of Perceived Social Support (12-84). Higher values indicate higher levels of perceived support.}
#'   \item{Nature}{score from a few questions asking about nature (3-15).  Higher scores indicate stronger connection with nature.}
#' }
#'
#' @source <https://osf.io/wf5th>
"gratitude"



#' Role Models for Women in STEM
#'
#' Data from a study examining the impact of women in STEM identifying with role models.
#'
#' Variable naming conventions in the original dataset were not clear. Summary statistics in the original manuscript were used to identify variables, but some output could not be replicated exactly.  In these cases, educated guesses were made based on the naming conventions.
#'
#' @format A data frame with 72 rows and 16 variables:
#' \describe{
#'   \item{participant}{unique participant id.}
#'   \item{condition}{indicator of treatment group. This represents the way in which participants engaged with mentors.}
#'   \item{race}{indicator of self-identified race.}
#'   \item{stemintent1}{score from a questionnaire measuring intentions to pursue a STEM career (real number, 1-7) prior to intervention. Higher values indicate stronger intent.}
#'   \item{stemintent2}{score from a questionnaire measuring intentions to pursue a STEM career (real number, 1-7) after intervention. Higher values indicate stronger intent.}
#'   \item{stembelong1}{score from a questionnaire measuring sense of belonging within STEM (real number, 1-7) prior to intervention. Higher values indicate stronger sense of belonging.}
#'   \item{stembelong2}{score from a questionnaire measuring sense of belonging within STEM (real number, 1-7) after intervention. Higher values indicate stronger sense of belonging.}
#'   \item{stemid1}{score from a questionnaire measuring self-identification with STEM (real number, 1-7) prior to intervention. Higher values indicate stronger sense of identification.}
#'   \item{stemid2}{score from a questionnaire measuring self-identification with STEM (real number, 1-7) after intervention. Higher values indicate stronger sense of identification.}
#'   \item{gnat1}{d' score from Go-No-Go Association Task assessing implicit stereotypes of women in STEM prior to intervention. Higher scores indicate higher association of women and STEM.}
#'   \item{gnat2}{d' score from Go-No-Go Association Task assessing implicit stereotypes of women in STEM after intervention. Higher scores indicate higher association of women and STEM.}
#'   \item{explicit1}{score from a questionnaire measuring agreement with STEM stereotypes (real number, 1-7) prior to intervention. Higher scores indicate stronger endorsement of stereotypes.}
#'   \item{explicit2}{score from a questionnaire measuring agreement with STEM stereotypes (real number, 1-7) after intervention. Higher scores indicate stronger endorsement of stereotypes.}
#'   \item{gpa}{GPA of participant at the end of the semester.}
#'   \item{stemgpa}{GPA of participant among STEM classes at end of the semester.}
#'   \item{othergpa}{GPA of participant among non-STEM classes at end of the semester.}
#' }
#'
#' @source <https://osf.io/j7umw>
"rolemodel"



#' Laptop vs. Longhand Note-taking
#'
#' Data from a study comparing note-taking longhand or with a laptop.
#'
#' @format A data frame with 65 rows and 13 variables:
#' \describe{
#'   \item{wordcount}{number of words in the notes taken by the participant.}
#'   \item{condition}{manner in which participant took notes during lecture.}
#'   \item{preference}{manner in which participant typically prefers taking notes.}
#'   \item{class}{indicator of academic class standing.}
#'   \item{talk}{indicator of subject of the lecture viewed.}
#'   \item{GPA}{cumulative GPA of participant.}
#'   \item{SATreading}{score on the reading portion of the SAT (perfect score is 800).}
#'   \item{SATmath}{score on the math portion of the SAT (perfect score is 800).}
#'   \item{SATwriting}{score on the writing portion of the SAT (perfect score is 800).}
#'   \item{SATcombined}{overall SAT score (perfect score is 2400).}
#'   \item{perception}{participant's perception of whether taking notes with a laptop or longhand is better for students. On a scale of 1-7, with 1 indicating laptop is significantly better and 7 indicating longhand is significantly better.}
#'   \item{objective}{score on questions assessing recall of facts from the lecture. Potential score varies by lecture topic.}
#'   \item{conceptual}{score on conceptual open-ended questions. Potential score varies by lecture topic.}
#' }
#'
#' @source <https://osf.io/qrs5y>
"notes"



#' Cellphone Addiction
#'
#' Data from a study examining the effect of being denied access to a cell phone.
#'
#' @format A data frame with 144 rows and 7 variables:
#' \describe{
#'   \item{Participant}{unique participant identification.}
#'   \item{Condition}{treatment condition assigned. "On" indicates access to cell phone, but asked to put it away; "Off" indicates access to cell phone, but asked to turn it off and put it away; "Removed" indicates cell phone was shut off and removed from participant's possession.}
#'   \item{PANAS1pos}{score on the Positive And Negative Affect Schedule indicating positive affect (10-50) prior to intervention. Higher scores indicate more positive emotions.}
#'   \item{PANAS2pos}{score on the Positive And Negative Affect Schedule indicating positive affect (10-50) following intervention. Higher scores indicate more positive emotions.}
#'   \item{PANAS1neg}{score on the Positive And Negative Affect Schedule indicating negative affect (10-50) prior to intervention. Higher scores indicate more negative emotions.}
#'   \item{PANAS2neg}{score on the Positive And Negative Affect Schedule indicating negative affect (10-50) following intervention. Higher scores indicate more negative emotions.}
#'   \item{Duration}{participant's perception of how long (minutes) they were left alone in a room following intervention (0-10). Actual time left alone was 3 minutes.}
#' }
#'
#' @source <https://osf.io/xfkh8>
"cellphones"