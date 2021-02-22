#' SENIC dataset from Kutner (2005)
#'
#' A dataset containing characteristics on hospitals participating in the SENIC
#' project.
#'
#' @format A data frame with 113 rows and 12 variables:
#' \describe{
#'   \item{ID}{Unique hospital identifer.}
#'   \item{Length of Stay}{Average length of stay of all patients in hospital (days).}
#'   \item{Age}{Average age of patients (in years).}
#'   \item{Infection Risk}{estimated probability of acquiring infection in hospital (percent).}
#'   \item{Routine Culturing Ratio}{Ratio of number of cultures performed to the number of patients without signs or symptoms of hospital-acquired infection, times 100.}
#'   \item{Routine Xray Ratio}{Ratio of number of X-rays performed to number of patients without signs or symptoms of pneumonia, times 100.}
#'   \item{Number of Beds}{number of beds in hospital during study period.}
#'   \item{Medical School}{Indicator of whether the hospital is associated with a medical school (1 = Yes, 2 = No).}
#'   \item{Region}{Indicator of the geographic region for hospital (1 = NE, 2 = NC, 3 = S, 4 = W).}
#'   \item{Average Census}{number of patients per day in hospital during study period.}
#'   \item{Number Nurses}{number of full-time equivalent registered and licensed practical nurses during study period (number of full time plus one half the number of part time).}
#'   \item{Available Facilities}{Percent of 35 potential facilities and services that are provided by the hospital.}
#' }
#'
#' @source \url{http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix C Data Sets/APPENC01.txt}
"senic"



#' Real Estate dataset from Kutner (2005)
#'
#' A dataset containing characteristics on home sales.
#'
#' @format A data frame with 522 rows and 13 variables:
#' \describe{
#'   \item{ID}{Unique identification for each sale.}
#'   \item{Price}{Sale price of home ($).}
#'   \item{Area}{Finished area of residence (square feet).}
#'   \item{BedRooms}{Number of bedrooms in the residence.}
#'   \item{BathRooms}{Number of bathrooms in the residence.}
#'   \item{AC}{Indicator of whether home has central air conditioning (1 = Yes, 0 = No).}
#'   \item{Garage}{Number of cars that garage will hold.}
#'   \item{Pool}{Indicator of whether home has a swimming pool (1 = Yes, 0 = No).}
#'   \item{Year}{Year property was originally constructed.}
#'   \item{Quality}{Indicator of the quality of the construction (1 = High quality, 2 = Medium, 3 = Low).}
#'   \item{Style}{Indicator of the archiectural style (mapping unknown).}
#'   \item{Lot}{Lot size (square feet).}
#'   \item{Highway}{Indicator of whether home is located near highway (1 = Yes, 0 = No).}
#' }
#'
#' @source \url{http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Appendix C Data Sets/APPENC07.txt}
"RealEstate"



#' Rehabilitation therapy dataset from Kutner (2005)
#'
#' A dataset containing the recovery times of patients undergoing rehabilitation
#' therapy.
#'
#' @format A data frame with 24 rows and 4 variables:
#' \describe{
#'   \item{Subject}{unique patient identifer.}
#'   \item{Prior Activity Level}{indicator of the activity level of the subject prior to surgery.}
#'   \item{Recovery Time}{time (days) required to complete physical therapy successfully.}
#'   \item{Age}{age (years, rounded to nearest tenth) of patient.}
#' }
#'
#' @source \url{http://users.stat.ufl.edu/~rrandles/sta4210/Rclassnotes/data/textdatasets/KutnerData/Chapter 22 Data Sets/CH22PR11.txt}
"Therapy"



#' Example 4.4 from Montgomery (2008)
#'
#' A dataset containing the bacteria count in several milk containers.
#'
#' @format A data frame with 12 rows and 3 variables:
#' \describe{
#'   \item{Day}{indicator of the day on which the corresponding response was collected.}
#'   \item{Solution}{indicator of the washing solution utilized.}
#'   \item{CFU}{the number of colony-forming units (CFU's) of bacteria.}
#' }
#'
#' @source Montgomery \emph{Design and Analysis of Experiments} (2008); Exercise 4.4
"MilkBacteria"