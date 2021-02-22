# Function: reyes_setup
# Description: personal function for students in the course so that all
#              necessary functionality is incorporated.

ma223_setup <- function(){
  pkgs <- utils::installed.packages()
  dir <- pkgs[which(pkgs[, "Package"]=="IntroAnalysis"), "LibPath"]

  source(paste0(dir, "/IntroAnalysis/exec/ma223setup.R"))
}