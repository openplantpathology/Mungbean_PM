
#' Import mungbean powdery mildew data
#'
#' Imports mungbean powdery mildew data and formats columns correctly.
#'
#' @return a Tibble of mungbean powdery mildew trial data
#'
#' @examples
#'
#' PM_MB_means <- import_data()
#'

if (!require("pacman")) install.packages("pacman")
pacman::p_load(here)

import_ManData <- function() {
   x <- read.csv(file = here::here("cache/PM_MB_updated.csv"), stringsAsFactors = FALSE)
   y <- read.csv(file = here::here("cache/PM_Mungbean_SummaryOfTrialsWithRawData"), stringsAsFactors = FALSE)
   x <- x[!(x$trial_ref %in% unique(y$trial_ref)),] # note ! negate, therefore %notin%
   return(x)
}
