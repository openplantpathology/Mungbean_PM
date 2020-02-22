
#' Import mungbean powdery mildew data
#'
#' Imports mungbean powdery mildew data and formats columns correctly.
#'
#' @return a `data.frame` of mungbean powdery mildew trial data
#'
#' @examples
#'
#' PM_MB_means <- import_data()
#'

import_data <- function() {
   if (getwd() == "C:/Users/U8011054/OneDrive - USQ/Cloudstor/R/Mungbean_projects/Mungbean_PM/DataWrangle") {
      x <- read.csv(file = "../data/PM_MB_updated.csv")
   } else{
      x <- read.csv(file = "data/PM_MB_updated.csv")
   }
   x$trial_ref <- as.factor(x$trial_ref)
   x$location <- as.factor(x$location)
   x$host_genotype <- as.factor(x$host_genotype)
   x$fungicide_ai <- as.factor(x$fungicide_ai)
   x$trade_name <- as.factor(x$trade_name)
   x$year <- as.factor(x$year)
   x$replicates <- as.numeric(x$replicates)
   x$planting_date <- as.Date(x$planting_date, format = "%Y-%m-%d")
   x$emergence_date <-
      as.Date(x$emergence_date, format = "%Y-%m-%d")
   x$flowering_date <-
      as.Date(x$flowering_date, format = "%Y-%m-%d")
   x$pod_fill_date <- as.Date(x$pod_fill_date, format = "%Y-%m-%d")
   x$mid_late_pod_fill <-
      as.Date(x$mid_late_pod_fill, format = "%Y-%m-%d")
   x$first_sign_disease <-
      as.Date(x$first_sign_disease, format = "%Y-%m-%d")
   x$fungicide_application_1 <-
      as.Date(x$fungicide_application_1, format = "%Y-%m-%d")
   x$fungicide_application_2 <-
      as.Date(x$fungicide_application_2, format = "%Y-%m-%d")
   x$fungicide_application_3 <-
      as.Date(x$fungicide_application_3, format = "%Y-%m-%d")
   x$fungicide_application_4 <-
      as.Date(x$fungicide_application_4, format = "%Y-%m-%d")
   x$fungicide_application_5 <-
      as.Date(x$fungicide_application_5, format = "%Y-%m-%d")
   x$fungicide_application_6 <-
      as.Date(x$fungicide_application_6, format = "%Y-%m-%d")
   x$fungicide_application_7 <-
      as.Date(x$fungicide_application_7, format = "%Y-%m-%d")
   x$harvest_date <- as.Date(x$harvest_date, format = "%Y-%m-%d")
   x$final_assessment <-
      as.Date(x$final_assessment, format = "%Y-%m-%d")
   x$Y_error_type <- as.character(x$Y_error_type)
   
   if (!c("AUDPS_m") %in% colnames(x)) {
      x$AUDPS_m <- NA
   }
   if (!c("AUDPS_sd") %in% colnames(x)) {
      x$AUDPS_sd <- NA
   }
   if (!c("Inc_Ms") %in% colnames(x)) {
      x$Inc_Ms <- NA
   }
   
   x <- x[, !(colnames(x) == "X")]
   
   return(x)
}
