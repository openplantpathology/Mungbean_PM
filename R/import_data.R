
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
source("R/OD_DL_csv.R") # download from oneDrive shared link function.

import_data <- function() {
   x <-
      OD_DL_csv(sharedURL = "https://usqprd-my.sharepoint.com/:x:/g/personal/u8011054_usq_edu_au/ER13jFzyQqNMq_SEZkNW9NoBa1ynkFl48tXlmd0i_ZHr5w?e=FRY4uo",
                file_name = "mungbean.csv"
               )
   x$trial_ref <- as.factor(x$trial_ref)
   x$location <- as.factor(x$location)
   x$host_genotype <- as.factor(x$host_genotype)
   x$fungicide_ai <- as.factor(x$fungicide_ai)
   x$trade_name <- as.factor(x$trade_name)
   x$year <- as.factor(x$year)
   x$replicates <- as.numeric(x$replicates)
   x$planting_date <- as.Date(x$planting_date, format = "%d/%m/%Y")
   x$emergence_date <- as.Date(x$emergence_date, format = "%d/%m/%Y")
   x$flowering_date <- as.Date(x$flowering_date, format = "%d/%m/%Y")
   x$pod_fill_date <- as.Date(x$pod_fill_date, format = "%d/%m/%Y")
   x$mid_late_pod_fill <- as.Date(x$mid_late_pod_fill, format = "%d/%m/%Y")
   x$first_sign_disease <- as.Date(x$first_sign_disease, format = "%d/%m/%Y")
   x$fungicide_application_1 <- as.Date(x$fungicide_application_1, format = "%d/%m/%Y")
   x$fungicide_application_2 <- as.Date(x$fungicide_application_2, format = "%d/%m/%Y")
   x$fungicide_application_3 <- as.Date(x$fungicide_application_3, format = "%d/%m/%Y")
   x$fungicide_application_4 <- as.Date(x$fungicide_application_4, format = "%d/%m/%Y")
   x$fungicide_application_5 <- as.Date(x$fungicide_application_5, format = "%d/%m/%Y")
   x$fungicide_application_6 <- as.Date(x$fungicide_application_6, format = "%d/%m/%Y")
   x$fungicide_application_7 <- as.Date(x$fungicide_application_7, format = "%d/%m/%Y")
   x$harvest_date <- as.Date(x$harvest_date, format = "%d/%m/%Y")
   x$final_assessment <- as.Date(x$final_assessment, format = "%d/%m/%Y")
   x$Y_error_type <- as.character(x$Y_error_type)
   return(x)
}
