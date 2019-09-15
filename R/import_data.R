
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
import_data <- function() {
   x <-
      read_csv("data/1902 powdery mildew-Mungbean - Collated means.csv")
   x <- x %>%
      mutate(trial_ref = as.factor(trial_ref)) %>%
      mutate(location = as.factor(location)) %>%
      mutate(host_genotype = as.factor(host_genotype)) %>%
      mutate(fungicide_ai = as.factor(fungicide_ai)) %>%
      mutate(trade_name = as.factor(trade_name)) %>%
      mutate(year = as.factor(year)) %>%
      mutate(replicates = as.factor(replicates)) %>%
      mutate(planting_date = as.Date(planting_date, format = "%d/%m/%Y")) %>%
      mutate(emergence_date = as.Date(emergence_date, format = "%d/%m/%Y")) %>%
      mutate(flowering_date = as.Date(flowering_date, format = "%d/%m/%Y")) %>%
      mutate(pod_final_date = as.Date(pod_final_date, format = "%d/%m/%Y")) %>%
      mutate(mid_late_pod_final = as.Date(mid_late_pod_final, format = "%d/%m/%Y")) %>%
      mutate(first_sign_disease = as.Date(first_sign_disease, format = "%d/%m/%Y")) %>%
      mutate(fungicide_application_1 = as.Date(fungicide_application_1, format = "%d/%m/%Y")) %>%
      mutate(fungicide_application_2 = as.Date(fungicide_application_2, format = "%d/%m/%Y")) %>%
      mutate(fungicide_application_3 = as.Date(fungicide_application_3, format = "%d/%m/%Y")) %>%
      mutate(fungicide_application_4 = as.Date(fungicide_application_4, format = "%d/%m/%Y")) %>%
      mutate(fungicide_application_5 = as.Date(fungicide_application_5, format = "%d/%m/%Y")) %>%
      mutate(fungicide_application_6 = as.Date(fungicide_application_6, format = "%d/%m/%Y")) %>%
      mutate(fungicide_application_7 = as.Date(fungicide_application_7, format = "%d/%m/%Y")) %>%
      mutate(harvest_date = as.Date(harvest_date, format = "%d/%m/%Y")) %>%
      mutate(final_assessment = as.Date(final_assessment, format = "%d/%m/%Y"))
}
