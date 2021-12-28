classify_PM_data <- function(dat) {
   dat <-
      dat %>%
      dplyr::mutate(trial_ref = as.character(trial_ref)) %>%
      dplyr::mutate(year = as.character(year)) %>%
      dplyr::mutate(location = as.character(location)) %>%
      dplyr::mutate(host_genotype = as.character(host_genotype)) %>%
      dplyr::mutate(row_spacing = as.double(round(row_spacing, digits = 2))) %>%
      dplyr::mutate(replicates = as.integer(replicates)) %>%
      dplyr::mutate(planting_date = ymd(planting_date)) %>%
      dplyr::mutate(flowering_date = ymd(flowering_date)) %>%
      dplyr::mutate(pod_fill_date = ymd(pod_fill_date)) %>%
      dplyr::mutate(mid_late_pod_fill = ymd(mid_late_pod_fill)) %>%
      dplyr::mutate(first_sign_disease = ymd(first_sign_disease)) %>%
      dplyr::mutate(fungicide_ai = as.character(fungicide_ai)) %>%
      dplyr::mutate(dose_ai.ha = round(as.double(dose_ai.ha), digits = 2)) %>%
      dplyr::mutate(fungicide_application_1 = ymd(fungicide_application_1)) %>%
      dplyr::mutate(fungicide_application_2 = ymd(fungicide_application_2)) %>%
      dplyr::mutate(fungicide_application_3 = ymd(fungicide_application_3)) %>%
      dplyr::mutate(fungicide_application_4 = ymd(fungicide_application_4)) %>%
      dplyr::mutate(fungicide_application_5 = ymd(fungicide_application_5)) %>%
      dplyr::mutate(fungicide_application_6 = ymd(fungicide_application_6)) %>%
      dplyr::mutate(total_fungicide = as.integer(total_fungicide)) %>%
      dplyr::mutate(harvest_date = ymd(harvest_date)) %>%
      dplyr::mutate(final_assessment = ymd(final_assessment)) %>%
      dplyr::mutate(PM_final_severity = round(as.double(PM_final_severity), digits = 4)) %>%
      dplyr::mutate(disease_error = round(as.double(disease_error), digits = 6)) %>%
      dplyr::mutate(D_error_type = as.character(D_error_type)) %>%
      dplyr::mutate(grain_yield.t.ha. = round(as.double(grain_yield.t.ha.), digits = 4)) %>%
      dplyr::mutate(yield_error = round(as.double(yield_error), digits = 6)) %>%
      dplyr::mutate(Y_error_type = as.character(Y_error_type)) %>%
      dplyr::mutate(Y_Msquare = round(as.double(Y_Msquare), digits = 6)) %>%
      dplyr::mutate(yield_gain = round(as.double(yield_gain), digits = 4)) %>%
      dplyr::mutate(prop_YG = round(as.double(prop_YG), digits = 4))
   
   drops <- c(
      "trial_design",
      "plot_length.m.",
      "plot_width.m.",
      "plant_density",
      "trade_name",
      "n_treatment",
      "emergence_date",
      "rating_scale",
      "raw_graded",
      "raw_data_avail",
      "comments",
      "days_harvest.planting",
      "AUDPS_m",
      "AUDPC_sd",
      "Inc_Ms",
      "pod_fill_date",
      "mid_late_pod_fill",
      "flowering_date",
      "yield_gain",
      "Y_Msquare",
      "prop_YG",
      "AUDPC_m",
      "AUDPC_sd",
      "AUDPS_sd"
   )
   dat <- dat[ , !(names(dat) %in% drops)]

   return(dat)
}
