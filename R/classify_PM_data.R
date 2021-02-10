classify_PM_data <- function(dat) {
   dat <-
      dat %>%
      mutate(trial_ref = as.character(trial_ref)) %>%
      mutate(year = as.character(year)) %>%
      mutate(location = as.character(location)) %>%
      mutate(host_genotype = as.character(host_genotype)) %>%
      mutate(row_spacing = as.double(round(row_spacing, digits = 2))) %>%
      mutate(replicates = as.integer(replicates)) %>%
      mutate(planting_date = ymd(planting_date)) %>%
      mutate(flowering_date = ymd(flowering_date)) %>%
      mutate(pod_fill_date = ymd(pod_fill_date)) %>%
      mutate(mid_late_pod_fill = ymd(mid_late_pod_fill)) %>%
      mutate(first_sign_disease = ymd(first_sign_disease)) %>%
      mutate(fungicide_ai = as.character(fungicide_ai)) %>%
      mutate(dose_ai.ha = round(as.double(dose_ai.ha), digits = 2)) %>%
      mutate(fungicide_application_1 = ymd(fungicide_application_1)) %>%
      mutate(fungicide_application_2 = ymd(fungicide_application_2)) %>%
      mutate(fungicide_application_3 = ymd(fungicide_application_3)) %>%
      mutate(fungicide_application_4 = ymd(fungicide_application_4)) %>%
      mutate(fungicide_application_5 = ymd(fungicide_application_5)) %>%
      mutate(fungicide_application_6 = ymd(fungicide_application_6)) %>%
      mutate(total_fungicide = as.integer(total_fungicide)) %>%
      mutate(harvest_date = ymd(harvest_date)) %>%
      mutate(final_assessment = ymd(final_assessment)) %>%
      mutate(PM_final_severity = round(as.double(PM_final_severity), digits = 4)) %>%
      mutate(disease_error = round(as.double(disease_error), digits = 6)) %>%
      mutate(D_error_type = as.character(D_error_type)) %>%
      mutate(grain_yield.t.ha. = round(as.double(grain_yield.t.ha.), digits = 4)) %>%
      mutate(yield_error = round(as.double(yield_error), digits = 6)) %>%
      mutate(Y_error_type = as.character(Y_error_type)) %>%
      mutate(Y_Msquare = round(as.double(Y_Msquare), digits = 6)) %>%
      mutate(yield_gain = round(as.double(yield_gain), digits = 4)) %>%
      mutate(prop_YG = round(as.double(prop_YG), digits = 4)) %>%
      select(
         -c(
            trial_design,
            # remove the following columns
            plot_length.m.,
            plot_width.m.,
            plant_density,
            trade_name,
            n_treatment,
            emergence_date,
            rating_scale,
            raw_graded,
            raw_data_avail,
            comments,
            days_harvest.planting,
            AUDPS_m,
            AUDPC_sd,
            Inc_Ms,
            pod_fill_date,
            mid_late_pod_fill,
            flowering_date,
            yield_gain,
            Y_Msquare,
            prop_YG,
            AUDPC_m,
            AUDPC_sd,
            AUDPS_sd
         )
      )
   return(dat)
   }