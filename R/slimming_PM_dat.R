# This script takes the large dataset and slims it down
# The slimmed down dataset will be used for the Meta-analysis

#source("R/import_data.R")
# PM_MB_Mtemp <- import_data()
PM_MB_Mtemp <- PM_MB_means

PM_MB_Mtemp$dose <- NA
PM_MB_Mtemp[PM_MB_Mtemp$dose_ai.ha <= 70, "dose"] <- "lowDose"
PM_MB_Mtemp[PM_MB_Mtemp$dose_ai.ha > 70, "dose"] <- "highDose"

slim_PM_dat <- data.frame(
   trial = paste(PM_MB_Mtemp$trial_ref, PM_MB_Mtemp$year,
                 PM_MB_Mtemp$location,PM_MB_Mtemp$host_genotype,
                 PM_MB_Mtemp$row_spacing, PM_MB_Mtemp$dose,sep = "_"),
   trial_ref = as.factor(PM_MB_Mtemp$trial_ref),
   year = as.integer(PM_MB_Mtemp$year),
   host_genotype = PM_MB_Mtemp$host_genotype,
   location = as.character(PM_MB_Mtemp$location),
   row_spacing = as.double(PM_MB_Mtemp$row_spacing),
   n = as.integer(PM_MB_Mtemp$replicates),
   first_sign_disease = as.Date(PM_MB_Mtemp$first_sign_disease, format = "%Y-%m-%d"),
   fungicide_ai = as.character(PM_MB_Mtemp$fungicide_ai),
   fungicide_application_1 = as.Date(PM_MB_Mtemp$fungicide_application_1, format = "%Y-%m-%d"),
   fungicide_application_2 = as.Date(PM_MB_Mtemp$fungicide_application_2, format = "%Y-%m-%d"),
   fungicide_application_3 = as.Date(PM_MB_Mtemp$fungicide_application_3, format = "%Y-%m-%d"),
   total_fungicide = as.integer(PM_MB_Mtemp$total_fungicide),
   dose_ai.ha = as.double(PM_MB_Mtemp$dose_ai.ha),
   grain_yield.t.ha = as.double(PM_MB_Mtemp$grain_yield.t.ha.),
   yield_error = as.double(PM_MB_Mtemp$yield_error),
   yield_gain = as.double(PM_MB_Mtemp$yield_gain),
   prop_yield_gain = as.double(PM_MB_Mtemp$prop_YG),
   PM_final_severity = as.double(PM_MB_Mtemp$PM_final_severity),
   PM_final_severity_error = as.double(PM_MB_Mtemp$disease_error),
   Y_Msquare = as.double(PM_MB_Mtemp$Y_Msquare),
   Inc_Ms = as.double(PM_MB_Mtemp$Inc_Ms),
   AUDPC_m = as.double(PM_MB_Mtemp$AUDPC_m),
   AUDPC_sd = as.double(PM_MB_Mtemp$AUDPC_sd),
   AUDPS_m = as.double(PM_MB_Mtemp$AUDPS_m),
   AUDPS_sd = as.double(PM_MB_Mtemp$AUDPS_sd),
   dose = factor(PM_MB_Mtemp$dose)
   
)

# This loop only deletes Trials which did not test either
#  Propiconazole or Tebuconazole
for(i in levels(slim_PM_dat$trial_ref)){
   if(i == levels(slim_PM_dat$trial_ref)[1]){
      rows_to_delete <- vector()
   }
   
   if(any(slim_PM_dat[slim_PM_dat$trial_ref == i,"fungicide_ai"] == "tebuconazole" |
          slim_PM_dat[slim_PM_dat$trial_ref == i,"fungicide_ai"] == "propiconazole")){
      next()
   }else
      rows_to_delete <- c(rows_to_delete, which(slim_PM_dat$trial_ref == i))
}

slim_PM_dat <- slim_PM_dat[-rows_to_delete,]


slim_PM_dat <- slim_PM_dat[slim_PM_dat$fungicide_ai == "control" |
                              slim_PM_dat$fungicide_ai == "tebuconazole" |
                              slim_PM_dat$fungicide_ai == "propiconazole" ,
                           ]



write.csv(slim_PM_dat, file = "cache/slim_PM_dat.csv", row.names = FALSE)
