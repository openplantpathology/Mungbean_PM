# This script takes the large dataset and slims it down
# The slimed down dataset will be used for the Meta-analysis

source("R/import_data.R")
# PM_MB_means <- import_data()
PM_MB_means <- read.csv("data/1911_PM_MB_means&Ygains.csv")

slim_PM_dat <- data.frame(
   trial = paste(PM_MB_means$trial_ref, PM_MB_means$year,
                 PM_MB_means$location,PM_MB_means$host_genotype,
                 PM_MB_means$row_spacing, sep = "_"),
   trial_ref = as.character(PM_MB_means$trial_ref),
   year = as.integer(PM_MB_means$year),
   location = as.character(PM_MB_means$location),
   row_spacing = as.double(PM_MB_means$row_spacing),
   n = as.integer(PM_MB_means$replicates),
   first_sign_disease = as.Date(PM_MB_means$first_sign_disease, format = "%Y-%m-%d"),
   fungicide_ai = as.character(PM_MB_means$fungicide_ai),
   fungicide_application_1 = as.Date(PM_MB_means$fungicide_application_1, format = "%Y-%m-%d"),
   fungicide_application_2 = as.Date(PM_MB_means$fungicide_application_2, format = "%Y-%m-%d"),
   fungicide_application_3 = as.Date(PM_MB_means$fungicide_application_3, format = "%Y-%m-%d"),
   total_fungicide = as.integer(PM_MB_means$total_fungicide),
   grain_yield.t.ha = as.double(PM_MB_means$grain_yield.t.ha.),
   yield_error = as.double(PM_MB_means$yield_error),
   yield_gain = as.double(PM_MB_means$yield_gain),
   prop_yield_gain = as.double(PM_MB_means$prop_YG),
   PM_final_severity = as.double(PM_MB_means$PM_final_severity),
   PM_final_severity_error = as.double(PM_MB_means$disease_error)
)

slim_PM_dat <- slim_PM_dat[slim_PM_dat$fungicide_ai == "control" |
                              slim_PM_dat$fungicide_ai == "tebuconazole" |
                              slim_PM_dat$fungicide_ai == "propiconazole" ,
                           ]


write.csv(slim_PM_dat, file = "data/slim_PM_dat.csv", row.names = FALSE)

