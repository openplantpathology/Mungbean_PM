# Import rainfall data for each trial

library("dplyr")

# read in site location data, including lats and longs
experiment_sites <-
   read.csv("cache/Mungbean_experiment_sites.csv", stringsAsFactors = FALSE)

# read in cleaned dataset
PM_MB_dat <-
   read.csv("cache/PM_MB_clean_data.csv", stringsAsFactors = FALSE)

unique(PM_MB_dat$location)

# add lat and longs to PM_MB_dat data.frame
PM_MB_dat <-
   PM_MB_dat %>%
   mutate(
      lat = case_when(
         location == "Hermitage" ~
            experiment_sites[experiment_sites$location == "Hermitage Research Station", "lat"],
         location == "Kingaroy" ~
            experiment_sites[experiment_sites$location == "Red Vale & Kingaroy Research Station", "lat"],
         location == "Goolhi" ~
            experiment_sites[experiment_sites$location == "NGA - Goolhi", "lat"],
         location == "Marys Mount" ~
            experiment_sites[experiment_sites$location == "NGA - Marys Mount", "lat"],
         location == "Premer" ~
            experiment_sites[experiment_sites$location == "NGA - Premer", "lat"],
         location == "Millmerran" ~
            experiment_sites[experiment_sites$location == "NGA - Millmerran", "lat"],
         location == "Missen Flats" ~
            experiment_sites[experiment_sites$location == "Missen Flats", "lat"],
         location == "Wellcamp" ~
            experiment_sites[experiment_sites$location == "Wellcamp", "lat"],
         location == "Gatton" ~
            experiment_sites[experiment_sites$location == "Gatton", "lat"]
      ),
      lon = case_when(
         location == "Hermitage" ~
            experiment_sites[experiment_sites$location == "Hermitage Research Station", "lon"],
         location == "Kingaroy" ~
            experiment_sites[experiment_sites$location == "Red Vale & Kingaroy Research Station", "lon"],
         location == "Goolhi" ~
            experiment_sites[experiment_sites$location == "NGA - Goolhi", "lon"],
         location == "Marys Mount" ~
            experiment_sites[experiment_sites$location == "NGA - Marys Mount", "lon"],
         location == "Premer" ~
            experiment_sites[experiment_sites$location == "NGA - Premer", "lon"],
         location == "Millmerran" ~
            experiment_sites[experiment_sites$location == "NGA - Millmerran", "lon"],
         location == "Missen Flats" ~
            experiment_sites[experiment_sites$location == "Missen Flats", "lon"],
         location == "Wellcamp" ~
            experiment_sites[experiment_sites$location == "Wellcamp", "lon"],
         location == "Gatton" ~
            experiment_sites[experiment_sites$location == "Gatton", "lon"]
      )
   )




   