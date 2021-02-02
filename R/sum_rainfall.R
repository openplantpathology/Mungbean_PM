# Import rainfall data for each trial

library("dplyr")
library("lubridate")

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
         location == "Wellcamp " ~
            experiment_sites[experiment_sites$location == "Wellcamp", "lat"]
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
         location == "Wellcamp " ~
            experiment_sites[experiment_sites$location == "Wellcamp", "lon"]
      )
   )

# Function to run over each line for sum rainfall
crop_rain <- function(latitude, longitude, start, end) {
   # locations <- unique(data.frame(lat = latitude,
   #                                lon = longitude))
   
   LL <- c(latitude, longitude)
   
   
   #cat(LL, length(LL), class(LL))
   
   
   
   if (file.exists(paste("weather/", paste(LL, collapse = ""), ".csv", sep = ""))) {
      station_site <-
         read.csv(paste("weather/", paste(LL, collapse = ""), ".csv", sep = ""),
                  stringsAsFactors = FALSE)
      #message("imported: ",paste(LL,collapse = " "))
      site_num <- unique(station_site$site_num)
      
   } else{
      stat_sweep <- sweep_for_stations(latlon = as.numeric(LL))
      
      site_num <- stat_sweep[1, site]
      banned_stations <- c("055286") # stations don't report rain
      
      if (banned_stations %in% site_num) {
         site_num <- stat_sweep[2, site]
      }
      
      station_site <-
         bomrang::get_historical(stationid = site_num, type = "rain")
      station_site$site_num <- as.character(site_num)
      
      write.csv(
         station_site,
         file = paste("weather/", paste(LL, collapse = ""), ".csv", sep = ""),
         row.names = FALSE
      )
   }
   
   
   season <- interval(start = ymd(start), end = ymd(end))
   
   sum_rain <-
      station_site %>%
      mutate(log_date = ymd(paste(year, month, day, sep = "-"))) %>%
      filter(log_date %within% season) %>%
      summarise(total_rain = sum(rainfall, na.rm = TRUE)) %>%
      pull()
   
   return(sum_rain)
   
}



PM_MB_dat$sum_rain <-
   apply(PM_MB_dat, 1, function(x) {
      crop_rain(
         latitude = x["lat"],
         longitude = x["lon"],
         start = x["planting_date"],
         end = x["harvest_date"]
      )
   })


   