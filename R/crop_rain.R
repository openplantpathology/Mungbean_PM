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