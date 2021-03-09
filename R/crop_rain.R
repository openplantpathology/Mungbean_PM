library("dplyr")
library("lubridate")
# Function to run over each line for sum rainfall
crop_rain <-
   function(location_name,
            latitude,
            longitude,
            first_day,
            last_day) {
 
      input_data <- data.frame(location_name, latitude, longitude)
      trials <- unique(input_data)
      
      
      # Download trial data
      trials$stat_file <-
         apply(trials, 1, function(LL) {
            stat_sweep <-
               bomrang::sweep_for_stations(as.numeric(c(LL["latitude"], LL["longitude"])))
            
            # Remove banned stations
            banned_stations <-
               c("055286") # stations don't report rain
            stat_sweep <-
               stat_sweep[site != banned_stations, ]
            
            statID <-
               paste(stat_sweep[1, "name"], stat_sweep[1, "site"], sep = "_")
            stat_file <-
               paste("cache/weather/", statID, ".csv",sep = "", collapse = " ")
            
            if (file.exists(stat_file)) {
               #weather <- read.csv(stat_file, stringsAsFactors = FALSE)
               return(stat_file)
            } else{
               weather <-
                  bomrang::get_historical(stationid = stat_sweep[1, "site"], type = "rain")
               write.csv(weather, file = stat_file, row.names = FALSE)
               return(stat_file)
            }
         })


      
      # add a column with the weather station file name 
      for (i in 1:nrow(trials)) {
         input_data <- 
            input_data%>%
            mutate(stat_file = case_when(location_name == trials[i, "location_name"] &
                                            latitude == trials[i, "latitude"] &                     
                                            longitude == trials[i, "longitude"] ~ 
                                            trials[i, "stat_file"],
                                         TRUE ~ stat_file
                                            ),
                   start = first_day,
                   end = last_day)
      }
      
      
      # calculate the sum rainfall for the specified time interval
      input_data$sum_rain <-
         apply(input_data, 1, function(x1) {
            weather <-
               read.csv(file = x1["stat_file"], stringsAsFactors = FALSE)
            
            int1 <- interval(start = ymd(x1["start"]), end = ymd(x1["end"]))
      
            weather %>%
               mutate(log_date = ymd(paste(year, month, day, sep = "-"))) %>%
               filter(log_date %within% int1) %>%
               summarise(total_rain = sum(rainfall, na.rm = TRUE)) %>%
               pull()
         })

      # ensure NA is returned not 0 when start or end times are NA
      input_data <-
         input_data %>%
         mutate(sum_rain = case_when(is.na(start) |
                             is.na(end) ~ NA_real_,
                TRUE ~ sum_rain))

      
      return(input_data$sum_rain)
   }