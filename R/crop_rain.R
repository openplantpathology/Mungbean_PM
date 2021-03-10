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
      trials$station_file <-
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
            
            if (stat_file %in% paste0("cache/weather/", list.files(here::here("cache/weather/")))) {
               #weather <- read.csv(stat_file, stringsAsFactors = FALSE)
               return(stat_file)
            } else{
               weather <-
                  bomrang::get_historical(stationid = stat_sweep[1, "site"], type = "rain")
               write.csv(weather, file = stat_file, row.names = FALSE)
               return(stat_file)
            }
         })


      input_data <- full_join(input_data,trials, by = c("location_name", "latitude", "longitude"))
      
      input_data$start <- first_day
      input_data$end <- last_day

      
      # calculate the sum rainfall for the specified time interval
      input_data$sum_rain <-
         apply(input_data, 1, function(x1) {
            weather <-
               read.csv(file = x1["station_file"], stringsAsFactors = FALSE)
            
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