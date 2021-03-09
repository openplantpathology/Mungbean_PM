library("dplyr")
library("lubridate")
# Function to run over each line for sum rainfall
crop_rain <-
   function(location_name,
            latitude,
            longitude,
            start,
            end) {
      # # locations <- unique(data.frame(lat = latitude,
      # #                                lon = longitude))
      #
      # LL <- format(round(as.numeric(c(latitude, longitude)),4), nsmall = 4)
      #
      #    if (file.exists(paste("weather/", str_remove(paste(LL, collapse = ""),"-"), ".csv", sep = ""))) {
      #    station_site <-
      #       read.csv(paste("weather/", str_remove(paste(LL, collapse = ""),"-"), ".csv", sep = ""),
      #                stringsAsFactors = FALSE)
      #    #message("imported: ",paste(LL,collapse = " "))
      #    site_num <- unique(station_site$site_num)
      #
      # } else{
      #    stat_sweep <- sweep_for_stations(latlon = as.numeric(LL))
      #
      #    site_num <- stat_sweep[1, site]
      #    banned_stations <- c("055286") # stations don't report rain
      #
      #    if (banned_stations %in% site_num) {
      #       site_num <- stat_sweep[2, site]
      #    }
      #    message("Downloading weather data from BOM servers,\nplease make a cup of coffee this will take some time.")
      #    station_site <-
      #       bomrang::get_historical(stationid = site_num, type = "rain")
      #    station_site$site_num <- as.character(site_num)
      #
      #    write.csv(
      #       station_site,
      #       file = paste("weather/", str_remove(paste(LL, collapse = ""),"-"), ".csv", sep = ""),
      #       row.names = FALSE
      #    )
      # }
      #
      #
      # season <- interval(start = ymd(start), end = ymd(end))
      #
      # sum_rain <-
      #    station_site %>%
      #    mutate(log_date = ymd(paste(year, month, day, sep = "-"))) %>%
      #    filter(log_date %within% season) %>%
      #    summarise(total_rain = sum(rainfall, na.rm = TRUE)) %>%
      #    pull()
      #
      # return(sum_rain)
      
      
      
      
      
      
      
      
      
      
      input_data <- data.frame(location_name, latitude, longitude)
      trials <- unique(input_data)
      
      
      # Download data
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


      input_data$stat_file <- NA_character_
      # add a column with the weather station file to 
      for (i in 1:nrow(trials)) {
         input_data <- 
            input_data%>%
            mutate(stat_file = case_when(location_name == trials[i, "location_name"] &
                                            latitude == trials[i, "latitude"] &                     
                                            longitude == trials[i, "longitude"] ~ 
                                            trials[i, "stat_file"],
                                         TRUE ~ stat_file
                                            ),
                   s1 = ymd(start),
                   e1 = ymd(end))%>%
            mutate(season = interval(start = s1, end = e1))
      }
      
      
      # apply(input_data, 1, function(x1){
      #   
      #})
      
         
      # cbind(input_data, start, end)
      # 
      #    input_data$season <- interval(start = ymd(input_data$start), end = ymd(input_data$end))
      #    
      #    # input_data %>%
         #    mutate(sum_rain = read.csv())
         # 
         # sum_rain <-
         #    weather %>%
         #    mutate(log_date = ymd(paste(year, month, day, sep = "-"))) %>%
         #    filter(log_date %within% season) %>%
         #    summarise(total_rain = sum(rainfall, na.rm = TRUE)) %>%
         #    pull()
         # 
         # return(sum_rain)
         
      
      

      
      return(input_data)
   }