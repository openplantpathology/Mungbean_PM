AUS_rainfall <- function(StartDate = Sys.Date()-30,
                         EndDate = Sys.Date(),
                         lat = NULL,
                         long = NULL,
                         StationID = "041525"
                       ){
   # This function should return the quantity of rainfall between the input dates
   #library(bomrang)
   #lat <- experiment_sites[experiment_sites$location == "Emerald Agricultural College","lat"]
   #long <- experiment_sites[experiment_sites$location == "Emerald Agricultural College","lon"]
   

   StartDate <- as.Date(StartDate)
   EndDate <- as.Date(EndDate)
   
   if(is.null(lat) & is.null(long)){
      
      temp_files <- list.files(tempdir())
      if(sum(temp_files == paste0("IDCJAC0009_",StationID,"_1800_Data.csv"))>0){
         rain <- read.csv(paste0(tempdir(),"\\IDCJAC0009_",StationID,"_1800_Data.csv"))
      }else{
      
      rain <- as.data.frame(get_historical(stationid = StationID, type = c("rain")))
      }
      
      
      
   }else{
   
   
   
			
      
      if(sum(list.files(tempdir()) == "cache_rain.csv") > 0){
         cache_rain <- read.csv(paste0(tempdir(),"\\cache_rain.csv"), stringsAsFactors = FALSE)
         
         if(length(cache_rain[cache_rain$latt == lat &
                       cache_rain$lon == long,]$BOM.station.number) > 0){
            StationID <- as.character(cache_rain[cache_rain$latt == lat &
                                                    cache_rain$lon == long,]$BOM.station.number[1])
            
            if(nchar(StationID) == 5){
               StationID <- paste0("0",StationID)
            }
            if(nchar(StationID) == 4){
               StationID <- paste0("00",StationID)
            }
            
            temp_files <- list.files(tempdir())
            if(sum(temp_files == paste0("IDCJAC0009_",StationID,"_1800_Data.csv"))>0){
               rain <- read.csv(paste0(tempdir(),"\\IDCJAC0009_",StationID,"_1800_Data.csv"), stringsAsFactors = FALSE)
               }else{
                  rain <- as.data.frame(get_historical(latlon = unlist(c(lat,long)) , type = c("rain")))
               }
            #rain <- as.data.frame(get_historical(latlon = unlist(c(lat,long)) , type = c("rain")))
            }else{
               rain <- as.data.frame(get_historical(latlon = unlist(c(lat,long)) , type = c("rain")))
            }
         }else{
            cache_rain <- data.frame()
            rain <- as.data.frame(get_historical(latlon = unlist(c(lat,long)) , type = c("rain")))
         }
      
     
      
         
    }
	
	rain$Bureau.of.Meteorology.station.number <- as.character(rain[,2])
	
	if(nchar(rain$Bureau.of.Meteorology.station.number[1]) == 5){
	rain$Bureau.of.Meteorology.station.number[1] <- paste0("0",rain$Bureau.of.Meteorology.station.number[1])
	}
	if(nchar(rain$Bureau.of.Meteorology.station.number[1]) == 4){
	rain$Bureau.of.Meteorology.station.number[1] <- paste0("00",rain$Bureau.of.Meteorology.station.number[1])
	}
     


	 
         cache_rain_tmp <- data.frame(Product.code = as.character(rain[1,1]),
                                      BOM.station.number = 
                                         as.character(rain$Bureau.of.Meteorology.station.number[1]),
                                      download_date = Sys.time(),
                                      latt = lat,
                                      lon = long,
                                      stringsAsFactors = FALSE)
         
         cache_rain <- rbind(cache_rain,cache_rain_tmp)
         write.csv(cache_rain, paste0(tempdir(),"\\cache_rain.csv"), row.names = FALSE)

   
   
   season <- as.numeric(as.Date(EndDate) - as.Date(StartDate))
   
   Date1 <- as.numeric(unlist(strsplit(as.character(StartDate),"-")))
   
   
   row_start <- as.numeric(rownames(rain[rain$year == Date1[1] &
                   rain$month == Date1[2] &
                   rain$day == Date1[3], ]))
   
   rainfall <- sum(rain[row_start:(row_start+season),"rainfall"],na.rm = T)
   message(paste0("Season days: ",season))
   return(rainfall)                                       
}
