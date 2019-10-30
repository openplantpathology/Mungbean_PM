AUS_rainfall <- function(StartDate = Sys.Date()-30,
                         EndDate = Sys.Date(),
                         latlong = NULL,
                         StationID = "041525"
                       ){
   # This function should return the quantity of rainfall between the input dates
   #library(bomrang)
   # lat <- experiment_sites[experiment_sites$location == "Missen Flats","lat"]
   # long <- experiment_sites[experiment_sites$location == "Missen Flats","lon"]
   
   
   StartDate <- as.Date(StartDate)
   EndDate <- as.Date(EndDate)
   latlong <- as.numeric(latlong)
   
   if(is.null(latlong)){
      
      temp_files <- list.files(tempdir())
      if(sum(temp_files == paste0("IDCJAC0009_",StationID,"_1800_Data.csv"))>0){
         rain <- read.csv(paste0(tempdir(),"\\IDCJAC0009_",StationID,"_1800_Data.csv"))
      }else{
      
      rain <- as.data.frame(get_historical(stationid = StationID, type = c("rain")))
      }
      
      
      
   }else{
   
   
   
			
      
      if(sum(list.files(tempdir()) == "cache_rain.csv") > 0){
         cache_rain <- read.csv(paste0(tempdir(),"\\cache_rain.csv"), stringsAsFactors = FALSE)

         if(length(cache_rain[cache_rain$latt == as.numeric(latlong[1]) &
                       cache_rain$lon == as.numeric(latlong[2]),]$BOM.station.number) > 0){
            StationID <- as.character(cache_rain[cache_rain$latt == as.numeric(latlong[1]) &
                                                    cache_rain$lon == latlong[2],]$BOM.station.number[1])
            
            if(nchar(StationID) == 5){
               StationID <- paste0("0",StationID)
            }
            if(nchar(StationID) == 4){
               StationID <- paste0("00",StationID)
            }
            
            temp_files <- list.files(tempdir())
            if(sum(temp_files == paste0("IDCJAC0009_",StationID,"_1800_Data.csv")) > 0){
               rain <- read.csv(paste0(tempdir(),"\\IDCJAC0009_",StationID,"_1800_Data.csv"), stringsAsFactors = FALSE)
               }
            if(sum(temp_files == paste0("IDCJAC0009_",StationID,"_1800_Data.csv")) == 0){
                  rain <- as.data.frame(get_historical(latlon = as.numeric(latlong) , type = c("rain")))
               }
            
            }else{
               rain <- as.data.frame(get_historical(latlon = as.numeric(latlong) , type = c("rain")))
            }
         }
      if(sum(list.files(tempdir()) == "cache_rain.csv") == 0){
            cache_rain <- data.frame()
            rain <- as.data.frame(get_historical(latlon = as.numeric(latlong) , type = c("rain")))
         }
      
     
      
         
    }
	head(rain)
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
                                      download_date = as.character(Sys.time()),
                                      latt = latlong[1],
                                      lon = latlong[2],
                                      stringsAsFactors = FALSE)
         
         cache_rain <- rbind(cache_rain,cache_rain_tmp)
         write.csv(cache_rain, paste0(tempdir(),"\\cache_rain.csv"), row.names = FALSE)

   #StartDate <- as.Date("2017-01-27")-10
   #EndDate <- as.Date("2017-01-27")+90
   
   season <- as.numeric(as.Date(EndDate) - as.Date(StartDate))
   
   Date1 <- as.numeric(unlist(strsplit(as.character(StartDate),"-")))
   
   
   row_start <- which(rain[,3] == Date1[1] &
                         rain[,4] == Date1[2] &
                         rain[,5] == Date1[3])
   

   
   
   rainfall <- sum(rain[as.numeric(row_start):(as.numeric(row_start) + as.numeric(season)),"Rainfall.amount..millimetres."],na.rm = TRUE)
   message(paste("rainfall:",rainfall))
   message(paste0("Season days: ",season))
   return(rainfall)                                       
}
