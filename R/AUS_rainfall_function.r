AUS_rainfall <- function(StartDate = Sys.Date()-30,
                         EndDate = Sys.Date(),
                       StationID = "041525",
                       latlong = NULL){
   # This function should return the quantity of rainfall between the input dates
   library(bomrang)
   
   
   if(is.null(latlong)){
      
      rain <- as.data.frame(get_historical(stationid = StationID, type = c("rain")))   
   }else{
      rain <- as.data.frame(get_historical(latlon = latlong , type = c("rain")))   
   }
   
   season <- EndDate - StartDate
   
   Date1 <- as.numeric(unlist(strsplit(as.character(StartDate),"-")))
   Date2 <- as.numeric(unlist(strsplit(as.character(StartDate),"-")))
   
   x1 <- sum(rain[rain$year == Date1[1] &
                     rain$year == Date2[1] &
                     rain$month == Date[2] &
                     rain$day >= Date[3] &
                     rain$day < Date[3] + 7, ]$rainfall > 0, na.rm = TRUE)
   return(x1)                                       
}