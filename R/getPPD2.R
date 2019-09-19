#' Download PPD data
#' 
#' Download PPD data for a station and return as a data frame
#'
#' @param id Weather station ID.
#' @param start Start date (String YYYYmmdd).
#' @param end End date (String YYYYmmdd).
#' @param silo.apiKey SILO API key (available from https://silo.longpaddock.qld.gov.au/).
#' @param variables Vector of weather variables (Full list available  at https://silo.longpaddock.qld.gov.au/climate-variables)
#'
#' @author Paul Melloy & Fiona Evans
#' 
#' @return Data frame containing daily weather data.
#' @export


getPPD2 <- function(station_id, lat, long, start, end, silo.apikey, variables) {
if(missing(lat)|missing(long)){
# 'If' statement for when user does not specify variables
   if(missing(variables)){
      variables <- paste("daily_rain", "max_temp", "min_temp", "rh_tmax", "rh_tmin","vp", "vp_deficit", "radiation",
              sep = ",")
   }

   
url <- "https://siloapi.longpaddock.qld.gov.au/pointdata?apikey="


   print(paste0(url,silo.apikey , "&start=", start,  "&finish=", end,  "&station=", station_id,  "&format=csv", "&variables=", variables))
read.csv(paste0(url,silo.apikey , "&start=", start,  "&finish=", end,  "&station=", station_id,  "&format=csv", "&variables=", variables))



}else{ 
   if(missing(station_id)) {
   # 'If' statement for when user does not specify variables
   if(missing(variables)){
      variables <- paste("daily_rain", "max_temp", "min_temp", "rh_tmax", "rh_tmin","vp", "vp_deficit", "radiation",
                         sep = ",")
   }
   
   url <- "https://siloapi.longpaddock.qld.gov.au/pointdata?apikey="
   
   print(paste0(url,silo.apikey , "&start=", start,  "&finish=", end,  "&lat=", lat, "&lon=",long,  "&format=csv", "&variables=", variables))   
read.csv(paste0(url,silo.apikey , "&start=", start,  "&finish=", end,  "&lat=", lat, "&lon=",long,  "&format=csv", "&variables=", variables))


   
   
}
   
}
   
}

# Testing
# silo_war_Tmax <- getPPD2(station_id = 041525,start = 20110101,end = 20160630, silo.apikey = silo.API, variables = "max_temp,min_temp,daily_rain")

# url2 <- "https://siloapi.longpaddock.qld.gov.au/pointdata?apikey=YeTZsWlFNC8hvbQLllv8KtbRQWsje4qxic5QQ7mL&start=20110101&finish=20160630&station=41525&format=csv&variables=max_temp,min_temp,daily_rain"

# library(httr)
# r <- GET(url = "https://siloapi.longpaddock.qld.gov.au/pointdata?apikey=YeTZsWlFNC8hvbQLllv8KtbRQWsje4qxic5QQ7mL&start=20110101&finish=20160630&station=41525&format=csv&variables=max_temp,min_temp,daily_rain")


