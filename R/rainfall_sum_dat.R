# rainfall sum data.fram
# Import the information for each experimental site
experiment_sites <- read.csv("data/Mungbean_experiment_sites.csv", stringsAsFactors = FALSE)


rain_dat <- PM_MB_means[!is.na(PM_MB_means$planting_date),] # Which locations don't have NA for planting date
unique(as.character(rain_dat$location)) # locations with recorded planting date

# Create data set for which we will input the sum in-crop rainfall
rain_dat_sum <- rain_dat %>%
   group_by(trial_ref) %>%
   summarise(location = unique(location),
             planting_date = unique(as.character(planting_date))[1],
             grain_yield = mean(grain_yield.t.ha., na.rm = TRUE))

rain_dat_sum <- rain_dat_sum[order(rain_dat_sum$location),] # order them to make adding the lat long easier

# add columns for lat and lon
rain_dat_sum$lat <- NA
rain_dat_sum$lon <- NA


# Input lat and lon to dataframe for each experimental location
rain_dat_sum[rain_dat_sum$location == "Bongeen_1", c("lat","lon")] <- 
   experiment_sites[experiment_sites$location == "Bongeen 1 & 2",c("lat","lon")]

rain_dat_sum[rain_dat_sum$location == "Hermitage", c("lat","lon")] <- 
   experiment_sites[experiment_sites$location == "Hermitage Research Station",c("lat","lon")]

rain_dat_sum[rain_dat_sum$location == "Gatton", c("lat","lon")] <- 
   experiment_sites[experiment_sites$location == "Gatton",c("lat","lon")]

rain_dat_sum[rain_dat_sum$location == "Kingaroy", c("lat","lon")] <- 
   experiment_sites[experiment_sites$location == "Red Vale & Kingaroy Research Station",c("lat","lon")]

rain_dat_sum[rain_dat_sum$location == "Missen Flats", c("lat","lon")] <- 
   experiment_sites[experiment_sites$location == "Missen Flats",c("lat","lon")]

# sweep_for_stations(latlon = unlist(c(rain_dat_sum[rain_dat_sum$location == "Premer", c("lat","lon")])))[2,c("lon")]
# closest weather to Premer does not have rain, recoding so it takes the next closest station
rain_dat_sum[rain_dat_sum$location == "Premer", c("lat","lon")] <- 
   sweep_for_stations(latlon = unlist(c(rain_dat_sum[rain_dat_sum$location == "Premer", c("lat","lon")])))[2,c("lat","lon")]

rain_dat_sum[rain_dat_sum$location == "Emerald", c("lat","lon")] <- 
   experiment_sites[experiment_sites$location == "Emerald Agricultural College",c("lat","lon")]

rain_dat_sum[rain_dat_sum$location == "Redvale", c("lat","lon")] <- 
   experiment_sites[experiment_sites$location == "Red Vale & Kingaroy Research Station",c("lat","lon")]

rain_dat_sum[rain_dat_sum$location == "Wellcamp", c("lat","lon")] <- 
   experiment_sites[experiment_sites$location == "Wellcamp",c("lat","lon")]

rain_dat_sum[rain_dat_sum$location == "Dalby", c("lat","lon")] <- c(-27.168426, 151.264421)











# add column for sum in-crop rainfall
rain_dat_sum$rainfall_sum <- NA