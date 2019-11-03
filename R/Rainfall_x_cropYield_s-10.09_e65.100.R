# In crop rainfall lm loop to predict grainyield
# See 05_MetaAnalysis.Rmd
#  Code chunk rainfall_sum_Sstart
# This file runs a linear model on grain yield and inseason rainfall
# this is to find the best timeframe that contributes to grain yeild
#
# This file contains a start date of:
#     between 10 days after planting and 30 days after planting
# An end date between 
#     60 days after planting and 95 days after planting.
# 

start1 <- -10
start2 <- 9
end1 <- 65
end2 <- 100


library(tidyverse)
library(theme.usq)
library(bomrang)
library(parallel)
source("R/import_data.R")


# ___________
# Import data
PM_MB_means <- import_data()
# see Yield vs in-season rain section on the influence of rain and irrigation on grain yield
PM_MB_means$irrigation <- NA
PM_MB_means[PM_MB_means$location == "Hermitage", "irrigation"] <- TRUE
PM_MB_means[PM_MB_means$location != "Hermitage", "irrigation"] <- FALSE


#_________
# Prep data
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

# source funtion that uses bomrang get_historical to return the sum rainfall between two dates
   source("R/AUS_rainfall_function.r")

# loop to download and save all the rainfall sum data
# this first loop Investigate when is the first day in the season for which in-crop rainfall is important

# use Bomrang to find the quantaty of rain for the season and do a linear regression
      lm_rain <- data.frame(start_day = rep(start1:start2, times = length(end1:end2)),
                            end_day = rep(end1:end2, each = length(start1:start2)),
                            lm_pval = NA,
                            lm_rsquared = NA,
                            lm_adj_rsquared = NA
      )
      #dim(lm_rain)
      
      
      # for using parRapply   
      # cl8 <- makeCluster(getOption("cl.cores", 5))
      # clusterExport(cl8, list("rain_dat_sum", "AUS_rainfall", "lm_rain", "get_historical"))
      # testlist1 <- parRapply(cl= cl8,lm_rain,function(x1){
      
      testlist1 <- apply(lm_rain,1,function(x1){
         
          
            rain_dat_sum$rainfall_sum <- apply(rain_dat_sum,1,function(x5){
               AUS_rainfall(StartDate = as.Date(x5[3]) + x1[1],
                            EndDate = as.Date(x5[3]) + x1[2],
                            latlong = as.vector(unlist(x5[5:6])))
            })
         
         
         class(as.vector(unlist(rain_dat_sum[1,5:6])))
         
         # for(i in seq_along(rain_dat_sum$trial_ref)){     
         #    rain_dat_sum$rainfall_sum[i] <- AUS_rainfall(StartDate = as.Date(rain_dat_sum$planting_date)[i] + x1[1],
         #                                                 EndDate = as.Date(rain_dat_sum$planting_date[i]) + 80 + x1[2],
         #                                                 latlong = c(rain_dat_sum$lat[i],rain_dat_sum$lon[i]))
         #                                                 
         # }
         
         lmod1 <- summary(lm(rainfall_sum ~ grain_yield, 
                             data = rain_dat_sum[rain_dat_sum$location != "Hermitage",])) # remove hermitage which is irrigated
         
         pval <- coef(lmod1)[2,4]
         rsqu <- lmod1$r.squared
         ADrs <- lmod1$adj.r.squared
         return(list(pval,rsqu,ADrs))
         
      })
      
      #stopCluster(cl8)
      
      
      
      
      
      
      for(i in seq_along(testlist1)){
         lm_rain$lm_pval[i] <- testlist1[[i]][[1]]
         lm_rain$lm_rsquared[i] <- testlist1[[i]][[2]]
         lm_rain$lm_adj_rsquared[i] <- testlist1[[i]][[3]]
      }
      
      write.csv(lm_rain, file = "data/lmInSeasonRainfall_-10.09_65.100.csv", row.names = FALSE)
      
      message("Script completed running")
      