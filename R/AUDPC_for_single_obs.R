# estimating_AUDPC_for_trials with no raw data

library(agricolae)

estAUDPC <- PM_MB_means[is.na(PM_MB_means$AUDPC_m) &
                           !is.na(PM_MB_means$PM_final_severity),]

# are there any entries which don't state the first sign of disease?
any(is.na(estAUDPC$first_sign_disease)) # No


# are there any entries which don't state when they were assessed for disease at the end of the trial
any(is.na(estAUDPC$final_assessment)) # No

#Good! Lets begin 

estAUDPC$tInf <- as.Date(estAUDPC$final_assessment, format = "%Y-%m-%d") - 
   as.Date(estAUDPC$first_sign_disease, format = "%Y-%m-%d")

for(i in unique(estAUDPC$trial_ref)){
   x1 <- apply(data.frame(zero = rep(0, times = length(estAUDPC[estAUDPC$trial_ref == i,"final_assessment"])),
                          FSev = estAUDPC[estAUDPC$trial_ref == i,"PM_final_severity"]-1),1,
               FUN = audpc, dates = c(0, unique(estAUDPC[estAUDPC$trial_ref == i,"tInf"])+7)
   )
   estAUDPC[estAUDPC$trial_ref == i,"AUDPC_m"] <- x1
}

PM_MB_means[is.na(PM_MB_means$AUDPC_m) &
               !is.na(PM_MB_means$PM_final_severity),"AUDPC_m"] <- estAUDPC$AUDPC_m

#____________________________AUDPStairs________________________________________________
# estimating_AUDPStairs_for_trials with no raw data

estAUDPS <- PM_MB_means[is.na(PM_MB_means$AUDPS_m) &
                           !is.na(PM_MB_means$PM_final_severity),]

# are there any entries which don't state the first sign of disease?
any(is.na(estAUDPS$first_sign_disease)) # No


# are there any entries which don't state when they were assessed for disease at the end of the trial
any(is.na(estAUDPS$final_assessment)) # No

#Good! Lets begin 

estAUDPS$tInf <- as.Date(estAUDPS$final_assessment, format = "%Y-%m-%d") - 
   as.Date(estAUDPS$first_sign_disease, format = "%Y-%m-%d")

for(i in unique(estAUDPS$trial_ref)){
   x1 <- apply(data.frame(zero = rep(0, times = length(estAUDPS[estAUDPS$trial_ref == i,"final_assessment"])),
                          FSev = estAUDPS[estAUDPS$trial_ref == i,"PM_final_severity"]-1),1,
               FUN = audps, dates = c(0, unique(estAUDPS[estAUDPS$trial_ref == i,"tInf"])+7)
   )
   estAUDPS[estAUDPS$trial_ref == i,"AUDPS_m"] <- x1
}

PM_MB_means[is.na(PM_MB_means$AUDPS_m) &
               !is.na(PM_MB_means$PM_final_severity),"AUDPS_m"] <- estAUDPS$AUDPS_m

