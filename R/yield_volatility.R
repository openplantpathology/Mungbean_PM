yield_volatility <- function(genotype,
                             control_only = TRUE,
                             genotype_by_trial = TRUE,
                             location = NA) {
   if (isTRUE(genotype_by_trial)) {
      PlotTitle1 <-
         "Probability of yield (t/ha) for each genotype grouped by trial"
      
      if (isTRUE(control_only)) {
         dat1 <- PM_MB_means[PM_MB_means$host_genotype == genotype &
                                PM_MB_means$Y_error_type == "stdev" &
                                PM_MB_means$trial_ref != "mung1718/01" &
                                # Outlier
                                PM_MB_means$fungicide_ai == "control",]
         PlotTitle2 <- "\nin no spray control plots only"
         
      } else{
         dat1 <- PM_MB_means[PM_MB_means$host_genotype == genotype &
                                PM_MB_means$trial_ref != "mung1718/01" &
                                # Outlier
                                PM_MB_means$Y_error_type == "stdev",]
         PlotTitle2 <- "\nin no spray control and spray treated plots"
      }
      
      dat1 <- dat1[!is.na(dat1$trial_ref), ]
      
      for (i in unique(dat1$trial_ref)) {
         dat2 <- dat1[dat1$trial_ref == i, ]
         for (j in seq_along(dat2$trial_ref)) {
            if (j == 1 & i == unique(dat1$trial_ref)[1]) {
               dat3 <- data.frame()
            }
            
            
            dat3 <- rbind(dat3, data.frame(
               trial = rep(i, times = 600),
               values = rnorm(
                  n = 600,
                  mean = dat2$grain_yield.t.ha.[j],
                  sd = dat2$yield_error[j]
               )
            ))
         }
      }
      
      seq_along(10)
      
      Plot1 <- dat3 %>%
         ggplot(aes(x = values)) +
         geom_density(aes(fill = trial), alpha = 0.4) +
         xlim(0.25, 2.25) +
         ggtitle(paste0(PlotTitle1, PlotTitle2))
      
      mean_Y_volatility <- dat3 %>%
         group_by(trial) %>%
         summarise(volatility = sort(values)[0.975 * length(values)] -
                      sort(values)[0.025 * length(values)])
      
      
      return(list(dat3, Plot1, mean_Y_volatility))
   }
   
   
   
   
   
   if (isFALSE(genotype_by_trial)) {
      PlotTitle1 <-
         "Probability of yield (t/ha) for each genotype regardless of trial"
      
      if (isTRUE(control_only)) {
         if (!is.na(location)) {
            dat1 <- PM_MB_means[PM_MB_means$Y_error_type == "stdev" &
                                   PM_MB_means$location == location &
                                   PM_MB_means$trial_ref != "mung1718/01" &
                                   # Outlier
                                   PM_MB_means$fungicide_ai == "control",]
            PlotTitle2 <- "\nin no spray control plots only"
            
         }
         if (is.na(location)) {
            dat1 <- PM_MB_means[PM_MB_means$Y_error_type == "stdev" &
                                   PM_MB_means$trial_ref != "mung1718/01" &
                                   # Outlier
                                   PM_MB_means$fungicide_ai == "control",]
            PlotTitle2 <- "\nin no spray control and treated plots"
            
         }
         
         
      } else{
         if (is.na(location)) {
            dat1 <-
               PM_MB_means[PM_MB_means$trial_ref != "mung1718/01" & # Outlier
                              PM_MB_means$Y_error_type == "stdev",]
         }
         if (!is.na(location)) {
            dat1 <- PM_MB_means[PM_MB_means$location == location &
                                   PM_MB_means$trial_ref != "mung1718/01" &
                                   # Outlier
                                   PM_MB_means$Y_error_type == "stdev",]
         }
      }
      
      dat1 <- dat1[!is.na(dat1$trial_ref), ]
      dat1 <- dat1[!is.na(dat1$host_genotype), ]
      
      for (i in unique(as.character(dat1$host_genotype))) {
         dat2 <- dat1[dat1$host_genotype == i, ]
         for (j in seq_along(dat2$host_genotype)) {
            if (j == 1 & i == unique(dat1$host_genotype)[1]) {
               dat3 <- data.frame()
            }
            
            
            dat3 <-
               rbind(dat3, data.frame(
                  trial = rep(i, times = 600),
                  values = rnorm(
                     n = 600,
                     mean = dat2$grain_yield.t.ha.[j],
                     sd = dat2$yield_error[j]
                  )
               ))
         }
      }
      
      
      Plot1 <- dat3 %>%
         ggplot(aes(x = values)) +
         geom_density(aes(fill = trial), alpha = 0.4) +
         xlim(0.25, 2.25)
      
      mean_Y_volatility <- dat3 %>%
         group_by(trial) %>%
         summarise(volatility = sort(values)[0.975 * length(values)] -
                      sort(values)[0.025 * length(values)])
      
      
      return(list(dat3, Plot1, mean_Y_volatility))
   }
   
   
}