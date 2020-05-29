# read in data
library(metafor)
library(dplyr)


slimmer_PM_dat <- read.csv("./cache/slimmer_PM_clusterdat.csv")
dat1 <- read.csv("./cache/metaIteration.csv")

i2 <- max(dat1$iteration)

# Identify trials without variance
   TrialMSQ <- slimmer_PM_dat %>%
      group_by(trial_ref,location,year)%>%
      summarise(unique(Y_Msquare))
   
   # impute variance
   for (i in TrialMSQ[is.na(TrialMSQ$`unique(Y_Msquare)`), ]$trial_ref) {
      slimmer_PM_dat[slimmer_PM_dat$trial_ref == i, "Y_Msquare"] <-
         exp(rnorm(
            n = 1,
            mean = mean(log(TrialMSQ$`unique(Y_Msquare)`), na.rm = TRUE),
            sd(log(TrialMSQ$`unique(Y_Msquare)`), na.rm = TRUE)
         ))
   }
   
   #######
   
   #reformat data
   slimmer_PM_dat$yi <- log(slimmer_PM_dat$grain_yield.t.ha)
   slimmer_PM_dat$vi <-
      slimmer_PM_dat$Y_Msquare / (slimmer_PM_dat$n * slimmer_PM_dat$grain_yield.t.ha ^
                                     2)
   
   slimmer_PM_dat$spray_management <-
      factor(slimmer_PM_dat$spray_management)
   slimmer_PM_dat$trial_ref <- factor(slimmer_PM_dat$trial_ref)
   slimmer_PM_dat$trial <- factor(slimmer_PM_dat$trial)
   
   
   # Run meta-analysis
   PM_mv_AI <- rma.mv(
      yi,
      vi,
      mods = ~ spray_management,
      method = "ML",
      random = list(~ spray_management | trial),
      struct = "UN",
      data = slimmer_PM_dat
   )
   
   dat2 <- data.frame(
      iteration = i2+1,
      mods = names(coef(PM_mv_AI)),
      coefs = coef(PM_mv_AI),
      se = PM_mv_AI$se,
      zval = PM_mv_AI$zval,
      pval = PM_mv_AI$pval,
      ci.lb = PM_mv_AI$ci.lb,
      ci.ub = PM_mv_AI$ci.ub,
      row.names = NULL
   )
   
   
   # this is to check that another process has not finished between 
   # the start of this process and the end of it.
   # if there is it inserts the new data into the file
   
   dat1.1 <- read.csv("./cache/metaIteration.csv")
   i3 <- max(dat1.1$iteration)
   if(i3 != i2){
      dat2$iteration <- dat2$iteration + (i3 -i2)
      dat1 <- rbind(dat1.1, dat2)
   }else{
      dat1 <- rbind(dat1,dat2)   
   }

write.csv(dat1, "./cache/metaIteration.csv", row.names = FALSE)