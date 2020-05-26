impute_yield_err <- function(dat,xbar, Var,reps){
# dat is the whole dataframe containing all the values
# xbar  (character class) is column name of the means of the variable that needs imputation
# Var (character class)is the repective column name of the Variance of the variable that needs imputation
   
   
# generate yield error values
for (i in 0:500) {
   # Imputing missing log variances
   log_Yerror <- rnorm(
      n = sum(is.na(dat[,Var])),
      mean = mean(log(dat[,Var]), na.rm = TRUE),
      sd = sd(log(dat[,Var]), na.rm = TRUE)
   )
   
   # First let's convert Variance to standard error for all the variables that have variance recorded
   dat$yield_SE <-
      sqrt(dat[,Var]) / sqrt(dat[,reps])
   
   
   # Second; adding the imputed standard errors
   dat[is.na(dat[,Var]), "yield_SE"] <-
      sqrt(exp(log_Yerror)) / sqrt(dat[is.na(dat[,Var]), reps])
   
   # all errorbars should overlap so they are consistant with the meta-data describing no significant difference bettween treatments
   # let's test to make sure
   if (max(outer(
      X = dat[is.na(dat[,Var]), xbar] -
      (2 * dat[is.na(dat[,Var]), "yield_SE"]),
      Y = dat[is.na(dat[,Var]), xbar] +
      (2 * dat[is.na(dat[,Var]), "yield_SE"]),
      FUN = "-"
   )) > 0) {
      message(
         paste(
            "\nWarning!!: Imputed yield errors bestow significant differences on the means.\n",
            "The original experiment reported no significant differences in the yields, therefore values will be discarded and the imputation function run again - iteration number:",
            "i =",
            i
         )
      )
      next()
   } else{
      message(
         paste(
            i,
            "iterations: ",
            "imputed variances now show no significant distances :)\n Now adding variances to data "
         )
      )
      
      # Plot the imputed errors for each trial
      # Plot will not work without the dataset it was designed for.
      Vplot <-
         dat[is.na(dat[,Var]), ] %>%
         ggplot(aes(y = grain_yield.t.ha, trial)) +
         geom_pointrange(
            ymin = dat[is.na(dat[,Var]), xbar] -
               (1.95 * dat[is.na(dat[,Var]), "yield_SE"]),
            ymax = dat[is.na(dat[,Var]), xbar] +
               (1.95 * dat[is.na(dat[,Var]), "yield_SE"]),
            position = "jitter"
         ) +
         ylim(0.2, 1.5)
      
      dat[is.na(dat[,Var]), Var] <-
         exp(log_Yerror)
      print(Vplot)
      break()
   }
}
   
   return(list(new_dataFrame = dat,
               var_plot = Vplot))
}
