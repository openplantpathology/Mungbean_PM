# this is a function to test a disease progress curve for it's ability
# To fit a linear or polynomial functions.
# The function expects a dataframe where each column variable is a column of disease severity ratings for a specific day
# The function also requires a vector of days the same length as the number of columns and
# corresponding to the days separating each disease observation 
# There is a beta function which can handle multiple treatments


library(lme4)
library(dplyr)
library(agricolae)
library(ggplot2)
dpc <- function(dat, days, treat_var = NULL,treat_subset = NULL){

dat$dis_zero <- 1
dat <- select(dat, dis_zero, everything())
disease_var <- sapply(dat,class) %in% "numeric"
colNames_Disease <- colnames(dat)[disease_var]
if(!is.null(treat_var)){
colNames_Disease <- colNames_Disease[colNames_Disease != (treat_var)]}

if(length(days) != length(colNames_Disease)){
   warning("Number of column variables for disease observations does not match the length of the number of days")
}

# Change dataset from wide to long
dat_long <- as.data.frame(dat %>%
   pivot_longer(
      #-treat_var,
         cols = colNames_Disease,
         names_to = "disease_obs",
         values_to = "dis_score")
)

# add a column which will reference the day number of disease observation
# this will be used in agricolae
dat_long$days <- 0   

# assign the number of days to align with the disease observation columns
# This assumes the columns were arranged left to right from earliers to latest observation
   for(i in colNames_Disease){
      dat_long[dat_long$disease_obs == i,"days"] <- days[colNames_Disease == i]
   }
   
# Order the factor to ensure they will be understood from earliest to latest observation
   dat_long$disease_obs <- reorder(factor(dat_long$disease_obs), dat_long$days)
   
   
# undertake linear model Plotting and AUDPC for when treatment_var is not specified
      if(is.null(treat_var)){
         
         lm_1 <- lm(dat_long[,"dis_score"] ~ 
                       dat_long[,"days"]
         )
         
         lm_sqrt <- lm(sqrt(dat_long[,"dis_score"]) ~ 
                          dat_long[,"days"]
         )
         
         lm_log <- lm(log(dat_long[,"dis_score"]) ~ 
                         dat_long[,"days"]
         )
         
         lm_2 <- lm((dat_long[,"dis_score"])^2 ~ 
                       dat_long[,"days"]
         )
         
         lm_3 <- lm((dat_long[,"dis_score"])^3 ~ 
                       dat_long[,"days"]
         )
         
         lm_4 <- lm((dat_long[,"dis_score"])^4 ~ 
                       dat_long[,"days"]
         )
         
         lm_summ <- function(lmodels){
            summary1 <- data.frame(model = deparse(substitute(lmodels)),
                                   summary(lmodels)$coefficients,
                                   r_squared = summary(lmodels)$r.squared)
            row.names(summary1) <- gsub("\\]", "",
                                        gsub("dat_long\\[, ",
                                             "",row.names(summary1)))
            return(summary1)}
         
         lm_summarys <- bind_rows(
            lm_summ(lm_1),
            lm_summ(lm_sqrt),
            lm_summ(lm_log),
            lm_summ(lm_2),
            lm_summ(lm_3),
            lm_summ(lm_4)
         )
         
         
         Plot <- 
            dat_long %>%
            ggplot(aes(x = days, y = dis_score))+
            geom_point()+
            geom_jitter(width = 0.1)+
            geom_smooth()
         
         AUDPC_full <- audpc(dat[,colNames_Disease], days)
         AUDPC_ends <- audpc(dat[,c(head(colNames_Disease,1),tail(colNames_Disease,1))], 
                             c(head(days,1),tail(days,1)))
         
      ###############################3
      }else{
         
         dat_long[,treat_var] <- factor(dat_long[,treat_var])
         
         lm_1 <- lm(dat_long[,"dis_score"] ~ 
                   dat_long[,"days"]:dat_long[,treat_var]
                )
         
         lm_sqrt <- lm(sqrt(dat_long[,"dis_score"]) ~ 
                      dat_long[,"days"]:dat_long[,treat_var]
         )
         
         lm_log <- lm(log(dat_long[,"dis_score"]) ~ 
                      dat_long[,"days"]:dat_long[,treat_var]
         )
         
         lm_2 <- lm((dat_long[,"dis_score"])^2 ~ 
                         dat_long[,"days"]:dat_long[,treat_var]
         )
         
         lm_3 <- lm((dat_long[,"dis_score"])^3 ~ 
                       dat_long[,"days"]:dat_long[,treat_var]
         )
         
         lm_4 <- lm((dat_long[,"dis_score"])^4 ~ 
                       dat_long[,"days"]:dat_long[,treat_var]
         )
      
      Plot <- 
         dat_long %>%
         ggplot(aes(x = days, y = dis_score))+
         geom_jitter(width = 0.1)+
         geom_smooth()
      
      
      
      lm_summ <- function(lmodels){
               summary1 <- data.frame(model = deparse(substitute(lmodels)),
                                      summary(lmodels)$coefficients,
                                      r_squared = summary(lmodels)$r.squared)
               row.names(summary1) <- gsub("\\]", "_",
                                           gsub("dat_long\\[, \"days\"\\]:dat_long\\[,",
                                                "",row.names(summary1)))
               return(summary1)}
      
               lm_summarys <- bind_rows(
                  lm_summ(lm_1),
                  lm_summ(lm_sqrt),
                  lm_summ(lm_log),
                  lm_summ(lm_2),
                  lm_summ(lm_3),
                  lm_summ(lm_4)
            )
            
               AUDPC_full <- audpc(dat[,colNames_Disease], days)
               AUDPC_ends <- audpc(dat[,c(head(colNames_Disease,1),tail(colNames_Disease,1))], 
                                       c(head(days,1),tail(days,1)))
   }
 ###############################3  
   
   
   dat_long <- dat_long
   
   return(list(lm_summaries = lm_summarys,
               Plot = Plot,
               data_long = dat_long,
               data_wide = dat,
               lm_1 = lm_1,
               lm_sqrt = lm_sqrt,
               lm_sqrt = lm_sqrt,
               lm_2 = lm_2,
               lm_3 = lm_3,
               lm_4 = lm_4,
               AUDPC_full = AUDPC_full,
               AUDPC_ends = AUDPC_ends))
}


# More testing
# test1 <- Herm_10 %>%
#    filter(`Treat!` == 5)%>%
#    select(P.M.9wap, P.M.11wap, P.M.13wap)
#    
# test1.1 <- dpc(test1, days = c(-7,7,14,28), treat_var = `Treat!`)
# test1.2 <- dpc(test1, days = c(-7,7,14,28))
# 
# colnames(King_11)
# test2 <- King_11 %>%
#    filter(fungicide == "control")%>%
#    select(`9 w.a.p.`, `11 w.a.p.`,`12 w.a.p.`,`14 w.a.p.`)
# 
# test2.1 <- dpc(test2, days = c(-7,0,14,21,35))
# 
# test2.1$lm_summaries
# test2.1$Plot
# mean(test2.1$AUDPC_full)
# mean(test2.1$AUDPC_ends)
