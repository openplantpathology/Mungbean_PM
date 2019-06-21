# This script is dedicated to 
# importing and cleaning previous powdery Mildew experimental field trial data.

# install.packages("openxlsx")
# install.packages("readxl")
# install.packages("tidyr")

library(openxlsx)
library(readxl)
library(tidyr)
library(dplyr)
library(data.table)

# Year #  Trial location  #  Sowing date


# ____________________________________________________________
# _____________            Hermitage  2010        ____________

Herm_10 <- read_xls("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2010/2010 PMmung Herm/2010 PMmung hermitage RAW field book.xls", 
                        sheet = "For analysis")

Herm_10_m <- Herm_10 %>%
   group_by(`Cultivar!`, `Treat!`, `Description!`) %>%
   summarise_all(list(~mean(.,na.rm = TRUE),~sd(.,na.rm=TRUE)))
   
write.csv(as.data.frame(Herm_10_m ), "C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2010 PMmung Hermitage means.csv")


# ____________________________________________________________
# _____________            Hermitage  2011        ____________

Herm_11 <- read_xls("C:/Users/U8011054/OneDrive - USQ//Cloudstor/Mungbean/Past trials/2011/2011 PMmung Herm/pmhermratings2011.xls", 
                    sheet = "Analyses factor")


new_cols <- as.data.table(tstrsplit(Herm_11$'Treatment!'," "))
colnames(new_cols) <- c("spray_n", "spray", "fungicide")
Herm_11 <- cbind(new_cols, as.data.table(Herm_11)[, 'Treatment!' := NULL])
#Herm_11$`Yld Kg ha` <- as.numeric(Herm_11$`Yld Kg ha`)
head(Herm_11)

Herm_11_m <- Herm_11 %>%
   group_by(`Cultivar!`, `Tr`, `fungicide`, `spray_n`) %>%
   summarise_all(list(~mean(.,na.rm = TRUE),~sd(.,na.rm=TRUE)))

Herm_11_m <- data.table(Herm_11_m)[,c(1:4,8:12,16:20)]
colnames(Herm_11_m)

write.csv(as.data.frame(Herm_11_m ), "C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2011 PMmung Herm means.csv")




# ____________________________________________________________
# _____________            Kingaroy  2011        ____________

King_11 <- read_xls("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past trials/2011/2011 PMmung King/pmKing ratings2011.xls", 
                    sheet = "analyses")


new_cols <- as.data.table(tstrsplit(King_11$'Treatment!'," "))
colnames(new_cols) <- c("genotype", "spray_n", "spray", "fungicide")
King_11 <- cbind(new_cols, as.data.table(King_11)[, 'Treatment!' := NULL])
King_11$`Yld kg ha` <- as.numeric(King_11$`Yld kg ha`)
head(King_11)

King_11_m <- King_11 %>%
   group_by(`genotype`, `trt!`, `fungicide`, `spray_n`) %>%
   summarise_all(list(~mean(.,na.rm = TRUE),~sd(.,na.rm=TRUE)))

King_11_m <- data.table(King_11_m)[,c(1:4,8:13,18:23)]
colnames(King_11_m)

write.csv(as.data.frame(King_11_m ), "C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2011 PMmung Kingaroy means.csv")





# ____________________________________________________________
# _____________            Hermitage  2015        ____________

# Yeild data from 2016 Hermitage trial with final disease severity
Herm_15 <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2015/2015 PMmung Herm/Mugbean powdery mildew 2014-15 inc results v2.xlsx", 
                     sheet = "HRS yield data", startRow = 4, colNames = TRUE, detectDates = TRUE)[1:31,1:9]



Herm_15$Year <- 2015
Herm_15$location <- "Hermitage"

# Disease incidence assessments through the 2016 season
Herm_15.DR  <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2015/2015 PMmung Herm/Mugbean powdery mildew 2014-15 inc results v2.xlsx", 
                                    sheet = "HRS ratings & graphs", startRow = 5, colNames = TRUE)[1:31,1:8]

Herm_15$Plot.Length <- as.numeric(Herm_15$Plot.Length)
Herm_15$Plot.Weight <- as.numeric(Herm_15$Plot.Weight)
Herm_15$Plot.weight <- as.numeric(Herm_15$Plot.weight)
Herm_15$`Plot.Yield.(kg/ha)` <- as.numeric(Herm_15$`Plot.Yield.(kg/ha)`)
Herm_15$`Plot.Yield.(t/ha)` <- as.numeric(Herm_15$`Plot.Yield.(t/ha)`)


head(Herm_15)
#as.numeric(unlist(strsplit(Herm_15.DR$Treatment,)))
Herm_15$disease_severity <- as.numeric(Herm_15.DR[,8])



Herm_15_m <- as.data.table(Herm_15[,c(4,5,7:12)] %>%
   group_by(`Year`,`location`,`Treatment`) %>%
   summarise_all(list(~mean(.,na.rm = TRUE),~sd(.,na.rm=TRUE))))


write.csv(Herm_15_m[2:7,], "C:/Users/U8011054/OneDrive - USQ/Cloudstor/R/Powdery_mildew_Mungbean/Herm_15_means.csv")






# __________________________________________________________________________________
# _____________   Hermitage  2016 yeild and disease assessment means    ____________

# Yeild data from 2016 Hermitage trial with final disease severity
Herm_16 <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2016/Yeild Data 2016 Kingaroy Hermitage (PaulM adjusted).xlsx", 
                     sheet = "Hermitage", startRow = 1, colNames = TRUE)[1:28,1:9]



Herm_16$Year <- 2016
Herm_16$location <- "Hermitage"

# Disease incidence assessments through the 2016 season
Herm_16.DR  <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2016/2016 Mungbean PM trials summary.xlsx", 
                         sheet = "Hermitage_2016", startRow = 5, colNames = TRUE)


head(Herm_16.DR)
names(Herm_16.DR)[1] <- "Treatment"
names(Herm_16.DR)[6] <- "Final_Yield_tHa"
Herm_16.DR$Treatment <- c(4,3,7,5,2,6,1)
Herm_16.DR$Final_Yield_tHa <- Herm_16.DR$Final_Yield_tHa/1000
Herm_16.DR <- Herm_16.DR %>%
   gather(key = ass_date, value = mean_incidence, names(Herm_16.DR)[2:5]) %>%
   separate(col = ass_date, into = c("ass_date","days_after_emergence"), sep = "\\.", extra = "merge")
Herm_16.DR <- Herm_16.DR[order(Herm_16.DR$Treatment),]



# ______________________________________________________________________________________
# _____________   Kingaroy  2016 yeild and disease assessment means         ____________


King_16 <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2016/Yeild Data 2016 Kingaroy Hermitage (PaulM adjusted).xlsx", 
                     sheet = "Kingaroy", startRow = 1, colNames = TRUE)[1:28,1:9]


King_16$Year <- 2016
King_16$location <- "Kingaroy"




# Disease incidence assessments through the 2016 season
King_16.DR  <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2016/2016 Mungbean PM trials summary.xlsx", 
                         sheet = "Kingaroy_2016", startRow = 5, colNames = TRUE)



names(King_16.DR)[1] <- "Treatment"
names(King_16.DR)[6] <- "Final_Yield_tHa"
King_16.DR$Treatment <- c(3,2,7,6,5,4,1)
King_16.DR$Final_Yield_tHa <- King_16.DR$Final_Yield_tHa/1000
King_16.DR <- King_16.DR %>%
   gather(key = ass_date, value = mean_incidence, names(King_16.DR)[2:5]) %>%
   separate(col = ass_date, into = c("ass_date","days_after_emergence"), sep = "\\.", extra = "merge")
King_16.DR <- King_16.DR[order(King_16.DR$Treatment),]






# ____________________________________________________________
# _____________   Fogerty (missin flats) 2017     ____________
Fogerty_17 <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2017/Fogerty Work File 2017 (Autosaved).xlsx", 
                        sheet = "Data", startRow = 3, colNames = TRUE, detectDates = TRUE)[,1:43]

#dim(Fogerty_17)   # 162 rows , 43 Columns



# Columns to keep
Fogerty_17 <- Fogerty_17[,c(1:4,6,10, 9,14:20,22:23,39,41:43)]

names(Fogerty_17)

Fog_17_sum <- Fogerty_17 %>%
   group_by(Treatment, Chemical, `Row.Spacing.(m)`)%>%
   summarise(m_disease = mean(`2017-04-18`, na.rm = TRUE),
             sd_disease = sd(`2017-04-18`, na.rm = TRUE),
             m_Yield = mean(`Yield.t/ha`, na.rm = TRUE),
             sd_Yield = sd(`Yield.t/ha`, na.rm = TRUE))

write.csv(Fog_17_sum, "C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Fogerty_17_mean.csv")


# gather the incidence assessment into long format
Fogerty_17.1 <- gather(Fogerty_17, key = Incidence_assessment_date, value = Incidence, 
                        colnames(Fogerty_17)[c(11:16)])
#Fogerty_17.1$Severity <- gather(Fogerty_17, key = assessment_date, value = Severity, 
#                                colnames(Fogerty_17)[c(15,18)])
Fogerty_17.1$location <- "Fogerty"



# ____________________________________________________________
# _____________   Hermitage  2017     ____________
Herm_17 <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2017/Hermitage working file 2017 .xlsx", 
                        sheet = "Hermitage Trial", rows = 3:165, cols = 1:38, colNames = TRUE, detectDates = TRUE)

str(Herm_17)

# Adjust the Yield to 12% moisture
Herm_17$`Yield.T/Ha` <- (Herm_17$`Yield.T/Ha`/Herm_17$`Moisture%`)*12 

#dim(Herm_17)   # 162 rows , 43 Columns
#names(Herm_17)


# Create summarised data set for collated means meta-dataset
# NEXT STEP - sort and copy it to dataset
Herm_17_sum <- Herm_17 %>%
   group_by(Chemical, Chemical.Trt, `Row.Spacing.(m)`,Manage.Trt,Management) %>%
   summarise(PM_final_severity = mean(`I_11/05/2017`, na.rm = TRUE),
             Disease_error = sd(`I_11/05/2017`, na.rm = TRUE),
             `grain_yield(t/ha)` = mean(`Yield.T/Ha`, na.rm =TRUE),
             Yield_error = sd(`Yield.T/Ha`, na.rm =TRUE)
   )

Herm_17_sum <- with(Herm_17_sum,Herm_17_sum[order(Manage.Trt,Chemical, `Row.Spacing.(m)`),])

write.csv(Herm_17_sum, "./data/2017_Hermitage_summary.csv")





# Columns to keep
Herm_17 <- Herm_17[,c(1,2,4,5,7,10,9,11,12,43,
                      17,19,21,23,25,27,29,
#                      18,20,22,24,26,28,30,
                      34,36,37,35)]
# respectively
# plot and treatment variables
# Incidence assessment
# severity assessment
# Harvest assessment

Herm_17.1 <- reshape(data = Herm_17, idvar = "Plot", direction = "long",
        varying = list(incidence = names(Herm_17)[11:17]), 
#                       severity = names(Herm_17)[18:24]),
        timevar = "ass_date"
)

names(Herm_17.1)[16] <- c("Incidence")
Herm_17.1$ass_date <- rep(names(Herm_17)[11:17], each= 161) 
Herm_17.1$X43 <- NA
Herm_17.1$Total.Grain <- Herm_17.1$Total.Grain/1000
Herm_17.1$location <- "Hermitage"











# ______________________________________________________________________________________
# _____________   Wellcamp yield and disease assessment means         ____________

Well_18 <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/Wellcamp Fungicide x Row Spacing x Plant Population - Experimental Design Fogarty 2018.xlsx", 
                     sheet = "Data", startRow = 1, colNames = TRUE)

#dim(Well_18)   # 72 rows , 32 Columns
#names(Well_18)



# Columns to keep
Well_18 <- Well_18[,c(1,2,5,7,9,11,
                      15,17,19,21,23,25,
                      16,18,20,22,24,26,
                      30,32)]
# respectively
# plot and treatment variables
# Incidence assessment
# severity assessment
# Yield assessment

Well_18_inc <- reshape(data = Well_18, idvar = "Plot", direction = "long",
                     varying = list(incidence = names(Well_18)[7:12]), 
                     timevar = "ass_date_i"
)

Well_18_sev <- reshape(data = Well_18, idvar = "Plot", direction = "long",
                       varying = list(incidence = names(Well_18)[13:18]), 
                       timevar = "ass_date_s"
)


Well_18.1 <- cbind(Well_18_inc[,c(1:6,13:16)],Well_18_sev[,15:16])

names(Well_18.1)[10] <- c("Incidence")
names(Well_18.1)[12] <- c("Severity")
Well_18.1$ass_date_i <- rep(names(Well_18_inc)[7:12], each= 72) 
Well_18.1$ass_date_i <- as.Date(gsub("S.","", Well_18.1$ass_date_i), format = "%d-%m-%y")
names(Well_18.1)[9] <- c("Assessment_date")
Well_18.1 <- Well_18.1[c(1:10,12)]


Well_18.1$Row.Spacing <- as.numeric(unlist(strsplit(Well_18.1$Row.Spacing, " mtr")))


write.csv(Well_18.1, "C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2018_Wellcamp.csv")

Well_18_means <- Well_18.1 %>%
   group_by(Row.Spacing, Fungicide, Plant.Pop)%>%
   summarise(Yield = mean(`Yield./.ha`, na.rm = TRUE),
             Incidence  = max(Well_18.1$Incidence),
             Severity  = max(Severity, na.rm = TRUE))



write.csv(Well_18_means, "C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2018_Wellcamp means.csv")














# ________________________________________________________________
# _____________   Combining datasets              ________________


# ________________
#     2017
# ________________



# Function to check if each value in two vectors match.
# returns a data.frame of three columns, the two input vectors a column stateing if they match
col.ident <- function(x1, x2){
   vec1 <- vector()
   if(length(x1) != length(x2)){ cat("vector lengths don't match")
      break} else{
         for(i in 1:length(x1))
            vec1<- c(vec1,identical(x1[i],x2[i]))
      }
   data.frame(x1,x2,vec1)
}

# checking that each of the variables are the same as each other
dat2 <- col.ident(names(Herm_17.1),names(Fogerty_17.1))
dat2

# Variables are the same: now making the column names the same.
names(Herm_17.1) <- names(Fogerty_17.1)


# Binding both Hermitage data and Fogerty data together in a 2017 Powdery Mildew on Mungbean experiments
PM_MB_17 <- dplyr::bind_rows(Fogerty_17.1, Herm_17.1)


















# ________________
#     2016
# ________________


col.ident(names(Herm_16),names(King_16))

names(Herm_16)[4] <- names(King_16)[4]
names(King_16)[5] <- names(Herm_16)[5]
names(King_16)[6] <- names(Herm_16)[6]
names(King_16)[7] <- names(Herm_16)[7]  # I am assuming here that plot length and row length are the same thing

#PM_MB_16 <- dplyr::bind_rows(Herm_16,King_16)









# ________________________________________________________________
# _____________     Dataset of means              ________________
# ________________________________________________________________


# ___________________________________________________
# ______ Finding a summary of means for ___________






# data PM_MB_17 - powdery mildew data from two field trials in 2017, Hermitage and Fogerty
# data PM_MB_16 - powdery mildew data from two field trials in 2016, Hermitage and Kingaroy

dat3 <- PM_MB_17 %>%
   group_by(location, Treatment.No, Chem.Trt, `Row.Spacing.(m)`,Incidence_assessment_date) %>%
   summarise(Incidence = mean(Incidence), Yield = mean(`Yield.t/ha`)) %>%
   filter(location == "Fogerty") %>%
   filter(Incidence_assessment_date == "2017-04-18")

write.csv(dat3,"C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2017 Fogerty final mean incidence.csv")




# ___________________________________________________
# ______ And for Hermitage ___________


dat4 <- PM_MB_17 %>%
   group_by(location, Treatment.No, Chem.Trt, `Row.Spacing.(m)`,Incidence_assessment_date) %>%
   summarise(Incidence = mean(Incidence), Yield = mean(`Yield.t/ha`)) %>%
   filter(location == "Hermitage") %>%
   filter(Incidence_assessment_date == "2017-05-11")

write.csv(dat4,"C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2017 Hermitage final mean incidence.csv")



PM_MB_means <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/1902 powdery mildew-Mungbean - Collated means.xlsx", detectDates = TRUE)







# ___________________________________________________
# ______ And for Wellcamp 2018 ___________

head(Well_18.1)

dat5 <- Well_18.1 %>%
   group_by(factor(Row.Spacing), Fungicide, Assessment_date, Plant.Pop) %>%
   summarise(m.Incidence = mean(Incidence),
             sd.Incidence = sd(Incidence),
             Severity = mean(Severity, na.rm = T), 
             Yield = mean(`Yield./.ha`, na.rm = T),
             sd.Yield = mean(`Yield./.ha`, na.rm = T)) %>%
   filter(Assessment_date == "2018-05-02")

write.csv(dat5,"C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past trials/2017/2018 Wellcamp final mean incidence.csv")


# lattice::bwplot(Yield ~ `factor(Plant.Pop)`|Fungicide, data = dat4)
# lattice::bwplot(`Yield./.ha` ~ factor(Plant.Pop) | Fungicide, data = Well_18.1)
# 
 # lattice::bwplot(Severity ~ `factor(Plant.Pop)`|Fungicide, data = dat4)
 # lattice::bwplot(Severity ~ factor(Plant.Pop) | Fungicide, data = Well_18.1)
 # 
 # lattice::bwplot(Incidence ~ `factor(Plant.Pop)`|Fungicide, data = dat4)
 # lattice::bwplot(Incidence ~ factor(Plant.Pop) | Fungicide, data = Well_18.1)
 # 
 
 # 
# lattice::bwplot(Yield ~ `factor(Plant.Pop)`|`factor(Row.Spacing)`, data = dat4)
# lattice::bwplot(`Yield./.ha` ~ factor(Plant.Pop) | factor(Row.Spacing), data = Well_18.1)








PM_MB_means <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Documents/GitHub/Mungbean_PM/data/1902 powdery mildew-Mungbean - Collated means.xlsx", detectDates = TRUE)







Well_18.1







# ________________________________________________________________
# _____________         Weather data              ________________
# ________________________________________________________________



silo.API <- "YeTZsWlFNC8hvbQLllv8KtbRQWsje4qxic5QQ7mL"

# Functions
# n.date returns the Sys.date of the computer (today) in a date string which is in the format of YYYYMMDD
# @add.day parameter can be added to, add (eg. +7) or subtract (-7) from 

n.date <- function(add.day){
   if(missing(add.day)){
      as.numeric(gsub('-','',Sys.Date()))
   } else {
      as.numeric(gsub('-','',Sys.Date() + add.day))
   }
}









