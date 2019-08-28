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
# _____________                2013               ____________
# ____________________________________________________________

#_______
# Goolhi
#_______


# Import final disease survey
Goolhi.13 <- as.data.frame(read_xlsx(path = "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1305 Fungicides for powdery mildew in mungbean - Goolhi.xlsx",
          sheet = "33DAT1", range = "A15:AG75", col_names = TRUE))
head(Goolhi.13)

# Replace empty cells / NAs with zeros
for(i in 7:33){
   Goolhi.13[is.na(Goolhi.13[,i]),i] <- 0   
}


# Calculate incidence
#     Sites      1     2     3
# low canopy  c(7:9, 16:18, 25:27)
# mid canopy c(10:12, 19:21, 28:30)
# upp canopy c(13:15, 22:24, 31:33)

incidence1 <- vector(mode = "numeric",length = length(Goolhi.13[,1]))

# Convert disease survey data to 1-9 Incidence rating scale
for(i in seq_along(incidence1)){

   # incidence = 1
   if(all(Goolhi.13[i,7:33] == 0)){
      incidence1[i] <- 1
      next()
   }

   # incidence = 2
   if(all(Goolhi.13[i,c(10:15,19:24,28,33)] == 0) &&
      (sum(Goolhi.13[i,c(7:9,16:18,25:27)] > 0)/
       length(Goolhi.13[i,c(7:9,16:18,25:27)])) <= 0.75){
      incidence1[i] <- 2
      next()
   }
   

   # incidence = 3
   if(all(Goolhi.13[i,c(13:15, 22:24, 31:33)] == 0) &&
      sum(Goolhi.13[i,c(10:12, 19:21, 28:30)] != 0) <= 3 &&
      (sum(Goolhi.13[i,c(7:12,16:21,25:30)] > 0)/
       length(Goolhi.13[i,c(7:12,16:21,25:30)])) > 0.75){
      incidence1[i] <- 3
   }else{ # if it is in the lower half of the canopy and in less than 75% of plants then incidence = 2.5
      if(all(Goolhi.13[i,c(13:15, 22:24, 31:33)] == 0) &&
            sum(Goolhi.13[i,c(10:12, 19:21, 28:30)] != 0) <= 3 &&
            (sum(Goolhi.13[i,c(7:12,16:21,25:30)] > 0)/
             length(Goolhi.13[i,c(7:12,16:21,25:30)])) <= 0.75){
      incidence1[i] <- 2.5
      next()
      }
   }
   
   
   # incidence = 4
   if(all(Goolhi.13[i,c(13:15, 22:24, 31:33)] == 0) &&
      sum(Goolhi.13[i,c(10:12, 19:21, 28:30)] != 0) > 3 &&
      (sum(Goolhi.13[i,c(7:12,16:21,25:30)] > 0)/
       length(Goolhi.13[i,c(7:12,16:21,25:30)])) <= 0.75){
      incidence1[i] <- 4
      next()
   }
   

   # incidence = 5
   if(all(Goolhi.13[i,c(13:15, 22:24, 31:33)] == 0) &&
      sum(Goolhi.13[i,c(10:12, 19:21, 28:30)] != 0) > 3 &&
      (sum(Goolhi.13[i,c(7:12,16:21,25:30)] > 0)/
       length(Goolhi.13[i,c(7:12,16:21,25:30)])) > 0.75 &&
      any(Goolhi.13[i,c(7:12,16:21,25:30)] == 0)){
      incidence1[i] <- 5
      next()
   }
   

   # incidence = 6
   if(all(Goolhi.13[i,c(13:15, 22:24, 31:33)] == 0) &&
      all(Goolhi.13[i,c(7:12,16:21,25:30)] != 0)){
      incidence1[i] <- 6
      next()
   }


   # incidence = 7
   if(sum(Goolhi.13[i,c(13:15, 22:24, 31:33)] != 0) <= 3 &&
      all(Goolhi.13[i,c(7:12,16:21,25:30)] != 0)){
      incidence1[i] <- 7
      next()
   }
   

   # incidence = 8
   if((sum(Goolhi.13[i,c(13:15, 22:24, 31:33)] != 0) > 3 |
      sum(Goolhi.13[i,c(13:15, 22:24, 31:33)]) <= 200) &&
      all(Goolhi.13[i,c(7:12,16:21,25:30)] != 0) &&
      (sum(Goolhi.13[i,7:33] > 0)/
          length(Goolhi.13[i,7:33]) > 0.75) &&
      sum(Goolhi.13[i,7:33]) <= 2000){
      
      incidence1[i] <- 8
      next()
   }


   # incidence = 9
   if(sum(Goolhi.13[i,c(13:15, 22:24, 31:33)] != 0) > 3 &&
      all(Goolhi.13[i,c(7:12,16:21,25:30)] != 0) &&
      (sum(Goolhi.13[i,7:33] > 0)/
       length(Goolhi.13[i,7:33]) > 0.75) &&
      sum(Goolhi.13[i,7:33]) > 2000){
      
      incidence1[i] <- 9
      next()
   }
   
   # if not in the lower canopy but small colonies in mid and upper canopy
   if(all(Goolhi.13[i,c(7:9,16:18,25:27)] == 0) &&
      (sum(Goolhi.13[i,c(10:15,19:24,28,33)] > 0)/
       length(Goolhi.13[i,7:33]) <= 0.5)){
      
      incidence1[i] <- 2.5
      next()
   }
   
   
   # incidence = 7.5
   if(sum(Goolhi.13[i,c(13:15, 22:24, 31:33)] != 0) > 3 &&
      sum(Goolhi.13[i,c(7:12,16:21,25:30)] != 0) > 10 &&
      any(Goolhi.13[i,c(7:12,16:21,25:30)] == 0) &&
      (sum(Goolhi.13[i,7:33] > 0)/
       length(Goolhi.13[i,7:33]) > 0.7) &&
      sum(Goolhi.13[i,7:33]) <= 2000){
      incidence1[i] <- 7.5
      next()
   }
   
   # incidence = 6.5
   if(sum(Goolhi.13[i,c(13:15, 22:24, 31:33)] != 0) > 3 &&
      sum(Goolhi.13[i,c(7:12,16:21,25:30)] != 0) <= 12 &&
      sum(Goolhi.13[i,c(7:12,16:21,25:30)] != 0) >= 10 &&
      (sum(Goolhi.13[i,7:33] > 0)/
       length(Goolhi.13[i,7:33]) <= 0.75) &&
      sum(Goolhi.13[i,7:33]) <= 2000){
      incidence1[i] <- 6.5
      next()
   }
   
   
   # incidence = 4.5
   if(sum(Goolhi.13[i,c(13:15, 22:24, 31:33)] != 0) > 3 &&
      sum(Goolhi.13[i,c(7:12,16:21,25:30)] != 0) < 10 &&
      sum(Goolhi.13[i,c(7:12,16:21,25:30)] != 0) > 5 &&
      (sum(Goolhi.13[i,7:33] > 0)/
       length(Goolhi.13[i,7:33]) <= 0.75) &&
      sum(Goolhi.13[i,7:33]) <= 2000){
      incidence1[i] <- 4.5
      next()
   }else{
      # If none of the conditions are met give the incidence zero
      incidence1[i] <- 0
   }
   
   }

# check data to make sure there are no zeros
incidence1
length(Goolhi.13[incidence1 == 0,1])
Goolhi.13[incidence1 == 0,]
hist(incidence1)

# Re-format data-frame
Goolhi.13$Incidence <- incidence1
Goolhi.13D <- Goolhi.13[,c("Treat", "Rep", "Run", "Plot", "Incidence")]


 Goolhi.13D <- Goolhi.13D %>%
   group_by(Treat) %>%
   summarise(M.inc = mean(Incidence, na.rm = TRUE),
             sd.inc = sd(Incidence, na.rm = TRUE))
write.csv(Goolhi.13D, "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1305-Goolhi-Disease_means.csv")







# Read in Harvest data
Goolhi.13Y <- as.data.frame(read_xlsx(path = "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1305 Fungicides for powdery mildew in mungbean - Goolhi.xlsx",
                                     sheet = "Yield", range = "A15:F79", col_names = TRUE))
head(Goolhi.13Y)

Goolhi.13Y <- Goolhi.13Y %>%
   group_by(Treat) %>%
   summarise(M.Yield = mean(`Yield Kg/ha`, na.rm = TRUE),
             sd.Yield = sd(`Yield Kg/ha`, na.rm = TRUE))
write.csv(Goolhi.13Y, "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1305-Goolhi-Yield_means.csv")






#____________
# Marys mount
#_____________


# Import final disease survey
MarysM.13D <- as.data.frame(read_xlsx(path = "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1304 Fungicides for powdery mildew in mungbean - Marys Mount.xlsx",
                                     sheet = "19 DAT1 19-3-2013", range = "A14:DC72", col_names = TRUE))
head(MarysM.13D)
dim(MarysM.13D)

# Replace empty cells / NAs with zeros
for(i in 7:107){
   MarysM.13D[is.na(MarysM.13D[,i]),i] <- 0   
}


incidence2 <- vector(mode = "numeric",length = length(MarysM.13D[,1]))


# Convert disease survey data to 1-9 Incidence rating scale
for(i in seq_along(incidence2)){
   
   # incidence = 1
   if(all(MarysM.13D[i,58:107] == 0)){
      incidence2[i] <- 1
      next()
   }
   
   # incidence = 2
   if(sum(MarysM.13D[i,58:107] == 2) == 0 &&
      sum(MarysM.13D[i,58:107] == 1) == 0 &&
      (sum(MarysM.13D[i,58:107] == 3)/
       length(MarysM.13D[i,58:107])) <= 0.75){
      incidence2[i] <- 2
      next()
   }
   
   
   # incidence = 3
   if(sum(MarysM.13D[i,58:107] == 1) == 0 &&
      sum(MarysM.13D[i,58:107] == 2) <= 8 &&
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107])) > 0.75){
      incidence2[i] <- 3
   }else{ # if it is in the lower half of the canopy and in less than 75% of plants then incidence = 2.5
      if(sum(MarysM.13D[i,58:107] == 1) == 0 &&
         sum(MarysM.13D[i,58:107] == 2) <= 8 &&
         sum(MarysM.13D[i,58:107] == 3) > 0 &&
         (sum(MarysM.13D[i,58:107] > 0)/
          length(MarysM.13D[i,58:107])) <= 0.75){
         incidence2[i] <- 2.5
         next()
      }
   }
   
   
   # incidence = 4
   if(sum(MarysM.13D[i,58:107] == 1) == 0 &&
      sum(MarysM.13D[i,58:107] == 2) > 0 &&
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107])) <= 0.75){
      incidence2[i] <- 4
      next()
   }
   
   
   # incidence = 5
   if(sum(MarysM.13D[i,58:107] == 1) == 0 &&
      sum(MarysM.13D[i,58:107] == 2) > 0 &&
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107])) > 0.75){
      incidence2[i] <- 5
      next()
   }
   
   
   # incidence = 6
   if(sum(MarysM.13D[i,58:107] == 1) == 0 &&
      sum(MarysM.13D[i,58:107] >= 2) > 0 &&
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107])) > 0.95){
      incidence2[i] <- 6
      next()
   }
   
   
   # incidence = 7
   if(sum(MarysM.13D[i,58:107] == 1) <= 8 &&
      sum(MarysM.13D[i,58:107] >= 2) > 0 &&
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107])) > 0.95){
      incidence2[i] <- 7
      next()
   }
   
   
   # incidence = 8
   if(sum(MarysM.13D[i,58:107] >= 1) > 0 &&
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107])) > 0.75){
      incidence2[i] <- 8
      next()
   }
   
   
   # incidence = 9
   if(sum(MarysM.13D[i,58:107] >= 1) > 0 &&
      sum(MarysM.13D[i,7:56] >= (10*5*100*0.75)) &&  # ten leaves from 5 sample sites with a maximum % leaf area infected with PM of 100% * 0.75 as a threshold for leaf drop
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107])) > 0.75){      
      incidence2[i] <- 9
      next()
   }
   
   # if there are few (< 10%) leaves with very small infections in all parts of the canopy OR
   # if there is a stray colony on a upper leaf in addition to all lower 
    if(sum(MarysM.13D[i,58:107] == 3) >= 1 &&
       sum(MarysM.13D[i,58:107] == 1) <= 2 &&
       sum(MarysM.13D[i,58:107] == 2) <= 2 &&
       sum(MarysM.13D[i,7:56]) <= 150){
       incidence2[i] <- 2.5
       next()
    }
   
    # incidence = 7.5  In the upper canopy with less than 75% plants infected
   if(sum(MarysM.13D[i,58:107] == 3 ) > 0 &&
      sum(MarysM.13D[i,58:107] == 2 ) > 0 &&
      sum(MarysM.13D[i,58:107] == 1 ) > 0 &&
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107]) <= 0.75) &&
      sum(MarysM.13D[i,7:56]) >= 2000){
      incidence2[i] <- 7.5
      next()
   }
   # 
    # incidence = 6.5
    if(sum(MarysM.13D[i,58:107] == 1) > 3 &&
      sum(MarysM.13D[i,58:107] >= 2) > 0 &&
      sum(MarysM.13D[i,7:56] <= 500)  &&
       (sum(MarysM.13D[i,58:107] > 0)/
        length(MarysM.13D[i,58:107]) >= 0.65)){
       incidence2[i] <- 6.5
       next()
    }
   # 
   # 
   # incidence = 4.5 If PM is in the the lower and mid-canopy in less than 75% of plants and one or two in the upper canopy
   if(sum(MarysM.13D[i,58:107] == 1) <= 3 &&
      sum(MarysM.13D[i,58:107] >= 2) > 0 &&
      (sum(MarysM.13D[i,58:107] > 0)/
       length(MarysM.13D[i,58:107])) <= 0.75){
      incidence2[i] <- 4.5
      next()
   }else{
      # If none of the conditions are met give the incidence zero
      incidence2[i] <- 0}
   
   # }else{
   #    # If none of the conditions are met give the incidence zero
   #    incidence2[i] <- 0
   # }
   
}


# Check for Zeros which indicat the above code could not fit the data into an incidence rating on the 1-9 scale
incidence2
MarysM.13D[incidence2 == 0,1:5]
dim(MarysM.13D[incidence2 == 0,])
hist(incidence2)


# Re-format data-frame
MarysM.13D$Incidence <- incidence2
MarysM.13D <- MarysM.13D[,c("Treat", "Rep", "Run", "Plot", "Incidence")]


MarysM.13D <- MarysM.13D %>%
   group_by(Treat) %>%
   summarise(M.inc = mean(Incidence, na.rm = TRUE),
             sd.inc = sd(Incidence, na.rm = TRUE))
write.csv(MarysM.13D, "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1304-MaryMount-Disease_means.csv")



# read in yield and format
MarysM.13Y <- as.data.frame(read_xlsx(path = "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1304 Fungicides for powdery mildew in mungbean - Marys Mount.xlsx",
                                      sheet = "42 DAT1 Yield", range = "A14:F58", col_names = TRUE))
head(MarysM.13Y)

MarysM.13Y <- MarysM.13Y %>%
   group_by(Treat) %>%
   summarise(M.Yield = mean(`Yield Kg/ha`, na.rm = TRUE),
             sd.Yield = sd(`Yield Kg/ha`, na.rm = TRUE))
write.csv(MarysM.13Y, "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1304-MarysMount-Yield_means.csv")










#____________
# Premer
#_____________


# Import final disease survey
Premer.13D <- as.data.frame(read_xlsx(path = "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1303 Fungicides for powdery mildew in mungbean - Premer.xlsx",
                                      sheet = "31 DAT1 31-3-2013", range = "A14:DC52", col_names = TRUE))
head(Premer.13D)
dim(Premer.13D)

# remove Plots not surveyed
for(i in seq_along(Premer.13D[,1])){
   if(i == 1){plotsRecorded <- vector(length = length(Premer.13D[,1]))}
   
   plotsRecorded[i] <- all(is.na(Premer.13D[i,7:56]))
   
   if(i == length(Premer.13D[,1])){print(!plotsRecorded)}
}
# remove Plots not surveyed
Premer.13D <- Premer.13D[!plotsRecorded,]

incidence3 <- vector(mode = "numeric",length = length(Premer.13D[,1]))


# Convert disease survey data to 1-9 Incidence rating scale
for(i in seq_along(incidence3)){
   
   # incidence = 1
   if(all(Premer.13D[i,58:107] == 0)){
      incidence3[i] <- 1
      next()
   }
   
   # incidence = 2
   if(sum(Premer.13D[i,58:107] == 2) == 0 &&
      sum(Premer.13D[i,58:107] == 1) == 0 &&
      (sum(Premer.13D[i,58:107] == 3)/
       length(Premer.13D[i,58:107])) <= 0.75){
      incidence3[i] <- 2
      next()
   }
   
   
   # incidence = 3
   if(sum(Premer.13D[i,58:107] == 1) == 0 &&
      sum(Premer.13D[i,58:107] == 2) <= 8 &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107])) > 0.75){
      incidence3[i] <- 3
   }else{ # if it is in the lower half of the canopy and in less than 75% of plants then incidence = 2.5
      if(sum(Premer.13D[i,58:107] == 1) == 0 &&
         sum(Premer.13D[i,58:107] == 2) <= 8 &&
         sum(Premer.13D[i,58:107] == 3) > 0 &&
         (sum(Premer.13D[i,58:107] > 0)/
          length(Premer.13D[i,58:107])) <= 0.75){
         incidence3[i] <- 2.5
         next()
      }
   }
   
   
   # incidence = 4
   if(sum(Premer.13D[i,58:107] == 1) == 0 &&
      sum(Premer.13D[i,58:107] == 2) > 0 &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107])) <= 0.75){
      incidence3[i] <- 4
      next()
   }
   
   
   # incidence = 5
   if(sum(Premer.13D[i,58:107] == 1) == 0 &&
      sum(Premer.13D[i,58:107] == 2) > 0 &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107])) > 0.75){
      incidence3[i] <- 5
      next()
   }
   
   
   # incidence = 6
   if(sum(Premer.13D[i,58:107] == 1) == 0 &&
      sum(Premer.13D[i,58:107] >= 2) > 0 &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107])) > 0.95){
      incidence3[i] <- 6
      next()
   }
   
   
   # incidence = 7
   if(sum(Premer.13D[i,58:107] == 1) <= 8 &&
      sum(Premer.13D[i,58:107] >= 2) > 0 &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107])) > 0.95){
      incidence3[i] <- 7
      next()
   }
   
   
   # incidence = 8
   if(sum(Premer.13D[i,58:107] >= 1) > 0 &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107])) > 0.75){
      incidence3[i] <- 8
      next()
   }
   
   
   # incidence = 9
   if(sum(Premer.13D[i,58:107] >= 1) > 0 &&
      sum(Premer.13D[i,7:56] >= (10*5*100*0.75)) &&  # ten leaves from 5 sample sites with a maximum % leaf area infected with PM of 100% * 0.75 as a threshold for leaf drop
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107])) > 0.75){      
      incidence3[i] <- 9
      next()
   }
   
   # if there are few (< 10%) leaves with very small infections in all parts of the canopy OR
   # if there is a stray colony on a upper leaf in addition to all lower 
   if(sum(Premer.13D[i,58:107] == 3) >= 1 &&
      sum(Premer.13D[i,58:107] == 1) <= 2 &&
      sum(Premer.13D[i,58:107] == 2) <= 2 &&
      sum(Premer.13D[i,7:56]) <= 150){
      incidence3[i] <- 2.5
      next()
   }
   
   # incidence = 7.5  In the upper canopy with less than 75% plants infected
   if(sum(Premer.13D[i,58:107] == 3 ) > 0 &&
      sum(Premer.13D[i,58:107] == 2 ) > 0 &&
      sum(Premer.13D[i,58:107] == 1 ) > 0 &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107]) <= 0.75) &&
      sum(Premer.13D[i,7:56]) >= 2000){
      incidence3[i] <- 7.5
      next()
   }
   # 
   # incidence = 6.5
   if(sum(Premer.13D[i,58:107] == 1) > 3 &&
      sum(Premer.13D[i,58:107] >= 2) > 0 &&
      sum(Premer.13D[i,7:56] <= 500)  &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107]) >= 0.65)){
      incidence3[i] <- 6.5
      next()
   }
   # 
   # 
   # incidence = 4.5 If PM is in the the lower and mid-canopy in less than 75% of plants and one or two in the upper canopy
   if(sum(Premer.13D[i,58:107] == 1) <= 3 &&
      sum(Premer.13D[i,58:107] >= 2) > 0 &&
      (sum(Premer.13D[i,58:107] > 0)/
       length(Premer.13D[i,58:107])) <= 0.75){
      incidence3[i] <- 4.5
      next()
   }else{
      # If none of the conditions are met give the incidence zero
      incidence3[i] <- 0}
   
   # }else{
   #    # If none of the conditions are met give the incidence zero
   #    incidence3[i] <- 0
   # }
   
}

incidence2
MarysM.13D[incidence2 == 0,1:5]
dim(MarysM.13D[incidence2 == 0,])
hist(incidence2)

incidence3
Premer.13D[incidence3 == 0,1:5]
dim(Premer.13D[incidence3 == 0,])
hist(incidence3)


# Re-format data-frame
Premer.13D$Incidence <- incidence3
Premer.13D <- Premer.13D[,c("Treat", "Rep", "Run", "Plot", "Incidence")]


Premer.13D <- Premer.13D %>%
   group_by(Treat) %>%
   summarise(M.inc = mean(Incidence, na.rm = TRUE),
             sd.inc = sd(Incidence, na.rm = TRUE))
write.csv(Premer.13D, "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1303-Premer-Disease_means.csv")




# read in yield and format
Premer.13Y <- as.data.frame(read_xlsx(path = "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1303 Fungicides for powdery mildew in mungbean - Premer.xlsx",
                                      sheet = "Yield 18-4-2013", range = "A14:F58", col_names = TRUE))
head(Premer.13Y)

Premer.13Y <- Premer.13Y %>%
   group_by(Treat) %>%
   summarise(M.Yield = mean(`Yield kg/ha`, na.rm = TRUE),
             sd.Yield = sd(`Yield kg/ha`, na.rm = TRUE))
write.csv(Premer.13Y, "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/AM1304-Premer-Yield_means.csv")






#____________
# Millmerran
#_____________


# Import final disease survey
Millm.13D <- as.data.frame(read_xlsx(path = "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/Past trials/2013/BB1305 Fungicides for powdery mildew in mungbean - Millmerran.xlsx",
                                      sheet = "Assessment Sheet 24.4.2013", range = "A12:AH56", col_names = TRUE))
head(Millm.13D)
dim(Millm.13D)


sum(is.na(Millm.13D[,5:34]))
# replace NAs with zero Plots not surveyed
for(i in 5:34){
   for(j in seq_along(Millm.13D[,i])){
      if(is.na(Millm.13D[j,i])){
         Millm.13D[j,i] <- 0
      }
   }
}



incidence4 <- vector(mode = "numeric",length = length(Millm.13D[,1]))

# Calculate incidence
#     Sites                
# low canopy  c(5:13)
# mid canopy c(15:23)
# upp canopy c(25:33)


# Convert disease survey data to 1-9 Incidence rating scale
for(i in seq_along(incidence4)){
   
   # incidence = 1
   if(all(Millm.13D[i,c(5:13,15:23,25:33)] == 0)){
      incidence4[i] <- 1
      next()
   }
   
   # incidence = 2
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      all(Millm.13D[i,15:23] == 0) &&
      all(Millm.13D[i,25:33] == 0) &&
      (sum(Millm.13D[i,c(5:13)] > 0)/
       length(Millm.13D[i,c(5:13)])) <= 0.75){
      incidence4[i] <- 2
      next()
   }
   
   
   # incidence = 3
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      sum(Millm.13D[i,15:23] != 0) <= 3 &&
      sum(Millm.13D[i,25:33] == 0) == 0 &&
      (sum(Millm.13D[i,c(5:13,15:23)] > 0)/
       length(Millm.13D[i,c(5:13,15:23)])) >= 0.75){
      incidence4[i] <- 3
      }else{ # if it is in the lower half of the canopy and in less than 75% of plants then incidence = 2.5
         if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
            sum(Millm.13D[i,15:23] != 0) <= 3 &&
            sum(Millm.13D[i,25:33] == 0) == 0 &&
            (sum(Millm.13D[i,c(5:13,15:23)] > 0)/
             length(Millm.13D[i,c(5:13,15:23)])) <= 0.75){
         incidence4[i] <- 2.5
         next()
      }
   }
   
   
   # incidence = 4
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      sum(Millm.13D[i,15:23] != 0) >= 1 &&
      all(Millm.13D[i,25:33] == 0) &&
      (sum(Millm.13D[i,c(5:13,15:23)] > 0)/
       length(Millm.13D[i,c(5:13,15:23)])) <= 0.75){
      incidence4[i] <- 4
      next()
   }
   
   
   # incidence = 5
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      sum(Millm.13D[i,15:23] != 0) >= 1 &&
      all(Millm.13D[i,25:33] == 0) &&
      (sum(Millm.13D[i,c(5:13,15:23)] > 0)/
       length(Millm.13D[i,c(5:13,15:23)])) >= 0.75){
      incidence4[i] <- 5
      next()
   }
   
   
   # incidence = 6
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      sum(Millm.13D[i,15:23] != 0) >= 1 &&
      all(Millm.13D[i,25:33] == 0) &&
      (sum(Millm.13D[i,c(5:13,15:23)] > 0)/
       length(Millm.13D[i,c(5:13,15:23)])) >= 0.90){
      incidence4[i] <- 6
      next()
   }
   
   
   # incidence = 7
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      sum(Millm.13D[i,15:23] != 0) >= 1 &&
      sum(Millm.13D[i,25:33] != 0) <= 3 &&
      (sum(Millm.13D[i,c(5:13,15:23)] > 0)/
       length(Millm.13D[i,c(5:13,15:23)])) >= 0.90){
      incidence4[i] <- 7
      next()
   }
   
   
   # incidence = 8
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      sum(Millm.13D[i,15:23] != 0) >= 1 &&
      sum(Millm.13D[i,25:33] != 0) >= 1 &&
      (sum(Millm.13D[i,c(5:13,15:23,25:33)] > 0)/
       length(Millm.13D[i,c(5:13,15:23,25:33)])) >= 0.75){
      incidence4[i] <- 8
      next()
   }
   
   
   # incidence = 9
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      sum(Millm.13D[i,15:23] != 0) >= 1 &&
      sum(Millm.13D[i,25:33] != 0) >= 1 &&
      (sum(Millm.13D[i,c(5:13,15:23,25:33)] > 0)/
       length(Millm.13D[i,c(5:13,15:23,25:33)])) >= 0.75 &&
      sum(Millm.13D[i,c(5:13,15:23,25:33)]) > (9*3*100)*0.75){
      incidence4[i] <- 9
      next()
   }
   
   
   # if there are few (< 10%) leaves with very small infections in all parts of the canopy OR
   # if there is a stray colony on a upper leaf in addition to all lower 

   if(sum(Millm.13D[i,15:23] != 0) <= 3 &&
      sum(Millm.13D[i,25:33] != 0) <= 3 &&
      (sum(Millm.13D[i,c(5:13,15:23,25:33)])/
       length(Millm.13D[i,c(5:13,15:23,25:33)])/100) <= 0.1){
      
      incidence4[i] <- 2.5
      next()
   }
   
   # incidence = 7.5  In the upper canopy with less than 75% plants infected
   if(sum(Millm.13D[i,5:13] != 0) >= 1 &&
      sum(Millm.13D[i,15:23] != 0) >= 1 &&
      sum(Millm.13D[i,25:33] != 0) >= 3 &&
      (sum(Millm.13D[i,c(5:13,15:23,25:33)] > 0)/
       length(Millm.13D[i,c(5:13,15:23,25:33)])) <= 0.75){
      incidence4[i] <- 7.5
      next()
   }
   
   # if there are few more colonies(< 10%) leaves with very small infections in all parts of the canopy OR
   # if there is a stray colony on a upper leaf in addition to all lower 
   
   if(sum(Millm.13D[i,15:23] != 0) <= 6 &&
      sum(Millm.13D[i,25:33] != 0) <= 3 &&
      (sum(Millm.13D[i,c(5:13,15:23,25:33)])/
       length(Millm.13D[i,c(5:13,15:23,25:33)])/100) <= 0.1){
      
      incidence4[i] <- 3.5
      next()
   }
   
   if(sum(Millm.13D[i,15:23] != 0) >= 1 &&
      sum(Millm.13D[i,25:33] != 0) <= 3 &&
      (sum(Millm.13D[i,c(5:13,15:23,25:33)])/
       length(Millm.13D[i,c(5:13,15:23,25:33)])/100) <= 0.5){
      
      incidence4[i] <- 6.5
      next()
   }
   else{
      # If none of the conditions are met give the incidence zero
      incidence4[i] <- 0
   }
}
   # if there are few (< 10%) leaves with very small infections in all parts of the canopy OR
   # if there is a stray colony on a upper leaf in addition to all lower 
   
   # if(all(Millm.13D[i,c(7:9,16:18,25:27)] == 0) &&
   #    (sum(Millm.13D[i,c(10:15,19:24,28,33)] > 0)/
   #     length(Millm.13D[i,7:33]) <= 0.5)){
   #    
   #    incidence4[i] <- 2.5
   #    next()
   # }
   # 
   # 
   # # incidence = 7.5
   # if(sum(Millm.13D[i,c(13:15, 22:24, 31:33)] != 0) > 3 &&
   #    sum(Millm.13D[i,c(7:12,16:21,25:30)] != 0) > 10 &&
   #    any(Millm.13D[i,c(7:12,16:21,25:30)] == 0) &&
   #    (sum(Millm.13D[i,7:33] > 0)/
   #     length(Millm.13D[i,7:33]) > 0.7) &&
   #    sum(Millm.13D[i,7:33]) <= 2000){
   #    incidence4[i] <- 7.5
   #    next()
   # }
   # 
   # # incidence = 6.5
   # if(sum(Millm.13D[i,c(13:15, 22:24, 31:33)] != 0) > 3 &&
   #    sum(Millm.13D[i,c(7:12,16:21,25:30)] != 0) <= 12 &&
   #    sum(Millm.13D[i,c(7:12,16:21,25:30)] != 0) >= 10 &&
   #    (sum(Millm.13D[i,7:33] > 0)/
   #     length(Millm.13D[i,7:33]) <= 0.75) &&
   #    sum(Millm.13D[i,7:33]) <= 2000){
   #    incidence4[i] <- 6.5
   #    next()
   # }
   # 
   # 
   # # incidence = 4.5
   # if(sum(Millm.13D[i,c(13:15, 22:24, 31:33)] != 0) > 3 &&
   #    sum(Millm.13D[i,c(7:12,16:21,25:30)] != 0) < 10 &&
   #    sum(Millm.13D[i,c(7:12,16:21,25:30)] != 0) > 5 &&
   #    (sum(Millm.13D[i,7:33] > 0)/
   #     length(Millm.13D[i,7:33]) <= 0.75) &&
   #    sum(Millm.13D[i,7:33]) <= 2000){
   #    incidence4[i] <- 4.5
   #    next()
   #  }else{
   #    # If none of the conditions are met give the incidence zero
   #    incidence4[i] <- 0
   # }
   



incidence4
dim(Millm.13D[incidence4 == 0,])
Millm.13D[incidence4 == 0,1:7]
hist(incidence4)








# ____________________________________________________________
# _____________            Hermitage  2015        ____________

# Yeild data from 2016 Hermitage trial with final disease severity
Herm_15 <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2015 PMmung Herm/Mugbean powdery mildew 2014-15 inc results v2.xlsx", 
                     sheet = "HRS yield data", startRow = 4, colNames = TRUE, detectDates = TRUE)[1:31,1:9]



Herm_15$Year <- 2015
Herm_15$location <- "Hermitage"

# Disease incidence assessments through the 2016 season
Herm_15.DR  <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2015 PMmung Herm/Mugbean powdery mildew 2014-15 inc results v2.xlsx", 
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
Herm_17 <- Herm_17[,c(1,2,4,5,7,10,9,11,12,13,
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

# names(Herm_17.1)[16] <- c("Incidence")
# Herm_17.1$ass_date <- rep(names(Herm_17)[11:16], each= 161) 
# Herm_17.1$X43 <- NA
# Herm_17.1$Total.Grain <- Herm_17.1$Total.Grain/1000
# Herm_17.1$location <- "Hermitage"











# ______________________________________________________________________________________
# _____________   Wellcamp yield and disease assessment means         ____________

Well_18 <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/Past Trials/2018/Wellcamp Fungicide x Row Spacing x Plant Population - Experimental Design Fogarty 2018.xlsx", 
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
# dat2 <- col.ident(names(Herm_17.1),names(Fogerty_17.1))
# dat2

# Variables are the same: now making the column names the same.
# names(Herm_17.1) <- names(Fogerty_17.1)


# Binding both Hermitage data and Fogerty data together in a 2017 Powdery Mildew on Mungbean experiments
# PM_MB_17 <- dplyr::bind_rows(Fogerty_17.1, Herm_17.1)


















# ________________
#     2016
# ________________


# col.ident(names(Herm_16),names(King_16))
# 
# names(Herm_16)[4] <- names(King_16)[4]
# names(King_16)[5] <- names(Herm_16)[5]
# names(King_16)[6] <- names(Herm_16)[6]
# names(King_16)[7] <- names(Herm_16)[7]  # I am assuming here that plot length and row length are the same thing

#PM_MB_16 <- dplyr::bind_rows(Herm_16,King_16)









# ________________________________________________________________
# _____________     Dataset of means              ________________
# ________________________________________________________________


# ___________________________________________________
# ______ Finding a summary of means for ___________






# data PM_MB_17 - powdery mildew data from two field trials in 2017, Hermitage and Fogerty
# data PM_MB_16 - powdery mildew data from two field trials in 2016, Hermitage and Kingaroy

# dat3 <- PM_MB_17 %>%
#    group_by(location, Treatment.No, Chem.Trt, `Row.Spacing.(m)`,Incidence_assessment_date) %>%
#    summarise(Incidence = mean(Incidence), Yield = mean(`Yield.t/ha`)) %>%
#    filter(location == "Fogerty") %>%
#    filter(Incidence_assessment_date == "2017-04-18")
# 
# write.csv(dat3,"C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2017 Fogerty final mean incidence.csv")
# 



# ___________________________________________________
# ______ And for Hermitage ___________


# dat4 <- PM_MB_17 %>%
#    group_by(location, Treatment.No, Chem.Trt, `Row.Spacing.(m)`,Incidence_assessment_date) %>%
#    summarise(Incidence = mean(Incidence), Yield = mean(`Yield.t/ha`)) %>%
#    filter(location == "Hermitage") %>%
#    filter(Incidence_assessment_date == "2017-05-11")
# 
# write.csv(dat4,"C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/2017 Hermitage final mean incidence.csv")



# PM_MB_means <- read.xlsx("C:/Users/U8011054/OneDrive - USQ/Cloudstor/Mungbean/1902 powdery mildew-Mungbean - Collated means.xlsx", detectDates = TRUE)







# ___________________________________________________
# ______ And for Wellcamp 2018 ___________

# head(Well_18.1)

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





# ________________________________________________________________
# _____________         Hermitage 2019           ________________
# ________________________________________________________________
library(data.table)


PM_MB_19 <- read.xlsx("C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/1901_Herm_PM/1901 Mungbean powdery mildew disease and yeild data.xlsx",
                     startRow = 2,
                     cols = c(1:21))

PM_MB_19 <- subset(PM_MB_19, Harvest.Plot.Number != 5)
PM_MB_19 <- subset(PM_MB_19, Harvest.Plot.Number != 6)



PM19_means <- PM_MB_19 %>%
   group_by(TOS.NUM, `T/ment.NUM`)%>%
   summarise(Incidence190423 = mean(`23-4-19.Incedence`, na.rm = TRUE),
             Inc.sd.190423 = sd(`23-4-19.Incedence`, na.rm = TRUE),
             Inc.error.190423 = "Stdev",
             Incidence190501 = mean(`1-5-19.Incedence`, na.rm = TRUE),
             Inc.sd.190501 = sd(`1-5-19.Incedence`, na.rm = TRUE),
             Inc.error.190501 = "Stdev",
             Plot_length = mean(Plot.Lengths, na.rm = TRUE),
             Yield = mean(Yeild, na.rm = TRUE),
             Yield_error = sd(Yeild, na.rm = TRUE),
             AUDPC = mean(AUDPC, na.rm = TRUE)
   )

library(agricolae)
?audpc
colnames(PM_MB_19)

audpc(PM_MB_19[PM_MB_19$TOS.NUM == 2 ,c(5,7,9)],c(70,78,86))

PM_MB_19$AUDPC <- NA
PM_MB_19[PM_MB_19$TOS.NUM == 1 , "AUDPC"] <- audpc(PM_MB_19[PM_MB_19$TOS.NUM == 1 ,c(5,7)],c(70,78))
PM_MB_19[PM_MB_19$TOS.NUM == 2 , "AUDPC"] <- audpc(PM_MB_19[PM_MB_19$TOS.NUM == 2 ,c(5,7,9)],c(70,78,86))

boxplot(AUDPC ~ as.factor(`T/ment.NUM`)* as.factor(TOS.NUM) , data = PM_MB_19)


as.Date("2019-05-22") - as.Date("2019-02-04")
as.Date("2019-04-15") - as.Date("2019-02-04")
as.Date("2019-04-23") - as.Date("2019-02-04")
as.Date("2019-05-01") - as.Date("2019-02-04")


write.csv(PM19_means, "C:/Users/U8011054/USQ/SCP - Documents/DAW1810/Mungbean/1901_Herm_PM/2019_PM_Mungbean_treatMeans.csv")









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









