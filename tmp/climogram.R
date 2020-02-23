
## This file can be used to clean up/update 02_weather.Rmd if we use it.
## Delete this file before publishing. Either after incorporation into
## 02_weather.Rmd or upon deletion of that file.

library(ggplot2)
library(dplyr)
library(lubridate)
library(theme.usq)
library(caTools)
source("R/import_data.R")
PM.dat <-
   import_data()  # importing means yield and disease severity for each
# treatment within each powdery mildew trial on mungbean
source("R/gg_color_hue_function.R")

cols1 <- gg_color_hue(4)

# import data from SILO interpolated datasets downloaded from the QLD longpaddock website
silo_war_Tmax <- read.csv("./data/silo_war_Tmax.csv")
silo_king_Tmax <- read.csv("./data/silo_king_Tmax.csv")
silo_gatton_Tmax <- read.csv("./data/silo_gatton_Tmax.csv")
silo_emerald_Tmax <- read.csv("./data/silo_emerald_Tmax.csv")



PM.dat_sum <-
   PM.dat %>%
   distinct(location, year, first_sign_disease) %>%
   mutate(day = yday(first_sign_disease))

# Smoothing temperatures by taking a rolling 7-day average, runmean() from caTools
max_T <- runmean(silo_king_Tmax$max_temp, 7)
min_T <- runmean(silo_king_Tmax$min_temp, 7)

king_Tm_aver <-
   data.frame(
      location = "Kingaroy",
      date = as.Date(silo_king_Tmax$date),
      max_T,
      min_T,
      daily_rain = silo_king_Tmax$daily_rain
   )

# Add columns for year and day of year to data
king_Tm_aver$year <- year(king_Tm_aver$date)
king_Tm_aver$day <- yday(king_Tm_aver$date)

# set year to factor
king_Tm_aver$year <- as.factor(king_Tm_aver$year)

# set up axis
# max rainfall set from
ylim.prim <- c(0, round(max(king_Tm_aver$daily_rain), digits = 0))
ylim.sec <- c(round(min(king_Tm_aver$min_T), digits = 0),
              round(max(king_Tm_aver$max_T), digits = 0))

b <- diff(ylim.prim) / diff(ylim.sec)
a <- b * (ylim.prim[1] - ylim.sec[1])

King_FS <- PM.dat_sum[PM.dat_sum$location == "Kingaroy", ]

king_Tm_aver <- king_Tm_aver[king_Tm_aver$year == "2011" |
                                king_Tm_aver$year == "2012" |
                                king_Tm_aver$year == "2016", ]
king_Tm_aver <- subset(king_Tm_aver, day <= 151)


king_Tm_aver[king_Tm_aver$year == "2011" |
                king_Tm_aver$year == "2012" |
                king_Tm_aver$year == "2016", ] %>% 
   ggplot(aes(x = day, y = daily_rain)) +
   geom_col(colour = usq_cols("support navy")) +
   geom_line(aes(y = a + max_T * b),
                 colour = usq_cols("support red")) +
   geom_line(aes(y = a + min_T * b),
                 colour = usq_cols("support blue")) +
   geom_vline(
      xintercept = King_FS$day[1],
      size = 1,
      colour = cols1[1],
      linetype = "dotted"
   ) +
   geom_vline(
      xintercept = King_FS$day[2],
      size = 1.5,
      colour = cols1[2],
      linetype = "dotted"
   ) +
   geom_vline(
      xintercept = King_FS$day[3],
      size = 1,
      colour = cols1[3],
      linetype = "dashed"
   ) +
   scale_y_continuous("Precipitation (mm)",
                      sec.axis = sec_axis(~ (. - a) / b,
                                          name = "Temperature (˚C)")) +
   labs(title = "Climogram for Kingaroy",
        subtitle = "Daily data sourced from SILO") +
   xlab("Date") +
   theme_usq(base_size = 16) +
   theme(
      axis.ticks.y.left = element_line(colour = usq_cols("support navy")),
      axis.text.y.left = element_text(color = usq_cols("support navy")),
      axis.title.y.left = element_text(color = usq_cols("support navy")),
      strip.text.y = element_text(size = 12)
   ) +
   facet_grid(year ~ ., scales = "free")




ggplot(aes(x = day, colour = year)) +
   geom_line(aes(y = max_T, colour = year, group = day)) +
   geom_line(aes(y = min_T, colour = year, group = day)) +
   
   labs(x = "Date", y = "Daily maximum temperature averaged over 7 days",
        title = "Maximum and minimum daily temperatures in Kingaroy") +
   scale_x_continuous(
      limits = c(0, 200),
      labels = c(subset(king_Tm_aver, year == 2016)$date[c(1, 50, 100, 150, 200)]),
      breaks = c(1, 50, 100, 150, 200)
   ) +
   theme(
      panel.grid.major = element_line(colour = "grey80"),
      panel.grid.minor = element_line(colour = "grey50"),
      panel.background = element_rect(fill = NA)
   ) +
    +
   geom_col(aes(y = daily_rain, colour = year))
