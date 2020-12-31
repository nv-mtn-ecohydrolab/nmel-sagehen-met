############# Lauren Bolotin - bolotinljb@gmail.com ##################
library(tidyverse)
library(lubridate)
library(gridExtra)

## Bring in data ####
# Bring in our level 0 tower data
setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
T1_hourly <- readRDS("T1_hourly_lvl0.rds")
T3_hourly <- readRDS("T3_hourly_lvl0.rds")
T4_hourly <- readRDS("T4_hourly_lvl0.rds")

T1_hourly$DateTime <- ymd_hms(T1_hourly$DateTime)
T3_hourly$DateTime <- ymd_hms(T3_hourly$DateTime)
T4_hourly$DateTime <- ymd_hms(T4_hourly$DateTime)


# Bring in Rose's level 1 tower data
setwd("/Volumes/My Passport/Sagehen/Data Paper Download")
R1 <- read.csv("tower1_lvl1.csv")
sapply(R1, class)
R1$Date <- mdy_hm(R1$Date)
names(R1)
names(T1_hourly)

# Plot
p <- ggplot()+
  geom_line(R1, mapping = aes( Date, avgtemp_C_t1_25ft))+
  xlim(as.POSIXct("2009-01-02 13:00:00"), as.POSIXct("2020-12-17 07:00:00"))
p1 <- ggplot()+
  geom_line(T1_hourly, mapping = aes(DateTime, AirTC_25ft_Avg_mean))+
  xlim(as.POSIXct("2009-01-02 13:00:00"), as.POSIXct("2020-12-17 07:00:00"))

grid.arrange(p, p1, ncol=1)

## Compare ####
# Temp 25 ft
comp1 <- select(T1_hourly, 1:2)
comp2 <- select(R1, 1:2)
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$AirTC_25ft_Avg_mean-comp$avgtemp_C_t1_25ft
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))

# Temp 100 ft
comp1 <- select(T1_hourly, c(1,3))
comp2 <- select(R1, c(1,3))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$AirTC_100ft_Avg_mean-comp$avgtemp_C_t1_100ft
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))

# RH 25 ft
comp1 <- select(T1_hourly, c(1,4))
comp2 <- select(R1, c(1,4))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$RH_25ft_mean-comp$RHall_t1_25
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))

# RH 100 ft
comp1 <- select(T1_hourly, c(1,5))
comp2 <- select(R1, c(1,5))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$RH_100ft_mean-comp$RHall_t1_100
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))

# Wind Speed 25 ft
comp1 <- select(T1_hourly, c(1,6))
comp2 <- select(R1, c(1,6))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$WS_ms_25ft_mean-comp$avgws_ms_t1_25ft
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))

# Max Wind Speed 25 ft # the order differs here between the two df's -- go in order of Rose's columns
comp1 <- select(T1_hourly, c(1,10))
comp2 <- select(R1, c(1,7))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$WS_ms_25ft_Max_max-comp$maxws_ms_t1_25ft
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))
# I suspect that Rose calculated the maximum each hour of the ten minute data herself rather than
# using the column that actually say's it captures max wind speed. I will need to go back
# to the tower_eda.R script and change this and see if it helps

# Wind Speed 100 ft 
comp1 <- select(T1_hourly, c(1,7))
comp2 <- select(R1, c(1,8))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$WS_ms_100ft_mean-comp$avgws_ms_t1_100ft
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))
# This one is also off, but I'm not sure why. I need to go back and check the calculations.

# Max Wind Speed 100 ft
comp1 <- select(T1_hourly, c(1,11))
comp2 <- select(R1, c(1,9))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$WS_ms_100ft_Max_max-comp$maxws_ms_t1_100ft
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))
# This one is also off, but I'm not sure why. I need to go back and check the calculations.

# Solar Radiation
comp1 <- select(T1_hourly, c(1,8))
comp2 <- select(R1, c(1,10))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$srad_Wm2_mean-comp$srad_Wm2_
comp$diff <- abs(comp$diff)
comp$diff <- round(comp$diff, digits = 8)

ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))
# This looks like a units issue. See if you can sort out the units. 

# Pressure
comp1 <- select(T1_hourly, c(1,9))
comp2 <- select(R1, c(1,11))
colnames(comp2)[1] <- c("DateTime")
comp <- merge(comp1, comp2, all = TRUE)
comp$diff <- comp$BP_mbar_Avg_mean-comp$BP_mbar_t1
# This is not working for some reason: 
# comp$diff <- round(comp$diff, digits = 8)
# comp$diff <- abs(comp$diff)
# It is assigning a value to the difference even when the two values are the same

# This is also not working: 
# comp$diff <- ifelse(comp$BP_mbar_Avg_mean == comp$BP_mbar_t1, FALSE, TRUE)
# It is assigning TRUE even when the two values are the same


# This only works if you change the type of numeric value to integer (which gets rid of decimals)
# comp$diff <- near(comp$BP_mbar_Avg_mean, comp$BP_mbar_t1)
# Now all diff values say TRUE, so they are all the same



ggplot()+
  geom_point(data = comp, mapping = aes(x = DateTime, y = diff))
