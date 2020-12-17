library(tidyverse)
library(lubridate)

# WRCC DATA ####
setwd("/Volumes/My Passport/Sagehen/Sagehen Data")
wrcc <- read.csv("WRCC_DRI_data.csv")
head(wrcc)
sapply(wrcc, class)

# DateTime
colnames(wrcc)[1] <- "DateTime"
wrcc$Year <- substr(wrcc$DateTime, 1, 2)
wrcc$Month <- substr(wrcc$DateTime, 3, 4)
wrcc$Day <- substr(wrcc$DateTime, 5, 6)
wrcc$Hour <- substr(wrcc$DateTime, 7, 8)
wrcc$Minute <- substr(wrcc$DateTime, 9, 10)
wrcc$DateTime <- paste0("20", wrcc$Year, "-", wrcc$Month, "-", wrcc$Day, " ", wrcc$Hour, ":", wrcc$Minute)
wrcc <- select(wrcc, -c("Year", "Month", "Day", "Hour", "Minute"))
wrcc$DateTime <- ymd_hm(wrcc$DateTime)

# Precip 
colnames(wrcc)[2] <- "Precip_in"
# Wind speed
colnames(wrcc)[3] <- "Wind_Speed_mph"
# Wind direction
colnames(wrcc)[4] <- "Wind_Dir_deg"
# Air temp
colnames(wrcc)[5] <- "Air_Temp_degC"
# Fuel temp
wrcc <- select(wrcc, -c("Fuel_Temp"))
# RH
colnames(wrcc)[6] <- "Rel_Humidity_pct"
# Battery
colnames(wrcc)[7] <- "Battery_volts"
# Dir Max Gust
wrcc <- select(wrcc, -c("Dir_MxGust"))
# MxGust Speed
colnames(wrcc)[8] <- "MxGust_Speed_mph"
# Solar rad
colnames(wrcc)[9] <- "Solar_Rad_w_m2"

# Replace all -9999 with NA
wrcc$Precip_in[which(wrcc$Precip_in == "-9999")] <- NA 
wrcc$Air_Temp_degC[which(wrcc$Air_Temp_degC == "-9999")] <- NA 
wrcc$Rel_Humidity_pct[which(wrcc$Rel_Humidity_pct == "-9999")] <- NA 
wrcc$Solar_Rad_w_m2[which(wrcc$Solar_Rad_w_m2 == "-9999")] <- NA 


# Plots ####
theme_set(theme(legend.position = "none",panel.background = element_blank(), 
                axis.line = element_line(colour = "black")))

# Precip
ggplot(wrcc, aes(DateTime, Precip_in))+
  geom_line()

# Temp
ggplot(wrcc, aes(DateTime, Air_Temp_degC))+
  geom_line()

# RH
ggplot(wrcc, aes(DateTime, Rel_Humidity_pct))+
  geom_line()

# Solar rad
ggplot(wrcc, aes(DateTime, Solar_Rad_w_m2))+
  geom_line()
