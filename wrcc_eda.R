library(tidyverse)
library(lubridate)
library(weathermetrics)
library(readxl)

# WRCC DATA ####
setwd("/Volumes/My Passport/Sagehen/Data Paper 2.0/COOP")
wrcc_1 <- read.table("WRCC_data_1.xls", skip = 3, header = TRUE, row.names = NULL)
wrcc_2 <- read.table("WRCC_data_2.xls", skip = 3, header = TRUE, row.names = NULL)
wrcc_3 <- read.table("WRCC_data_3.xls", skip = 3, header = TRUE, row.names = NULL)
wrcc_4 <- read.table("WRCC_data_4.xls", skip = 3, header = TRUE, row.names = NULL)
wrcc_5 <- read.table("WRCC_data_5.xls", skip = 3, header = TRUE, row.names = NULL)

head(wrcc_1)
col.names <- c("DateTime", "WS_ms", "AirTC_avg", "AirTC_max", "AirTC_min",
                      "RH_pct", "RH_max", "RH_min", "BP_mbar", "Srad_Wm2", 
                      "Precip_mm", "Precip_accum", "SnowDepth_mm")

colnames(wrcc_1) <- col.names
colnames(wrcc_2) <- col.names
colnames(wrcc_3) <- col.names
colnames(wrcc_4) <- col.names
colnames(wrcc_5) <- col.names
rm(col.names)

wrcc_s <- bind_rows(wrcc_1, wrcc_2, wrcc_3, wrcc_4, wrcc_5)
rm(wrcc_1, wrcc_2, wrcc_3, wrcc_4, wrcc_5)
head(wrcc_s)
wrcc_s$Year <- substr(wrcc_s$DateTime, 1, 2)
wrcc_s$Month <- substr(wrcc_s$DateTime, 3,4)
wrcc_s$Day <- substr(wrcc_s$DateTime, 5, 6)
wrcc_s$Hour <- substr(wrcc_s$DateTime, 7, 8)
wrcc_s$Minute <- substr(wrcc_s$DateTime, 9, 10)

head(wrcc_s10)
sapply(wrcc_s10, class)

# FORMAT DF #### 
# DateTime
colnames(wrcc_s10)[1] <- "DateTime"
wrcc_s10$Year <- substr(wrcc_s10$DateTime, 1, 2)
wrcc_s10$Month <- substr(wrcc_s10$DateTime, 3, 4)
wrcc_s10$Day <- substr(wrcc_s10$DateTime, 5, 6)
wrcc_s10$Hour <- substr(wrcc_s10$DateTime, 7, 8)
wrcc_s10$Minute <- substr(wrcc_s10$DateTime, 9, 10)
wrcc_s10$DateTime <- paste0("20", wrcc_s10$Year, "-", wrcc_s10$Month, "-", wrcc_s10$Day, " ", wrcc_s10$Hour, ":", wrcc_s10$Minute)
wrcc_s10 <- select(wrcc_s10, -c("Year", "Month", "Day", "Hour", "Minute", "Fuel_Temp", "Battery_Voltage"))
wrcc_s10$DateTime <- ymd_hm(wrcc_s10$DateTime)
# Precip 
colnames(wrcc_s10)[2] <- "Precip_mm"
wrcc_s10$Precip_mm <- wrcc_s10$Precip_mm*25.4
# Wind speed
colnames(wrcc_s10)[3] <- "Wind_Speed_ms"
wrcc_s10$Wind_Speed_ms <- wrcc_s10$Wind_Speed_ms*0.44704
# Wind direction
colnames(wrcc_s10)[4] <- "Wind_Dir_deg"
# Air temp
colnames(wrcc_s10)[5] <- "Air_Temp_degC" 
wrcc_s10$Air_Temp_degC <- fahrenheit.to.celsius(wrcc_s10$Air_Temp_degC)
# RH
colnames(wrcc_s10)[6] <- "Rel_Humidity_pct"
# Dir Max Gust
colnames(wrcc_s10)[7] <- "Dir_MxGust"
# MxGust Speed
colnames(wrcc_s10)[8] <- "MxGust_Speed_mph"
# Solar rad
colnames(wrcc_s10)[9] <- "Solar_Rad_w_m2"
# Replace all -9999 with NA
wrcc_s10$Precip_mm[which(wrcc_s10$Precip_mm == "-9999")] <- NA 
wrcc_s10$Air_Temp_degC[which(wrcc_s10$Air_Temp_degC == "-9999")] <- NA 
wrcc_s10$Rel_Humidity_pct[which(wrcc_s10$Rel_Humidity_pct == "-9999")] <- NA 
wrcc_s10$Solar_Rad_w_m2[which(wrcc_s10$Solar_Rad_w_m2 == "-9999")] <- NA 

# CREATE DAILY DF ####
# Daily Averages
wrcc_sd <- wrcc_s10 %>%
  group_by(date(DateTime)) %>%
  summarise_at(.vars = c("Air_Temp_degC"), .funs = c("mean" = mean))

# Daiy Increment (P)
wrcc_sd_p <- wrcc_s10 %>%
  group_by(date(DateTime)) %>%
  summarise_at(.vars = c("Precip_mm"), .funs = c("sum" = sum))
wrcc_sd$Precip_mm_sum <- wrcc_sd_p$sum
rm(wrcc_sd_p)

# Daily Max/Min (T)
wrcc_sd_t <- wrcc_s10 %>%
  group_by(date(DateTime)) %>%
  summarise_at(.vars = c("Air_Temp_degC"), .funs = c("min" = min, "max" = max))
wrcc_sd$Air_Temp_degC_min <- wrcc_sd_t$min
wrcc_sd$Air_Temp_degC_max <- wrcc_sd_t$max
rm(wrcc_sd_t)
colnames(wrcc_sd)[1] <- "Date"

# CREATE HOURLY DF ####
wrcc_s10$DateTime2 <- substr(wrcc_s10$DateTime, 1, 13)
wrcc_s10$DateTime2 <- paste0(wrcc_s10$DateTime2, ":00:00")
# Hourly Averages
wrcc_sh <- wrcc_s10 %>%
  group_by(DateTime2) %>%
  summarise_at(.vars = c("Wind_Speed_ms", "Wind_Dir_deg",
                         "Air_Temp_degC", "Rel_Humidity_pct",
                         "MxGust_Speed_mph", "Solar_Rad_w_m2"), .funs = c("mean" = mean))

# Hourly Increment (P)
wrcc_sh_p <- wrcc_s10 %>%
  group_by(DateTime2) %>%
  summarise_at(.vars = c("Precip_mm"), .funs = c("sum" = sum))
wrcc_sh$Precip_mm_sum <- wrcc_sh_p$sum
rm(wrcc_sh_p)

# Hourly Max/Min (T)
wrcc_sh_trh <- wrcc_s10 %>%
  group_by(DateTime2) %>%
  summarise_at(.vars = c("Air_Temp_degC", "Rel_Humidity_pct"), .funs = c("min" = min, "max" = max))
wrcc_sh$Air_Temp_degC_min <- wrcc_sh_trh$Air_Temp_degC_min
wrcc_sh$Air_Temp_degC_max <- wrcc_sh_trh$Air_Temp_degC_max
wrcc_sh$Rel_Humidity_min <- wrcc_sh_trh$Rel_Humidity_pct_min
wrcc_sh$Rel_Humidity_max <- wrcc_sh_trh$Rel_Humidity_pct_max
rm(wrcc_sh_trh)
colnames(wrcc_sh)[1] <- "Date"

# COMPARE to ROSE ####
setwd("/Volumes/My Passport/Sagehen/Data Paper Download")
wrcc_sh_R <- read.csv("dfwrcc_hourly_lvl1.csv")

# # Plots #
# theme_set(theme(legend.position = "none",panel.background = element_blank(), 
#                 axis.line = element_line(colour = "black")))
# 
# # Precip
# ggplot(wrcc_sh, aes(DateTime, Precip_mm))+
#   geom_line()
# 
# # Temp
# ggplot(wrcc_sh, aes(DateTime, Air_Temp_degC))+
#   geom_line()
# 
# # RH
# ggplot(wrcc_sh, aes(DateTime, Rel_Humidity_pct))+
#   geom_line()
# 
# # Solar rad
# ggplot(wrcc_sh, aes(DateTime, Solar_Rad_w_m2))+
#   geom_line()
