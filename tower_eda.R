############# Lauren Bolotin - bolotinljb@gmail.com ##################

# SH TOWER DATA ####
# Read in raw data, format and remove unreasonable values following Petersky & Harpold 2018

library(tidyverse)
library(lubridate)
library(gridExtra)

# TOWER 1 ####
# avgtemp_C_t1_25ft: hourly avg. temperature (deg C) 25 ft
# avgtemp_C_t1_100ft: hourly avg. temperature (deg C) 100 ft
# RHavg_pct_t1_25ft: hourly avg. relative humidity (%) 25 ft
# RHavg_pct_t1_100ft: hourly avg. relative humidity (%)  100 ft
# ws_ms_t1_25ft: hourly wind speed (m/s) 25 ft
# ws_ms_t1_100ft: hourly wind speed (m/s) 100 ft
# maxws_ms_t1_25ft: maximum hourly wind speed (m/s) 25 ft
# ws_ms_t1_100ft: maximum hourly wind speed (m/s) 100 ft
# srad_Wm2_t1: hourly solar radiation (W/m2)
# BP_mbar_t1: hourly barometric pressure (mbar)

## Berkeley:
setwd("/Volumes/My Passport/Tower Data - Berkeley")
T1 <- read.csv("sagh_sagehen_TenMin.csv", skip = 1)
names(T1)
head(T1)
T1 <- T1[-c(1,2), ]  
T1 <- select(T1, c("TIMESTAMP", "AirTC_25ft_Avg", "AirTC_100ft_Avg",
                   "RH_25ft", "RH_100ft", "WS_ms_25ft", "WS_ms_100ft", 
                   "WS_ms_100ft_Max", "WS_ms_25ft_Max", "SlrkW_Avg",
                   "BP_mbar_Avg"))
T1$TIMESTAMP <- ymd_hms(T1$TIMESTAMP)
T1 <- T1 %>% distinct()
sapply(T1, class)
cols.num <- c("AirTC_25ft_Avg", "AirTC_100ft_Avg",
              "RH_25ft", "RH_100ft", "WS_ms_25ft", "WS_ms_100ft", 
              "WS_ms_100ft_Max", "WS_ms_25ft_Max", "SlrkW_Avg",
              "BP_mbar_Avg")
T1[cols.num] <- sapply(T1[cols.num],as.numeric)
ggplot()+
  geom_line(T1, mapping = aes(TIMESTAMP, AirTC_25ft_Avg))


T1_2 <- read.csv("sagh_sagehen_TenMin_backup.csv", skip = 1)
T1_2 <- T1_2[-c(1,2), ]  
T1_2 <- select(T1_2, c("TIMESTAMP", "AirTC_25ft_Avg", "AirTC_100ft_Avg",
                       "RH_25ft", "RH_100ft", "WS_ms_25ft", "WS_ms_100ft", 
                       "WS_ms_100ft_Max", "WS_ms_25ft_Max", "SlrkW_Avg",
                       "BP_mbar_Avg"))
T1_2 <- T1_2 %>% distinct()
T1_2$TIMESTAMP <- ymd_hms(T1_2$TIMESTAMP)
sapply(T1_2, class)
cols.num <- c("AirTC_25ft_Avg", "AirTC_100ft_Avg",
              "RH_25ft", "RH_100ft", "WS_ms_25ft", "WS_ms_100ft", 
              "WS_ms_100ft_Max", "WS_ms_25ft_Max", "SlrkW_Avg",
              "BP_mbar_Avg")
T1_2[cols.num] <- sapply(T1_2[cols.num],as.numeric)
ggplot()+
  geom_line(T1_2, mapping = aes(TIMESTAMP, AirTC_25ft_Avg))

T1_final <- rbind(T1_2, T1)
T1_final <- T1_final %>% distinct()
ggplot()+
  geom_line(T1_final, mapping = aes(TIMESTAMP, AirTC_25ft_Avg))

## Berkeley Sensor Database:
setwd("/Volumes/My Passport/Sagehen/Data Paper 2.0/Towers/Berkeley Sensor Network")
T1_3 <- read.csv("BSN_T1.csv")
names(T1_3)
names(T1_2)
T1_3 <- select(T1_3, c("Timestamp","Air.Temp.C.25ft.Sage1..Sagehen1.",
                       "Air.Temp.C.100ft.Sage1..Sagehen1.",
                       "Rel.Hum.25ft.Sage1..Sagehen1.",
                       "Rel.Hum.100ft.Sage1..Sagehen1.",
                       "Wind.Spd.MS.25ft.Sage1..Sagehen1.",
                       "Wind.Spd.MS.100ft.Sage1..Sagehen1.",
                       "Wind.Spd.Max.MS.100ft.Sage1..Sagehen1.",
                       "Wind.Spd.Max.MS.25ft.Sage1..Sagehen1.",
                       "Solar.Radiation.Avg.W.m.2.Sage1..Sagehen1.",
                       "BP.mb.25ft.Sage1..Sagehen1." ))
colnames(T1_3) <- colnames(T1_final)
sapply(T1_3, class)
T1_3$TIMESTAMP <- ymd_hms(T1_3$TIMESTAMP)
T1_3 <- T1_3 %>% distinct()
T1_final <- rbind(T1_3, T1_final)
T1_final <- T1_final %>% distinct()

## Dendra:
setwd("/Volumes/My Passport/Sagehen/Data Paper 2.0/Towers/Dendra")
T1_4 <- read.csv("Dendra_T1_AirTemp.csv")
T1_5 <- read.csv("Dendra_T1_RH.csv")
T1_4 <- merge(T1_4, T1_5, all = TRUE)
rm(T1_5)
colnames(T1_4) <- c("TIMESTAMP", "AirTC_100ft_Avg", "AirTC_25ft_Avg", 
                    "RH_100ft", "RH_25ft")
sapply(T1_4, class)
T1_4$TIMESTAMP <- ymd_hms(T1_4$TIMESTAMP)
T1_4 <- T1_4[-which(T1_4$TIMESTAMP %in% T1_final$TIMESTAMP),]

T1_final <- bind_rows(T1_final, T1_4)
dup <- T1_final[duplicated(T1_final$TIMESTAMP),]

# DF Formatting and conversions:
sapply(T1_final, class)
# cols.num <- c("WS_ms_25ft","WS_ms_100ft", "WS_ms_100ft_Max", "WS_ms_25ft_Max",
#               "SlrkW_Avg", "BP_mbar_Avg")
# T1_final[cols.num] <- sapply(T1_final[cols.num],as.numeric)
colnames(T1_final)[10] <- "srad_Wm2"

# Plot 
ggplot()+
  geom_line(dat = T1_final, mapping = aes(x = TIMESTAMP, y = AirTC_25ft_Avg))
ggplot()+
  geom_line(dat = T1_final, mapping = aes(x = TIMESTAMP, y = RH_25ft))
ggplot()+
  geom_line(dat = T1_final, mapping = aes(x = TIMESTAMP, y = srad_Wm2))

rm(T1, T1_2, T1_3, T1_4, cols.num)

# Remove unreasonable data
T1_final$AirTC_25ft_Avg[which(T1_final$AirTC_25ft_Avg < -30)] <- NA #2010-11-04 12:00:00
T1_final$AirTC_100ft_Avg[which(T1_final$AirTC_100ft_Avg < -30)] <- NA
T1_final$AirTC_25ft_Avg[which(T1_final$AirTC_25ft_Avg > 40)] <- NA
T1_final$AirTC_100ft_Avg[which(T1_final$AirTC_100ft_Avg > 40)] <- NA
T1_final$WS_ms_25ft[which(T1_final$WS_ms_25ft < 0)] <- NA
T1_final$WS_ms_100ft[which(T1_final$WS_ms_100ft < 0)] <- NA
T1_final$WS_ms_25ft[which(T1_final$WS_ms_25ft > 25)] <- NA
T1_final$WS_ms_100ft[which(T1_final$WS_ms_100ft > 25)] <- NA

T1_final$srad_Wm2 <- T1_final$srad_Wm2 * 1000
T1_final$srad_Wm2[which(T1_final$srad_Wm2 < 0)] <- NA
T1_final$srad_Wm2[which(T1_final$srad_Wm2 > 2500)] <- NA

T1_final$BP_mbar_Avg[which(T1_final$BP_mbar_Avg < 800)] <- NA
T1_final$RH_25ft[which(T1_final$RH_25ft <0 )] <- NA
T1_final$RH_25ft[which(T1_final$RH_25ft > 100 )] <- NA
T1_final$RH_100ft[which(T1_final$RH_100ft <0 )] <- NA
T1_final$RH_100ft[which(T1_final$RH_100ft > 100 )] <- NA
# Note from metadata: Temperature measurements were logged as daily/hourly averages; precipitation, snow depth, and snow water equivalent were logged with their end-of-day amounts

summary(T1_final$AirTC_25ft_Avg)
summary(T1_final$AirTC_100ft_Avg)
summary(T1_final$RH_25ft)
summary(T1_final$RH_100ft)
summary(T1_final$WS_ms_25ft)
summary(T1_final$WS_ms_100ft)
summary(T1_final$srad_Wm2) # Still confused by these units....something isn't right
summary(T1_final$BP_mbar_Avg)

T1_final$Date <- date(T1_final$TIMESTAMP)
T1_final$DateTime <- paste(substr(T1_final$TIMESTAMP,1, 13), ":00:00", sep = "")
setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
saveRDS(T1_final, "T1_lvl0.rds")

# TOWER 3 ####
# avgtemp_C_t3_25ft: hourly avg. temperature (deg C) 25 ft
# avgtemp_C_t3_100ft: hourly avg. temperature (deg C) 100 ft
# maxtemp_C_t3_25ft: hourly max. temperature (deg C) 25 ft
# maxtemp_C_t3_100ft: hourly max. temperature (deg C) 100 ft
# mintemp_C_t3_25ft: hourly min. temperature (deg C) 25 ft
# mintemp_C_t3_100ft: hourly min. temperature (deg C) 100 ft
# RHavg_pct_t3_25ft: hourly avg. relative humidity (%) 25 ft
# RHavg_pct_t3_100ft: hourly avg. relative humidity (%)  100 ft
# RHmax_pct_t3_25ft: hourly max. relative humidity (%) 25 ft
# RHmax_pct_t3_100ft: hourly max. relative humidity (%)  100 ft
# RHmin_pct_t3_25ft: hourly min. relative humidity (%) 25 ft
# RHmin_pct_t3_100ft: hourly min. relative humidity (%)  100 ft
# ws_ms_t3_25ft: hourly wind speed (m/s) 25 ft
# ws_ms_t3_100ft: hourly wind speed (m/s) 100 ft
# maxws_ms_t3_25ft: maximum hourly wind speed (m/s) 25 ft
# ws_ms_t3_100ft: maximum hourly wind speed (m/s) 100 ft
# srad_Wm2_t3: hourly solar radiation (W/m2)
# BP_mbar_t3: hourly barometric pressure (mbar)

## Berkeley:
setwd("/Volumes/My Passport/Tower Data - Berkeley")
T3 <- read.csv("Station3_wx_TenMin.csv", skip = 1)
names(T3)
T3 <- T3[-c(1,2), ]  
T3 <- select(T3, c("TIMESTAMP", "AirTC_25ft_Avg", "AirTC_25ft_Max",
                   "AirTC_25ft_Min", "AirTC_100ft_Avg", "AirTC_100ft_Max",
                   "AirTC_100ft_Min", "RH_25ft_Max", "RH_25ft_Min",
                   "RH_25ft", "RH_100ft", "RH_100ft_Max", "RH_100ft_Min",
                   "WS_ms_25ft", "WS_ms_25ft_Max", "WS_ms_100ft", 
                   "WS_ms_100ft_Max", "SlrkW_Avg",
                   "BP_mbar_Avg"))
T3 <- T3 %>% distinct()
sapply(T3, class)
cols.num <- c("AirTC_25ft_Avg", "AirTC_25ft_Max",
              "AirTC_25ft_Min", "AirTC_100ft_Avg", "AirTC_100ft_Max",
              "AirTC_100ft_Min", "RH_25ft_Max", "RH_25ft_Min",
              "RH_25ft", "RH_100ft", "RH_100ft_Max", "RH_100ft_Min",
              "WS_ms_25ft", "WS_ms_25ft_Max", "WS_ms_100ft", "WS_ms_100ft_Max",
              "SlrkW_Avg",
              "BP_mbar_Avg")
T3[cols.num] <- sapply(T3[cols.num],as.numeric)
T3$TIMESTAMP <- ymd_hms(T3$TIMESTAMP)

T3_2 <- read.csv("Station3_wx_TenMin_backup.csv", skip = 1)
T3_2 <- T3_2[-c(1,2), ]  
T3_2 <- select(T3_2, c("TIMESTAMP", "AirTC_25ft_Avg", "AirTC_25ft_Max",
                       "AirTC_25ft_Min", "AirTC_100ft_Avg", "AirTC_100ft_Max",
                       "AirTC_100ft_Min", "RH_25ft_Max", "RH_25ft_Min",
                       "RH_25ft", "RH_100ft", "RH_100ft_Max", "RH_100ft_Min",
                       "WS_ms_25ft", "WS_ms_25ft_Max", "WS_ms_100ft", "WS_ms_100ft_Max",
                       "WS_ms_100ft_Max", "WS_ms_25ft_Max", "SlrkW_Avg",
                       "BP_mbar_Avg"))
T3_2 <- T3_2 %>% distinct()
cols.num <- c("AirTC_25ft_Avg", "AirTC_25ft_Max",
              "AirTC_25ft_Min", "AirTC_100ft_Avg", "AirTC_100ft_Max",
              "AirTC_100ft_Min", "RH_25ft_Max", "RH_25ft_Min",
              "RH_25ft", "RH_100ft", "RH_100ft_Max", "RH_100ft_Min",
              "WS_ms_25ft", "WS_ms_25ft_Max", "WS_ms_100ft", "WS_ms_100ft_Max",
              "SlrkW_Avg",
              "BP_mbar_Avg")
T3_2[cols.num] <- sapply(T3_2[cols.num],as.numeric)
T3_2$TIMESTAMP <- ymd_hms(T3_2$TIMESTAMP)

T3_final <- rbind(T3, T3_2)
dup <- T3_final[duplicated(T3_final$TIMESTAMP),]

## Berkeley Sensor Network: 
setwd("/Volumes/My Passport/Sagehen/Data Paper 2.0/Towers/Berkeley Sensor Network")
T3_3 <- read.csv("T3_download.csv")
names(T3_3)
names(T3_2)
T3_3 <- select(T3_3, c("Timestamp","Air.Temp.C.Avg.25ft.Sage3..Sagehen3.",
                       "Air.Temp.C.Max.25ft.Sage3..Sagehen3." ,
                       "Air.Temp.C.Min.25ft.Sage3..Sagehen3." ,
                       "Air.Temp.C.Avg.100ft.Sage3..Sagehen3.",
                       "Air.Temp.C.Max.100ft.Sage3..Sagehen3." ,
                       "Air.Temp.C.Min.100ft.Sage3..Sagehen3.",
                       "Rel.Hum.Max.25ft.Sage3..Sagehen3.",
                       "Rel.Hum.Min.25ft.Sage3..Sagehen3." ,
                       "Rel.Hum.25ft.Sage3..Sagehen3." ,
                       "Rel.Hum.100ft.Sage3..Sagehen3.",
                       "Rel.Hum.Max.100ft.Sage3..Sagehen3.",
                       "Rel.Hum.Min.100ft.Sage3..Sagehen3." ,
                       "Wind.Spd.MS.25ft.Sage3..Sagehen3.",
                       "Wind.Spd.Max.MS.25ft.Sage3..Sagehen3.",
                       "Wind.Spd.MS.100ft.Sage3..Sagehen3." ,
                       "Wind.Spd.Max.MS.100ft.Sage3..Sagehen3.",
                       "Solar.Radiation.Avg.W.m.2.Sage3..Sagehen3.",
                       "BP.mb.Avg.Sage3..Sagehen3."  ))
colnames(T3_3) <- colnames(T3_final)
sapply(T3_3, class)
T3_3$TIMESTAMP <- ymd_hms(T3_3$TIMESTAMP)
T3_3 <- T3_3 %>% distinct()
T3_final <- rbind(T3_3, T3_final)
colnames(T3_final)[18] <- "srad_Wm2"
dup <- T3_final[duplicated(T3_final$TIMESTAMP),]

ggplot()+
  geom_line(dat = T3_final, mapping = aes(x = TIMESTAMP, y = AirTC_25ft_Avg))
ggplot()+
  geom_line(dat = T3_final, mapping = aes(x = TIMESTAMP, y = AirTC_25ft_Max))
ggplot()+
  geom_line(dat = T3_final, mapping = aes(x = TIMESTAMP, y = AirTC_25ft_Min))
ggplot()+
  geom_line(dat = T3_final, mapping = aes(x = TIMESTAMP, y = AirTC_100ft_Avg))
ggplot()+
  geom_line(dat = T3_final, mapping = aes(x = TIMESTAMP, y = AirTC_100ft_Max))
ggplot()+
  geom_line(dat = T3_final, mapping = aes(x = TIMESTAMP, y = AirTC_100ft_Min))+
  xlim(as.POSIXct("2010-01-01 00:00:00"), as.POSIXct("2011-01-01 00:00:00"))


# Remove unreasonable data
T3_final$AirTC_25ft_Avg[which(T3_final$AirTC_25ft_Avg < -30)] <- NA #2010-11-04 12:00:00
T3_final$AirTC_25ft_Avg[which(T3_final$AirTC_25ft_Avg > 40)] <- NA

T3_final$AirTC_100ft_Avg[which(T3_final$AirTC_100ft_Avg < -30)] <- NA
T3_final$AirTC_100ft_Avg[which(T3_final$AirTC_100ft_Avg > 40)] <- NA

T3_final$AirTC_25ft_Min[which(T3_final$AirTC_25ft_Min < -30)] <- NA 
T3_final$AirTC_25ft_Min[which(T3_final$AirTC_25ft_Min > 40)] <- NA 

T3_final$AirTC_25ft_Max[which(T3_final$AirTC_25ft_Max < -30)] <- NA
T3_final$AirTC_25ft_Max[which(T3_final$AirTC_25ft_Max > 40)] <- NA

T3_final$AirTC_100ft_Min[which(T3_final$AirTC_100ft_Min < -30)] <- NA
T3_final$AirTC_100ft_Min[which(T3_final$AirTC_100ft_Min > 40)] <- NA

T3_final$AirTC_100ft_Max[which(T3_final$AirTC_100ft_Max > 40)] <- NA
T3_final$AirTC_100ft_Max[which(T3_final$AirTC_100ft_Max < -30)] <- NA


T3_final$WS_ms_25ft[which(T3_final$WS_ms_25ft < 0)] <- NA
T3_final$WS_ms_100ft[which(T3_final$WS_ms_100ft < 0)] <- NA
T3_final$WS_ms_25ft[which(T3_final$WS_ms_25ft > 25)] <- NA
T3_final$WS_ms_100ft[which(T3_final$WS_ms_100ft > 25)] <- NA

T3_final$srad_Wm2 <- T3_final$srad_Wm2 * 1000
T3_final$srad_Wm2[which(T3_final$srad_Wm2 < 0)] <- NA
T3_final$srad_Wm2[which(T3_final$srad_Wm2 > 2500)] <- NA

T3_final$BP_mbar_Avg[which(T3_final$BP_mbar_Avg < 800)] <- NA
T3_final$RH_25ft_Max[which(T3_final$RH_25ft_Max < 0)] <- NA
T3_final$RH_25ft_Max[which(T3_final$RH_25ft_Max > 100)] <- NA
T3_final$RH_100ft_Max[which(T3_final$RH_100ft_Max < 0)] <- NA
T3_final$RH_100ft_Max[which(T3_final$RH_100ft_Max > 100)] <- NA
T3_final$RH_25ft_Min[which(T3_final$RH_25ft_Min < 0)] <- NA
T3_final$RH_25ft_Min[which(T3_final$RH_25ft_Min > 100)] <- NA
T3_final$RH_100ft_Min[which(T3_final$RH_100ft_Min < 0)] <- NA
T3_final$RH_100ft_Min[which(T3_final$RH_100ft_Min > 100)] <- NA
T3_final$RH_25ft[which(T3_final$RH_25ft < 0)] <- NA
T3_final$RH_100ft[which(T3_final$RH_100ft > 100)] <- NA

summary(T3_final$AirTC_25ft_Avg)
summary(T3_final$AirTC_100ft_Avg)
summary(T3_final$RH_25ft)
summary(T3_final$RH_100ft)
summary(T3_final$WS_ms_25ft)
summary(T3_final$WS_ms_100ft)
summary(T3_final$srad_Wm2) # Still confused by these units....something isn't right
summary(T3_final$BP_mbar_Avg)

ggplot()+
  geom_line(dat = T3_final, mapping = aes(x = TIMESTAMP, y = AirTC_25ft_Avg))

rm(T3, T3_2, T3_3, dup, cols.num)

T3_final$Date <- date(T3_final$TIMESTAMP)
T3_final$DateTime <- paste(substr(T3_final$TIMESTAMP,1, 13), ":00:00", sep = "")
setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
saveRDS(T3_final, "T3_lvl0.rds")

# TOWER 4 ####
# avgtemp_C_t4_25ft: hourly avg. temperature (deg C) 25 ft
# avgtemp_C_t4_100ft: hourly avg. temperature (deg C) 100 ft
# maxtemp_C_t4_25ft: hourly max. temperature (deg C) 25 ft
# mintemp_C_t4_25ft: hourly min. temperature (deg C) 25 ft
# RHavg_pct_t4_25ft: hourly avg. relative humidity (%) 25 ft
# RHmax_pct_t4_25ft: hourly max. relative humidity (%) 25 ft
# RHmin_pct_t4_25ft: hourly min. relative humidity (%) 25 ft
# ws_ms_t4_25ft: hourly wind speed (m/s) 25 ft
# ws_ms_t4_100ft: hourly wind speed (m/s) 100 ft
# maxws_ms_t4_25ft: maximum hourly wind speed (m/s) 25 ft
# ws_ms_t4_100ft: maximum hourly wind speed (m/s) 100 ft
# srad_Wm2_t4: hourly solar radiation (W/m2)
# BP_mbar_t4: hourly barometric pressure (mbar)

## Berkeley: 
setwd("/Volumes/My Passport/Tower Data - Berkeley")
T4 <- read.csv("Station4_wx_TenMin.csv", skip = 1)
names(T4)
T4 <- T4[-c(1,2), ]  
T4 <- select(T4, c("TIMESTAMP","AirTC_25ft_Avg", "AirTC_100ft_Avg", 
                   "AirTC_25ft_Max","AirTC_25ft_Min", "RH_25ft_Max",       
                   "RH_25ft_Min", "RH_25ft", "RH_100ft", "WS_ms_25ft", 
                   "WS_ms_100ft", "WS_ms_25ft_Max", "WS_ms_100ft_Max",
                   "SlrkW_Avg", "BP_mbar_Avg"))
sapply(T4, class)
cols.num <- c("AirTC_25ft_Avg", "AirTC_100ft_Avg", 
              "AirTC_25ft_Max","AirTC_25ft_Min", "RH_25ft_Max",       
              "RH_25ft_Min", "RH_25ft", "RH_100ft", "WS_ms_25ft", 
              "WS_ms_100ft", "WS_ms_25ft_Max", "WS_ms_100ft_Max",
              "SlrkW_Avg", "BP_mbar_Avg")
T4[cols.num] <- sapply(T4[cols.num],as.numeric)
T4$TIMESTAMP <- ymd_hms(T4$TIMESTAMP)
T4 <- T4 %>% distinct()

T4_2 <- read.csv("Station4_wx_TenMin_backup.csv", skip = 1)
T4_2 <- T4_2[-c(1,2), ]  
T4_2 <- select(T4_2, c("TIMESTAMP","AirTC_25ft_Avg", "AirTC_100ft_Avg", 
                       "AirTC_25ft_Max","AirTC_25ft_Min", "RH_25ft_Max",       
                       "RH_25ft_Min", "RH_25ft", "RH_100ft", "WS_ms_25ft", 
                       "WS_ms_100ft", "WS_ms_25ft_Max", "WS_ms_100ft_Max",
                       "SlrkW_Avg", "BP_mbar_Avg"))
sapply(T4_2, class)
cols.num <- c("AirTC_25ft_Avg", "AirTC_100ft_Avg", 
              "AirTC_25ft_Max","AirTC_25ft_Min", "RH_25ft_Max",       
              "RH_25ft_Min", "RH_25ft", "RH_100ft", "WS_ms_25ft", 
              "WS_ms_100ft", "WS_ms_25ft_Max", "WS_ms_100ft_Max",
              "SlrkW_Avg", "BP_mbar_Avg")
T4_2[cols.num] <- sapply(T4_2[cols.num],as.numeric)
T4_2$TIMESTAMP <- ymd_hms(T4_2$TIMESTAMP)
T4_2 <- T4_2 %>% distinct()

T4_final <- rbind(T4, T4_2)

ggplot()+
  geom_line(dat = T4_final, mapping = aes(x = TIMESTAMP, y = AirTC_25ft_Avg))

## Berkeley Sensor Network:
setwd("/Volumes/My Passport/Sagehen/Data Paper 2.0/Towers/Berkeley Sensor Network")
T4_3 <- read.csv("T4_download.csv")
names(T4_3)
names(T4_2)
T4_3 <- select(T4_3, c("Timestamp","Air.Temp.C.Avg.25ft.Sage4..Sagehen4.",
                       "Air.Temp.C.Avg.100ft.Sage4..Sagehen4.",
                       "Air.Temp.C.Max.25ft.Sage4..Sagehen4." ,
                       "Air.Temp.C.Min.25ft.Sage4..Sagehen4." ,
                       "Rel.Hum.Max.25ft.Sage4..Sagehen4.",
                       "Rel.Hum.Min.25ft.Sage4..Sagehen4." ,
                       "Rel.Hum.25ft.Sage4..Sagehen4." ,
                       "Rel.Hum.100ft.Sage4..Sagehen4.",
                       "Wind.Spd.MS.25ft.Sage4..Sagehen4.",
                       "Wind.Spd.MS.100ft.Sage4..Sagehen4." ,
                       "Wind.Spd.Max.MS.25ft.Sage4..Sagehen4.",
                       "Wind.Spd.Max.MS.100ft.Sage4..Sagehen4.",
                       "Solar.Radiation.Avg.W.m.2.Sage4..Sagehen4.",
                       "BP.mb.Avg.Sage4..Sagehen4."  ))
colnames(T4_3) <- colnames(T4_final)
sapply(T4_3, class)
T4_3$TIMESTAMP <- ymd_hms(T4_3$TIMESTAMP)
T4_3 <- T4_3 %>% distinct()
T4_final <- rbind(T4_3, T4_final)
colnames(T4_final)[14] <- "srad_Wm2"
dup <- T4_final[duplicated(T4_final$TIMESTAMP),]

ggplot()+
  geom_line(dat = T4_final, mapping = aes(x = TIMESTAMP, y = AirTC_25ft_Avg))

# Remove unreasonable data
T4_final$AirTC_25ft_Avg[which(T4_final$AirTC_25ft_Avg < -30)] <- NA #2010-11-04 12:00:00
T4_final$AirTC_100ft_Avg[which(T4_final$AirTC_100ft_Avg < -30)] <- NA
T4_final$AirTC_25ft_Avg[which(T4_final$AirTC_25ft_Avg > 40)] <- NA
T4_final$AirTC_100ft_Avg[which(T4_final$AirTC_100ft_Avg > 40)] <- NA
T4_final$WS_ms_25ft[which(T4_final$WS_ms_25ft < 0)] <- NA
T4_final$WS_ms_100ft[which(T4_final$WS_ms_100ft < 0)] <- NA
T4_final$WS_ms_25ft[which(T4_final$WS_ms_25ft > 25)] <- NA
T4_final$WS_ms_100ft[which(T4_final$WS_ms_100ft > 25)] <- NA

T4_final$srad_Wm2 <- T4_final$srad_Wm2 * 1000
T4_final$srad_Wm2[which(T4_final$srad_Wm2 < 0)] <- NA
T4_final$srad_Wm2[which(T4_final$srad_Wm2 > 2500)] <- NA
T4_final$BP_mbar_Avg[which(T4_final$BP_mbar_Avg < 800)] <- NA
T4_final$RH_25ft_Max[which(T4_final$RH_25ft_Max < 0)] <- NA
T4_final$RH_25ft_Max[which(T4_final$RH_25ft_Max > 100)] <- NA
T4_final$RH_25ft_Min[which(T4_final$RH_25ft_Min < 0)] <- NA
T4_final$RH_25ft_Min[which(T4_final$RH_25ft_Min > 100)] <- NA
T4_final$RH_25ft[which(T4_final$RH_25ft < 0)] <- NA
T4_final$RH_100ft[which(T4_final$RH_100ft > 100)] <- NA

summary(T4_final$AirTC_25ft_Avg)
summary(T4_final$AirTC_100ft_Avg)
summary(T4_final$RH_25ft)
summary(T4_final$RH_100ft)
summary(T4_final$WS_ms_25ft)
summary(T4_final$WS_ms_100ft)
summary(T4_final$srad_Wm2) # Still confused by these units....something isn't right
summary(T4_final$BP_mbar_Avg)

rm(T4, T4_2, T4_3, cols.num, dup)

ggplot()+
  geom_line(dat = T4_final, mapping = aes(x = TIMESTAMP, y = AirTC_25ft_Avg))

T4_final$Date <- date(T4_final$TIMESTAMP)
T4_final$DateTime <- paste(substr(T4_final$TIMESTAMP,1, 13), ":00:00", sep = "")

setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
saveRDS(T4_final, "T4_lvl0.rds")

# HOURLY/DAILY DATA ####
## Tower 1 :
setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
T1_final <- readRDS("T1_lvl0.rds")
names(T1_final)
T1_hourly_1 <- T1_final %>%
  group_by(DateTime) %>%
  summarise_at(.vars = c("AirTC_25ft_Avg", "AirTC_100ft_Avg", "RH_25ft", "RH_100ft",
                         "WS_ms_25ft", "WS_ms_100ft", "srad_Wm2", "BP_mbar_Avg"), .funs = c("mean" = mean))
T1_hourly_2 <- T1_final %>%
  group_by(DateTime) %>%
  summarise_at(.vars = c("WS_ms_25ft_Max", "WS_ms_100ft_Max", "WS_ms_25ft", "WS_ms_100ft"), .funs = c("max" = max))

T1_hourly <- merge(T1_hourly_1, T1_hourly_2)
T1_hourly$DateTime <- ymd_hms(T1_hourly$DateTime)
rm(T1_hourly_1, T1_hourly_2)

setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
saveRDS(T1_hourly, "T1_hourly_lvl0.rds")
saveRDS(T1_hourly, "T1_hourly_lvl0_2.0.rds") # has both max calculated from _Max wind speeds and max calculated from regular wind speed 


T1_daily_1 <- T1_final %>%
  group_by(Date) %>%
  summarise_at(.vars = c("AirTC_25ft_Avg", "AirTC_100ft_Avg", "RH_25ft", "RH_100ft",
                         "WS_ms_25ft", "WS_ms_100ft", "srad_Wm2", "BP_mbar_Avg"), .funs = c("mean" = mean))
T1_daily_2 <- T1_final %>%
  group_by(Date) %>%
  summarise_at(.vars = c("WS_ms_25ft_Max", "WS_ms_100ft_Max"), .funs = c("max" = max))
T1_daily <- merge(T1_daily_1, T1_daily_2)
rm(T1_daily_2, T1_daily_1)

## Tower 3:
setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
T3_final <- readRDS("T3_lvl0.rds")
names(T3_final)
T3_hourly_1 <- T3_final %>%
  group_by(DateTime) %>%
  summarise_at(.vars = c("AirTC_25ft_Avg",  "AirTC_100ft_Avg", "RH_25ft", 
                         "RH_100ft", "WS_ms_25ft", "WS_ms_100ft", "srad_Wm2",
                         "BP_mbar_Avg"), .funs = c("mean" = mean))
T3_hourly_2 <-  T3_final %>%
  group_by(DateTime) %>%
  summarise_at(.vars = c("AirTC_25ft_Avg", "AirTC_100ft_Avg", "RH_25ft", 
                         "RH_100ft", "WS_ms_25ft", "WS_ms_100ft"),
               .funs = c("max" = max))
T3_hourly_3 <- T3_final %>% 
  group_by(DateTime) %>%
  summarise_at(.vars = c("AirTC_25ft_Avg", "AirTC_100ft_Avg", "RH_25ft",
                         "RH_100ft"), .funs = c("min" = min))
T3_hourly <- merge(T3_hourly_1, T3_hourly_2)
T3_hourly <- merge(T3_hourly, T3_hourly_3)
rm(T3_hourly_1, T3_hourly_2, T3_hourly_3)

names(T3_final)
T3_daily_1 <- T3_final %>%
  group_by(Date) %>%
  summarise_at(.vars = c("AirTC_25ft_Avg",  "AirTC_100ft_Avg", "RH_25ft", 
               "RH_100ft", "WS_ms_25ft", "WS_ms_100ft", "srad_Wm2",
               "BP_mbar_Avg"), .funs = c("mean" = mean)) 
T3_daily_2 <- T3_final %>%
  group_by(Date) %>%
  summarise_at(.vars = c("AirTC_25ft_Max", "AirTC_100ft_Max", "RH_25ft_Max", 
                         "RH_100ft_Max", "WS_ms_25ft_Max", "WS_ms_100ft_Max"),
               .funs = c("max" = max))
T3_daily_3 <- T3_final %>%
  group_by(Date) %>%
  summarise_at(.vars = c("AirTC_25ft_Min", "AirTC_100ft_Min", "RH_25ft_Min",
                         "RH_100ft_Min"), .funs = c("min" = min))
T3_daily <- merge(T3_daily_1, T3_daily_2)
T3_daily <- merge(T3_daily, T3_daily_3)
rm(T3_daily_1, T3_daily_2, T3_daily_3)

## Tower 4:
setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
T4_final <- readRDS("T4_lvl0.rds")
names(T4_final)
T4_hourly_1 <- T4_final %>%
  group_by(DateTime) %>%
  summarise_at(.vars = c("AirTC_25ft_Avg", "AirTC_100ft_Avg", "RH_25ft", 
                         "RH_100ft", "WS_ms_25ft", "WS_ms_100ft", "srad_Wm2",
                         "BP_mbar_Avg"), .funs = c("mean" = mean))
T4_hourly_2 <- T4_final %>%
  group_by(DateTime) %>%
  summarise_at(.vars = c("AirTC_25ft_Max", "RH_25ft", "WS_ms_25ft",
                         "WS_ms_100ft"), .funs = c("max" = max))
T4_hourly_3 <- T4_final %>%
  group_by(DateTime) %>%
  summarise_at(.vars = c("AirTC_25ft_Min", "RH_25ft"), .funs = c("min" = min))
T4_hourly <- merge(T4_hourly_1, T4_hourly_2)
T4_hourly <- merge(T4_hourly, T4_hourly_3)
rm(T4_hourly_1, T4_hourly_2, T4_hourly_3)
T4_daily_1 <-  T4_final %>%
  group_by(Date) %>%
  summarise_at(.vars = c("AirTC_25ft_Avg", "AirTC_100ft_Avg", "RH_25ft", 
                         "RH_100ft", "WS_ms_25ft", "WS_ms_100ft", "srad_Wm2",
                         "BP_mbar_Avg"), .funs = c("mean" = mean))
T4_daily_2 <- T4_final %>%
  group_by(Date) %>%
  summarise_at(.vars = c("AirTC_25ft_Max", "RH_25ft_Max", "WS_ms_25ft_Max",
                         "WS_ms_100ft_Max"), .funs = c("max" = max))
T4_daily_3 <- T4_final %>%
  group_by(Date) %>%
  summarise_at(.vars = c("AirTC_25ft_Min", "RH_25ft_Min"), .funs = c("min" = min))
T4_daily <- merge(T4_daily_1, T4_daily_2)
T4_daily <- merge(T4_daily, T4_daily_3)
rm(T4_daily_1, T4_daily_2, T4_daily_3)


# Save data frames
setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
saveRDS(T3_hourly, "T3_hourly_lvl0.rds")
saveRDS(T4_hourly, "T4_hourly_lvl0.rds")

saveRDS(T1_daily, "T1_daily_lvl0.rds")
saveRDS(T3_daily, "T3_daily_lvl0.rds")
saveRDS(T4_daily, "T4_daily_lvl0.rds")

rm(T1_final, T3_final, T4_final)


