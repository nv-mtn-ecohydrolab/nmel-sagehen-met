library(tidyverse)
library(lubridate)
library(weathermetrics)
library(readxl)
library(gridExtra)
# Snow Depth: low of 0 cm; all snow data from July 1st to September 30th changed to 0 cm
# Snow Water Equivalent: low of 0 mm; all snow data from July 1st to September 30th changed to 0 mm
# Soil Moisture: low of 0.01 (and 0% values changed to 0.01%) and high of 100%
# Cumulative Precipitation: low of 0 mm; for the WRCC hourly precipitation only: we use daily totals for P to make sure our annual sums are correct. We then normalize hourly precipitation for each day. Hourly SWE data was used to confirm increases in precipitation assigned to the hourly weight, i.e. hourly increases in SWE matched hourly increases in P. If SWE and P confirmed hourly weighting in that day, the normalized values were multiplied by the daily precipitation to create the hourly precipitation record. If P and SWE hourly data did not match but there was daily precipitation, than the hourly SWE data was normalized and used to distribute the daily precipitation amount.  In cases where precipitation was >0 but SWE was <=0, then hourly precipitation was assume equal.  All accumulated precipitation values were forced to be equal or greater thant he previous time step  . 

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
wrcc_s$DateTime <- as.POSIXct(wrcc_s$DateTime, format = "%y%m%d%H%M")

# FORMAT DF #### 
# Replace all -9999 with NA
wrcc_s$WS_ms[which(wrcc_s$WS_ms == "-9999")] <- NA 
wrcc_s$AirTC_avg[which(wrcc_s$AirTC_avg == "-9999")] <- NA 
wrcc_s$AirTC_max[which(wrcc_s$AirTC_max == "-9999")] <- NA 
wrcc_s$AirTC_min[which(wrcc_s$AirTC_min == "-9999")] <- NA 
wrcc_s$RH_pct[which(wrcc_s$RH_pct == "-9999")] <- NA 
wrcc_s$RH_max[which(wrcc_s$RH_max == "-9999")] <- NA 
wrcc_s$RH_min[which(wrcc_s$RH_min == "-9999")] <- NA 
wrcc_s$BP_mbar[which(wrcc_s$BP_mbar == "-9999")] <- NA 
wrcc_s$Srad_Wm2[which(wrcc_s$Srad_Wm2 == "-9999")] <- NA 
wrcc_s$Precip_mm[which(wrcc_s$Precip_mm == "-9999")] <- NA 
wrcc_s$Precip_accum[which(wrcc_s$Precip_accum == "-9999")] <- NA 
wrcc_s$SnowDepth_mm[which(wrcc_s$SnowDepth_mm == "-9999")] <- NA 

# Remove unreasonable data
wrcc_s$AirTC_avg[which(wrcc_s$AirTC_avg < -30)] <- NA
wrcc_s$AirTC_avg[which(wrcc_s$AirTC_avg > 40)] <- NA
wrcc_s$AirTC_max[which(wrcc_s$AirTC_max < -30)] <- NA
wrcc_s$AirTC_max[which(wrcc_s$AirTC_max > 40)] <- NA
wrcc_s$AirTC_min[which(wrcc_s$AirTC_min < -30)] <- NA
wrcc_s$AirTC_min[which(wrcc_s$AirTC_min > 40)] <- NA
wrcc_s$WS_ms[which(wrcc_s$WS_ms < 0)] <- NA
wrcc_s$WS_ms[which(wrcc_s$WS_ms > 25)] <- NA
wrcc_s$Srad_Wm2[which(wrcc_s$Srad_Wm2 < 0)] <- NA
wrcc_s$Srad_Wm2[which(wrcc_s$Srad_Wm2 > 2500)] <- NA
wrcc_s$BP_mbar[which(wrcc_s$BP_mbar < 800)] <- NA
wrcc_s$RH_pct[which(wrcc_s$RH_pct < 0)] <- NA
wrcc_s$RH_pct[which(wrcc_s$RH_pct > 100)] <- NA
wrcc_s$RH_max[which(wrcc_s$RH_max < 0)] <- NA
wrcc_s$RH_max[which(wrcc_s$RH_max > 100)] <- NA
wrcc_s$RH_min[which(wrcc_s$RH_min < 0)] <- NA
wrcc_s$RH_min[which(wrcc_s$RH_min > 100)] <- NA

# CREATE DAILY DF ####
# Daily Averages
# Not sure what to do with snow depth
wrcc_s$Date <- date(wrcc_s$DateTime)
names(wrcc_s)
wrcc_sd <- wrcc_s %>%
  group_by(date(Date)) %>%
  summarise_at(.vars = c("AirTC_avg", "WS_ms", "RH_pct", "BP_mbar", "Srad_Wm2", "SnowDepth_mm"), 
               .funs = c("mean" = mean))

# Daiy Increment (P) # this should = precip_accum
wrcc_sd_p <- wrcc_s %>%
  group_by(date(Date)) %>%
  summarise_at(.vars = c("Precip_mm"), .funs = c("sum" = sum))
wrcc_sd$Precip_mm_sum <- wrcc_sd_p$sum
rm(wrcc_sd_p)

# Daily Max/Min (T, RH, WS)
wrcc_sd_max <- wrcc_s %>%
  group_by(date(Date)) %>%
  summarise_at(.vars = c("AirTC_max", "RH_max", "WS_ms", "Precip_accum"), .funs = c("max" = max))
colnames(wrcc_sd_max)[1] <- "Date"
wrcc_sd_min <- wrcc_s %>%
  group_by(date(Date)) %>%
  summarise_at(.vars = c("AirTC_min", "RH_min"), .funs = c("min" = min))
colnames(wrcc_sd_min)[1] <- "Date"
colnames(wrcc_sd)[1] <- "Date"

wrcc_sd <- as.data.frame(wrcc_sd)
wrcc_sd_max <- as.data.frame(wrcc_sd_max)
wrcc_sd_min <- as.data.frame(wrcc_sd_min)

wrcc_sd <- bind_cols(wrcc_sd, wrcc_sd_max, wrcc_sd_min)
rm(wrcc_sd_max, wrcc_sd_min)
wrcc_sd <- select(wrcc_sd, -c("Date...9", "Date...14"))
colnames(wrcc_sd)[1] <- "Date"

# CREATE HOURLY DF ####
wrcc_s$DateTime2 <- substr(wrcc_s$DateTime, 1, 13)
wrcc_s$DateTime2 <- paste0(wrcc_s$DateTime2, ":00:00")
# Hourly Averages
wrcc_sh <- wrcc_s %>%
  group_by(DateTime2) %>%
  summarise_at(.vars = c("AirTC_avg", "WS_ms", "RH_pct", "BP_mbar", "Srad_Wm2", "SnowDepth_mm"), 
               .funs = c("mean" = mean))

# Hourly Increment (P)
wrcc_sh_p <- wrcc_s %>%
  group_by(DateTime2) %>%
  summarise_at(.vars = c("Precip_mm"), .funs = c("sum" = sum))
wrcc_sh$Precip_mm_sum <- wrcc_sh_p$sum
rm(wrcc_sh_p)


# Hourly Max/Min (T)
wrcc_sh_max <- wrcc_s %>%
  group_by(DateTime2) %>%
  summarise_at(.vars = c("AirTC_max", "RH_max", "WS_ms", "Precip_accum"), .funs = c("max" = max))
colnames(wrcc_sh_max)[1] <- "Date"
wrcc_sh_min <- wrcc_s %>%
  group_by(DateTime2) %>%
  summarise_at(.vars = c("AirTC_min", "RH_min"), .funs = c("min" = min))
colnames(wrcc_sh_min)[1] <- "Date"
colnames(wrcc_sh)[1] <- "Date"

wrcc_sh <- as.data.frame(wrcc_sh)
wrcc_sh_max <- as.data.frame(wrcc_sh_max)
wrcc_sh_min <- as.data.frame(wrcc_sh_min)

wrcc_sh <- bind_cols(wrcc_sh, wrcc_sh_max, wrcc_sh_min)
rm(wrcc_sh_max, wrcc_sh_min)
wrcc_sh <- select(wrcc_sh, -c("Date...9", "Date...14"))
colnames(wrcc_sh)[1] <- "Date"

sapply(wrcc_sh, class)
wrcc_sh$Date <- ymd_hms(wrcc_sh$Date)
sapply(wrcc_sd, class)
wrcc_sd$Date <- ymd(wrcc_sd$Date)

class# Save data files:
setwd("/Volumes/My Passport/Sagehen/nmel-sagehen-met/Data")
saveRDS(wrcc_s, "wrcc_lvl0.rds")
rm(wrcc_s)
saveRDS(wrcc_sd, "wrcc_daily_lvl0.rds")
saveRDS(wrcc_sh, "wrcc_hourly_lvl0.rds")

# COMPARE to ROSE ####
setwd("/Volumes/My Passport/Sagehen/Data Paper Download")
wrcc_sh_R <- read.csv("dfwrcc_hourly_lvl1.csv")
sapply(wrcc_sh_R, class)
wrcc_sh_R$Date <- mdy_hm(wrcc_sh_R$Date)

names(wrcc_sh_R)
names(wrcc_sh)

# Plots #
theme_set(theme(legend.position = "none",panel.background = element_blank(),
                axis.line = element_line(colour = "black")))

# Precip
p1 <- ggplot(wrcc_sh, aes(Date, Precip_accum_max))+
  geom_line()
p2 <- ggplot(wrcc_sh_R, aes(Date, precip_accum_mm_wrcc))+
  geom_line()
grid.arrange(p1, p2, ncol=1)

# Temp
p3 <- ggplot(wrcc_sh, aes(Date, AirTC_avg_mean))+
  geom_line()+
  xlim(as.POSIXct("1997-10-01 00:00:00"), as.POSIXct("2017-09-30 00:00:00"))
p4 <- ggplot(wrcc_sh_R, aes(Date, avgtemp_C_wrcc))+
  geom_line()
grid.arrange(p3, p4, ncol = 1)

# RH
p5 <- ggplot(wrcc_sh, aes(Date, RH_pct_mean))+
  geom_line()+
  xlim(as.POSIXct("1997-10-01 00:00:00"), as.POSIXct("2017-09-30 00:00:00"))
p6 <- ggplot(wrcc_sh_R, aes(Date, RHavg_pct_wrcc))+
  geom_line()
grid.arrange(p5, p6, ncol = 1)

# Solar rad
p7 <- ggplot(wrcc_sh, aes(Date, Srad_Wm2_mean))+
  geom_line()+
  xlim(as.POSIXct("1997-10-01 00:00:00"), as.POSIXct("2017-09-30 00:00:00"))
p8 <- ggplot(wrcc_sh_R, aes(Date, srad_Wm2_wrcc))+
  geom_line()
grid.arrange(p7, p8, ncol = 1)

# 
