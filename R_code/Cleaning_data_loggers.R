# Cleaning EM50 and TinyTag information
#
# Moss project - N2-fixation
# By Emil A.S. Andersen
#
#
#=======  ♣   Libraries     ♣ =======
library(plotly)
library(tidyverse)
library(readxl)
library(lubridate)
#library(hms)
#
#
#
#=======  ♠   TinyTag     ♠ =======
# Load TinyTag temperature measurements
#
# Some information on TinyTag dates and daylight saving time (DST)
# See also https://www.geminidataloggers.com/support/tinytag-explorer-software/dates-and-times
#
#
# TinyTag logs as set by the computer when started, so it is important that time is correct on computer before starting measurements, including timezone!
# Furthermore, when data is downloaded on .ttd files it is kept as when launched. But when opening this .ttd file it is converted based on if it is currently DST or not.
# If the logger is recording from winter, but read in summer, it will display all times in DST, so 18:00 becomes 19:00, but if read in winter it will be 18:00. And vice versa for summer start, measured in winter being changed back one hour
# E.g.
# A measurement is started January 1st and continues to June 1st when data is downloaded.
# When the measurement (.ttd file) is read on June 1st they are converted to DST, so all values have 1 hour added. This is correct for summer, but wrong for winter. If the .ttd file is then read again in December, all values are now no longer DST: Correct for winter measurements, but wrong for summer.
# [I hate DST, why do we need it?!?]
#
#
# Heath habitat
TinyTag_heath_202101 <- read_csv("Data_raw/Loggers/AirT Heath 202009-202101.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_heath_202106 <- read_csv("Data_raw/Loggers/AirT Heath 202101-202106.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_heath_20210824 <- read_csv("Data_raw/Loggers/AirT Heath 20210824.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
#
# New logger as old no longer would download
# Delete first measurement as from indoors!
TinyTag_heath_20220721 <- read_csv("Data_raw/Loggers/AirT Heath 20220721 (until 0711).csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_heath_20220721 <- TinyTag_heath_20220721 %>%
  filter(Record != 1)
#
TinyTag_heath_20220916 <- read_csv("Data_raw/Loggers/AirT Heath 20220916.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
#
TinyTag_heath <- bind_rows(list(TinyTag_heath_202101, TinyTag_heath_202106, TinyTag_heath_20210824, TinyTag_heath_20220721, TinyTag_heath_20220916))
# Change accordingly, if only using average temperature and not max and min 
TinyTag_heath <- TinyTag_heath %>% # Split temperature from the unit and Date and time. Set temperature as numeric
  separate(AirT, sep = " ", into = c("AirT", "Unit")) %>%
  separate(Date_time, sep = " ", into = c("Day", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  mutate(Date = Day,
         Timestamp = Time) %>%
  relocate(Timestamp, .after = Date) %>%
  unite(Day, Time, col = "Day_ID", sep = " ") %>%
  select(!"sec") %>%
  mutate(across(c(AirT), as.numeric))
#
# Graph temperature values
TinyTag_heath %>%  
  mutate(across(Day_ID, ~ymd_hm(.x))) %>% 
  ggplot() + geom_line(aes(x = Day_ID, y = AirT))
#
# Missing values from August to December is from logger no longer connecting. Unsure if data available
#
# Check for duplicates
x <- TinyTag_heath %>%
  distinct()
#
xx <- TinyTag_heath %>%
  filter(n() > 1, .by = c(Day_ID, AirT))
# No duplicates
#
#
# Wetland habitat
TinyTag_wetland_202101 <- read_csv("Data_raw/Loggers/AirT Wetland 202007-202101.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
# All the same values from 202101 are found in the next download in 202102:
TinyTag_wetland_202102 <- read_csv("Data_raw/Loggers/AirT Wetland 20210219.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_wetland_202106 <- read_csv("Data_raw/Loggers/AirT Wetland 202106.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
# All values from this are found in Wetland 20210824. Discrepancies in time follows since this file was created on wintertime, while the other was created on DST!
#
# Duplicate?
#TinyTag_wetland_202106 <- read_csv("Data_raw/Loggers/AirT Wetland all 20210603.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
#
TinyTag_wetland_202108 <- read_csv("Data_raw/Loggers/AirT Wetland 20210824.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_wetland_202112 <- read_csv("Data_raw/Loggers/AirtT Wetland 20211201.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_wetland_202207 <- read_csv("Data_raw/Loggers/AirT Wetland 20220721 (until 0406).csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
#
# Combine
TinyTag_wetland <- bind_rows(list(TinyTag_wetland_202101, TinyTag_wetland_202102, TinyTag_wetland_202106, TinyTag_wetland_202108, TinyTag_wetland_202112, TinyTag_wetland_202207), .id = "id_file")
#
# Separate temperature from unit, remove duplicate measurements. Correct for DST
TinyTag_wetland.1 <- TinyTag_wetland %>%
  separate(AirT, sep = " ", into = c("AirT", "Unit")) %>%           
  separate(Max_Temp, sep = " ", into = c("Max_Temp_C", "UnitMax")) %>%
  separate(Min_Temp, sep = " ", into = c("Min_Temp_C", "UnitMin")) %>%
  mutate(across(AirT, as.numeric)) %>%
  mutate(id_file = case_when(id_file == "1" ~ "w202101",
                             id_file == "2" ~ "w202102",
                             id_file == "3" ~ "w202106",
                             id_file == "4" ~ "w202108",
                             id_file == "5" ~ "w202112",
                             id_file == "6" ~ "w202207")) %>%
  mutate(across(Date_time, ~ymd_hms(.x))) %>%
  # Some changes need to be made as to correct for when measurements were actually taken with respect to DST
  # w202101 was started on DST, but saved in Winter - Not used
  # w202102 was started on DST, but saved in Winter
  # w202106 was started in Winter, and saved in Winter !! - Not used
  # w202108 was started in Winter, but saved on DST
  # w202112 was started on DST, but saved in Winter - Not used
  # w202207 was started on DST, and saved on DST !!
  filter(id_file == "w202102" | id_file == "w202108" | id_file == "w202207") %>%
  #
  # Convert to UTC+1
  # A mistake was made. The entire dataset of any file saved in DST needs to be converted. If DST is wanted for specific times, use the given code.
  mutate(Tid = case_when(#id_file == "w202102" & Date_time < ymd_hm("20201025T03:00") ~ Date_time+hours(1), # No need to change, as it is already on UTC+1
                         #id_file == "w202106" & Date_time > ymd_hm("20210328T03:00") ~ Date_time+hours(1),
                         id_file == "w202108" ~ Date_time-hours(1), # & Date_time < ymd_hm("20210328T03:00") 
                         #id_file == "w202112" & Date_time < ymd_hm("20211031T03:00") ~ Date_time+hours(1),
                         id_file == "w202207" ~ Date_time-hours(1), # & Date_time > ymd_hm("20211031T03:00") & Date_time < ymd_hm("20220327T03:00")
                         TRUE ~ Date_time))
#
# Plot 
TinyTag_wetland.1 %>%
  mutate(across(c(Date_time, Tid), ~ymd_hms(.x))) %>%
  ggplot(aes(x = Tid, y = AirT, color = id_file)) + geom_line()
#
# Separate day and time
TinyTag_wetland.2 <- TinyTag_wetland.1 %>%
  mutate(Date = date(Tid),
         Time = hms::as_hms(Tid)) %>%
  relocate(c(Date, Time, Tid), .after = Record) %>%
  select(!c("id_file", "Date_time", "Unit", "UnitMax", "UnitMin")) %>%
  rename("Date_time" = Tid,
         "AirT_C" = AirT)
#
# Check for duplicates
x <- TinyTag_wetland.2 %>%
  distinct()
#
xx <- TinyTag_wetland.2 %>%
  filter(n() > 1, .by = c(Date_time, AirT_C))
# No duplicates
#
# Save Wetland Air temperature
write_csv(TinyTag_wetland.2, "Data_clean/AirT_wetland.csv", na = "NA")
#
#
#
#=======  ♠   EM50        ♠ =======
# ### EM50 loggers ###
#
#
#   # Heathland 1 #
#    Sensors:
#    P1: 5TM - B
#    P2: 5TM
#    P3: 5TM - Y
#    P4: 5TM
#    P5: 5TM - R
#
# Load data files
# EM3075
Heath1_3jun21 <- read_xls("Data_raw/Loggers/EM50/Heath1/EM3075 3Jun21-1336.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(1:3, 6:7, 10:11) %>%
  filter(Date_time >= ymd_hm("2020-08-28 19:00")) 
#
# EM14979
Heath1_21sep22 <- read_xls("Data_raw/Loggers/EM50/Heath1/[Field Heath 1 20220916]EM14979 21sep22-1014.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(1:3, 6:7, 10:11) %>%
  filter(Date_time >= ymd_hm("2021-06-02 18:00"),
         Date_time <= ymd_hm("2022-09-16 14:00"))
#
# Combine
Heath1 <- bind_rows(Heath1_3jun21, Heath1_21sep22)
#
Heath1 <- Heath1 %>%
  rename("Soil_moisture_B" = Soil_moist1,
         "Soil_temperature_B" = Soil_temp1,
         "Soil_moisture_Y" = Soil_moist3,
         "Soil_temperature_Y" = Soil_temp3,
         "Soil_moisture_R" = Soil_moist5,
         "Soil_temperature_R" = Soil_temp5) %>%
  # Convert to UTC+1
  mutate(Date_time = case_when(Date_time < ymd_hms("2020-12-07 14:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2020-12-07 14:00:00") & Soil_temperature_B <= -2.25 ~ Date_time-hours(1),
                               # New logger installed in DST
                               Date_time >= ymd_hms("2021-06-01 1:00:00") & Date_time < ymd_hms("2021-07-09 17:00:00") ~ Date_time-hours(1),
                               # Red laptop used, mistake in time => duplicate hour!
                               Date_time == ymd_hms("2021-07-09 17:00:00") & Soil_temperature_B == 16 ~ as.Date(NA),
                               Date_time == ymd_hms("2021-07-09 17:00:00") & Soil_temperature_B < 16 ~ Date_time-hours(1),
                               # Back to regular DST (UTC+2)
                               Date_time > ymd_hms("2021-07-09 17:00:00") & Date_time < ymd_hms("2021-11-30 11:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2021-11-30 11:00:00") & Soil_moisture_Y >= 0.143 ~ Date_time-hours(1),
                               TRUE ~ Date_time)) %>%
  filter(!is.na(Date_time))
#
# Note that because of using another laptop in downloading data, it seems an extra measurement was introduced in July 2021. This measurement should be deleted.
# The red laptop, as it is not connected to the internet, has its time slightly off, possibly, which means connecting shortly (16:58) before 17:00 on July 9th meant that it redid the same measurement hour, throwing the entire sequence off by one hour. This applies to multiple loggers, if they were downloaded  slightly before the hour according to that laptop!
#
#
#   # Heathland 2 #
#    Sensors:
#    P1: 5TM - P
#    P2: 5TM
#    P3: 5TM
#    P4: 5TM
#    P5: 5TM - W
#
# Load data file
Heath2 <- read_xls("Data_raw/Loggers/EM50/Heath2/[Field Heath 2 20220916]EM14980 21sep22-1158.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(1:3,10:11) %>%
  filter(Date_time >= ymd_hm("2020-08-28 19:00"),
         Date_time <= ymd_hm("2022-04-30 23:00"))
#
Heath2 <- Heath2 %>%
  rename("Soil_moisture_P" = Soil_moist1,
         "Soil_temperature_P" = Soil_temp1,
         "Soil_moisture_W" = Soil_moist5,
         "Soil_temperature_W" = Soil_temp5) %>%
  # Convert to UTC+1
  mutate(Date_time = case_when(Date_time < ymd_hms("2020-12-07 13:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2020-12-07 13:00:00") & Soil_temperature_P >= -2.25  ~ Date_time-hours(1),
                               Date_time > ymd_hms("2021-06-01 16:00:00") & Date_time < ymd_hms("2021-11-30 11:00:00") ~ Date_time-hours(1),
                               TRUE ~ Date_time))
# 2021-11-30 11:00 -> 2021-11-30 10:00
# But two identical values, so refer to specific location. If length of dataset changes, remember to change location, otherwise it will not shift!
Heath2$Date_time[11009] <- Heath2$Date_time[11009]-hours(1)
#
#
#   # Heathland 3 #
#    Sensors:
#    P1: 5TM - G
#    P2: 5TM - Ra
#    P3: 5TM - Au
#    P4: 5TM - Hy
#    P5: PAR
#
# Load data file
Heath3 <- read_xls("Data_raw/Loggers/EM50/Heath3/[Field Heath 3 20220916]EM14973 22sep22-1658.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "PAR"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  filter(Date_time >= ymd_hm("2020-08-28 19:00"),
         Date_time <= ymd_hm("2022-09-16 14:00"))
#
Heath3 <- Heath3 %>%
  rename("Soil_moisture_G" = Soil_moist1,
         "Soil_temperature_G" = Soil_temp1,
         "Soil_moisture_Ra" = Soil_moist2,
         "Soil_temperature_Ra" = Soil_temp2,
         "Soil_moisture_Au" = Soil_moist3,
         "Soil_temperature_Au" = Soil_temp3,
         "Soil_moisture_Hy" = Soil_moist4,
         "Soil_temperature_Hy" = Soil_temp4) %>%
  # Convert to UTC+1
  mutate(Date_time = case_when(Date_time < ymd_hms("2020-12-07 14:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2020-12-07 14:00:00") & Soil_temperature_G <= -3.4 ~ Date_time-hours(1),
                               Date_time >= ymd_hms("2021-06-02 18:00:00") & Date_time < ymd_hms("2021-07-09 17:00:00") ~ Date_time-hours(1),
                               # Red laptop used, mistake in time => duplicate hour!
                               Date_time == ymd_hms("2021-07-09 17:00:00") & Soil_temperature_G >= 13.8 ~ as.Date(NA),
                               Date_time == ymd_hms("2021-07-09 17:00:00") & Soil_temperature_G <= 13.8 ~ Date_time-hours(1),
                               Date_time > ymd_hms("2021-07-09 17:00:00") & Date_time < ymd_hms("2021-12-01 10:00:00") ~ Date_time-hours(1),
                               TRUE ~ Date_time))
# 2021-11-30 10:00 -> 2021-11-30 9:00
# But two identical values, so refer to specific location. If length of dataset changes, remember to change location, otherwise it will not shift!
Heath3$Date_time[11033] <- Heath3$Date_time[11033]-hours(1)
# Remove the extra time measurement in July
Heath3 <- Heath3  %>%
  filter(!is.na(Date_time))
#
# Note that because of using another laptop in downloading data, it seems an extra measurement was introduced in July 2021. This measurement should be deleted.
# The red laptop, as it is not connected to the internet, has its time slightly off, possibly, which means connecting shortly (16:58) before 17:00 on July 9th meant that it redid the same measurement hour, throwing the entire sequence off by one hour. This applies to multiple loggers, if they were downloaded  slightly before the hour according to that laptop!
#
#
#   # Heathland 4 #
#    Sensors:
#    P1: 5TM - DI
#    P2: 5TM - Pti
#    P3: 5TM - Pl
#    P4: 5TM - Po
#    P5: 5TM
#
# Load data file
Heath4 <- read_xls("Data_raw/Loggers/EM50/Heath4/[Field Heath 4 20220916]EM14991 22sep22-1640.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(1:9) %>%
  filter(Date_time >= ymd_hm("2020-08-28 18:00"), # No sensors logging before 09-01 18:00
         Date_time <= ymd_hm("2022-09-16 14:00"))
#
Heath4 <- Heath4 %>%
  rename("Soil_moisture_Di" = Soil_moist1,
         "Soil_temperature_Di" = Soil_temp1,
         "Soil_moisture_Pti" = Soil_moist2,
         "Soil_temperature_Pti" = Soil_temp2,
         "Soil_moisture_Pl" = Soil_moist3,
         "Soil_temperature_Pl" = Soil_temp3,
         "Soil_moisture_Po" = Soil_moist4,
         "Soil_temperature_Po" = Soil_temp4) %>%
  # Convert to UTC+1
  mutate(Date_time = case_when(Date_time < ymd_hms("2020-12-07 14:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2020-12-07 14:00:00") & Soil_temperature_Pl <= -4 ~ Date_time-hours(1),
                               Date_time >= ymd_hms("2021-06-02 17:00:00") & Date_time < ymd_hms("2021-07-09 17:00:00") ~ Date_time-hours(1),
                               # Red laptop used, mistake in time => duplicate hour!
                               Date_time == ymd_hms("2021-07-09 17:00:00") & Soil_temperature_Pl >= 21 ~ as.Date(NA),
                               Date_time == ymd_hms("2021-07-09 17:00:00") & Soil_temperature_Pl <= 21 ~ Date_time-hours(1),
                               Date_time > ymd_hms("2021-07-09 17:00:00") & Date_time < ymd_hms("2021-11-30 11:00:00") ~ Date_time-hours(1),
                               TRUE ~ Date_time))
# 2021-11-30 11:00 -> 2021-11-30 10:00
# But two identical values, so refer to specific location. If length of dataset changes, remember to change location, otherwise it will not shift!
Heath4$Date_time[11011] <- Heath4$Date_time[11011]-hours(1)
# Remove the extra time measurement in July
Heath4 <- Heath4 %>%
  filter(!is.na(Date_time))
#
# Note that because of using another laptop in downloading data, it seems an extra measurement was introduced in July 2021. This measurement should be deleted.
# The red laptop, as it is not connected to the internet, has its time slightly off, possibly, which means connecting shortly (16:58) before 17:00 on July 9th meant that it redid the same measurement hour, throwing the entire sequence off by one hour. This applies to multiple loggers, if they were downloaded  slightly before the hour according to that laptop!
#
#
#   # Combine Heathland #
Heath <- tibble(seq(from = ymd_hms('2020-08-28 17:00:00'), to = ymd_hms('2022-09-16 14:00:00'), by='hours')) %>%
  rename("Date_time" = "seq(...)")
#
Heath <- reduce(list(Heath, Heath1, Heath2, Heath3, Heath4), left_join, by = "Date_time")
#
#
#   # Wetland 1 #
#    Sensors:
#    P1: 5TM - B (wet)
#    P2: 5TM - B (Sf, extra plot)
#    P3: 5TM
#    P4: 5TM - Y (wet)
#    P5: 5TM - Y
#
# Load data file
Wetland1 <- read_xls("Data_raw/Loggers/EM50/Wetland1/[Field Wetland 1 20220929]EM4304 4okt22-1641.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(1:5, 8:11) %>%
  filter(Date_time >= ymd_hm("2020-08-28 17:00"),
         Date_time <= ymd_hm("2022-09-29 9:00"))
#
Wetland1 <- Wetland1 %>%
  rename("Soil_moisture_Bwet" = Soil_moist1,
         "Soil_temperature_Bwet" = Soil_temp1,
         "Soil_moisture_B" = Soil_moist2,
         "Soil_temperature_B" = Soil_temp2,
         "Soil_moisture_Ywet" = Soil_moist4,
         "Soil_temperature_Ywet" = Soil_temp4,
         "Soil_moisture_Y" = Soil_moist5,
         "Soil_temperature_Y" = Soil_temp5) %>%
  # Convert to UTC+1
  mutate(Date_time = case_when(Date_time < ymd_hms("2020-12-07 13:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2020-12-07 13:00:00") & Soil_temperature_Ywet >= -1 ~ Date_time-hours(1),
                               Date_time >= ymd_hms("2021-06-03 10:00:00") & Date_time < ymd_hms("2021-12-01 10:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2021-12-01 10:00:00") & Soil_moisture_Y >= 0.295 ~ Date_time-hours(1),
                               TRUE ~ Date_time))
#
#
#   # Wetland 2 #
#    Sensors:
#    P1: 5TM - W
#    P2: 5TM - W (wet)
#    P3: PAR
#    P4: 5TM - P (wet)
#    P5: 5TM - P
#
# Load data file
Wetland2 <- read_xls("Data_raw/Loggers/EM50/Wetland2/[Field Wetland 2 20220929]EM4305 4okt22-1721.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "PAR", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(1:10) %>%
  filter(Date_time >= ymd_hm("2020-08-28 18:00"),
         Date_time <= ymd_hm("2022-09-29 9:00"))
#
Wetland2 <- Wetland2 %>%
  rename("Soil_moisture_W" = Soil_moist1,
         "Soil_temperature_W" = Soil_temp1,
         "Soil_moisture_Wwet" = Soil_moist2,
         "Soil_temperature_Wwet" = Soil_temp2,
         "Soil_moisture_Pwet" = Soil_moist4,
         "Soil_temperature_Pwet" = Soil_temp4,
         "Soil_moisture_P" = Soil_moist5,
         "Soil_temperature_P" = Soil_temp5) %>%
  # Convert to UTC+1
  mutate(Date_time = case_when(Date_time < ymd_hms("2020-12-07 13:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2020-12-07 13:00:00") & Soil_temperature_P >= -1.9 ~ Date_time-hours(1),
                               Date_time >= ymd_hms("2021-06-03 10:00:00") & Date_time < ymd_hms("2021-12-01 10:00:00") ~ Date_time-hours(1),
                               Date_time == ymd_hms("2021-12-01 10:00:00") & is.na(Soil_temperature_W) ~ Date_time-hours(1),
                               TRUE ~ Date_time))
#
#
#   # Wetland 3 #
#    Sensors:
#    P1: 5TM - G
#    P2: 5TM - G (S)
#    P3: 5TM - G (Sf)
#    P4: 5TM - G (Sli)
#    P5: 5TM - R
#
# Load data file
Wetland3 <- read_xls("Data_raw/Loggers/EM50/Wetland3/[Field Wetland 3 20220929]]EM14982 4okt22-1702.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")) %>%
  select(1:11) %>%
  filter(Date_time >= ymd_hm("2020-08-28 18:00"))
#
Wetland3 <- Wetland3 %>%
  rename("Soil_moisture_G" = Soil_moist1,
         "Soil_temperature_G" = Soil_temp1,
         "Soil_moisture_S" = Soil_moist2,
         "Soil_temperature_S" = Soil_temp2,
         "Soil_moisture_Sf" = Soil_moist3,
         "Soil_temperature_Sf" = Soil_temp3,
         "Soil_moisture_Sli" = Soil_moist4,
         "Soil_temperature_Sli" = Soil_temp4,
         "Soil_moisture_R" = Soil_moist5,
         "Soil_temperature_R" = Soil_temp5) %>%
  # Convert to UTC+1
  mutate(Date_time = case_when(Date_time < ymd_hms("2020-12-07 13:00:00") ~ Date_time-hours(1),
                               Date_time >= ymd_hms("2021-06-03 10:00:00") & Date_time <= ymd_hms("2021-12-01 10:00:00") ~ Date_time-hours(1),
                               TRUE ~ Date_time))
# 2021-12-01 10:00 -> 2021-12-01 9:00
# But two identical values, so refer to specific location. If length of dataset changes, remember to change location, otherwise it will not shift!
Wetland3$Date_time[2420] <- Wetland3$Date_time[2420]-hours(1)
Wetland3 <- Wetland3 %>%
  # A measurement is missing, because the logger was connected just before 10, so the measurement was done and next was 11, and that matched in the logger
  add_row(tibble_row(Date_time = ymd_hms("2021-12-01 10:00:00")), .after = 11033)
#
#
#   # Combine Wetland #
Wetland <- tibble(seq(from = ymd_hms('2020-08-28 17:00:00'), to = ymd_hms('2022-09-29 9:00:00'), by='hours')) %>%
  rename("Date_time" = "seq(...)")

Wetland <- reduce(list(Wetland, Wetland1, Wetland2, Wetland3), left_join, by = "Date_time")


#
# CAREFUL!
1
#
#
# Save Heathland and Wetland logger data
write_csv(Heath, "Data_clean/Heath_EM50.csv", na = "NA")
write_csv(Wetland, "Data_clean/Wetland_EM50.csv", na = "NA")

# Heath <- read_csv("Data_clean/Heath_EM50.csv", col_names = TRUE)





# Outliers
# Soil Temperature and moisture
# Temperature
Heath_temp <- Heath %>%
  select(1,3,5,7,9,11,13,15,17,19,22,24,26,28) %>%
  pivot_longer(cols = c(2:14), names_to = "Sensor", values_to = "Soil_temperature")
#
Heath_temp %>%
  ggplot() +
  geom_point(aes(x = Date_time, y = Soil_temperature)) + facet_wrap(~Sensor)
#
# Temperature per block
plot_ly(Heath, x = ~Soil_temperature_B, y = ~Date_time, name = "Blue", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  add_trace(x = ~Soil_temperature_P, y = ~Date_time, name = "Purple",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Soil_temperature_R, y = ~Date_time, name = "Red",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Soil_temperature_W, y = ~Date_time, name = "White",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Soil_temperature_Y, y = ~Date_time, name = "Yellow",type = 'scatter', mode = "markers", marker = list(color = "#F0E442")) %>%
  add_trace(x = ~Soil_temperature_G, y = ~Date_time, name = "Green",type = 'scatter', mode = "markers", marker = list(color ="black")) %>%
  layout(title = "Soil temperature", xaxis = list(title = "Soil temperature (°C)"), margin = list(l = 100))
#
# Temperature per species
plot_ly(Heath, x = ~Soil_temperature_Au, y = ~Date_time, name = "Aulacomnium turgidum", type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Soil_temperature_Di, y = ~Date_time, name = "Dicranum scoparium",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Soil_temperature_Hy, y = ~Date_time, name = "Hylocomium splendens",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Soil_temperature_Pl, y = ~Date_time, name = "Pleurozium schreberi",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Soil_temperature_Po, y = ~Date_time, name = "Polytrichum commune",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Soil_temperature_Pti, y = ~Date_time, name = "Ptilidium ciliare",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Soil_temperature_Ra, y = ~Date_time, name = "Racomitrium lanuginosum",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Soil_temperature_G, y = ~Date_time, name = "Green",type = 'scatter', mode = "markers", marker = list(color ="black")) %>%
  layout(title = "Soil moisture per species", xaxis = list(title = "Soil moisture (%vol)"), margin = list(l = 100))


#
# Moisture
Heath_moist.1 <- Heath %>%
  select(1:2,4,6,8,10,12,14,16,18,21,23,25,27) %>%
  pivot_longer(cols = c(2:14), names_to = "Sensor", values_to = "Soil_moisture")

Heath_moist.2 <- Heath %>%
  select(1:2,4,6,8,10,12,14,16,18,21,23,25,27) %>%
  mutate(across(c(2:14), ~ .x*100))

#
Heath_moist.1 %>%
  ggplot() +
  geom_point(aes(x = Date_time, y = Soil_moisture)) + facet_wrap(~Sensor)

# Moisture per block
plot_ly(Heath_moist.2, x = ~Soil_moisture_B, y = ~Date_time, name = "Blue", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  add_trace(x = ~Soil_moisture_P, y = ~Date_time, name = "Purple",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Soil_moisture_R, y = ~Date_time, name = "Red",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Soil_moisture_W, y = ~Date_time, name = "White",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Soil_moisture_Y, y = ~Date_time, name = "Yellow",type = 'scatter', mode = "markers", marker = list(color = "#F0E442")) %>%
  layout(title = "Soil moisture", xaxis = list(title = "Soil moisture (%vol)"), margin = list(l = 100))
#
# Moisture per species
plot_ly(Heath_moist.2, x = ~Soil_moisture_Au, y = ~Date_time, name = "Aulacomnium turgidum", type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Soil_moisture_Di, y = ~Date_time, name = "Dicranum scoparium",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Soil_moisture_Hy, y = ~Date_time, name = "Hylocomium splendens",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Soil_moisture_Pl, y = ~Date_time, name = "Pleurozium schreberi",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Soil_moisture_Po, y = ~Date_time, name = "Polytrichum commune",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Soil_moisture_Pti, y = ~Date_time, name = "Ptilidium ciliare",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Soil_moisture_Ra, y = ~Date_time, name = "Racomitrium lanuginosum",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Soil_moisture_G, y = ~Date_time, name = "Green",type = 'scatter', mode = "markers", marker = list(color ="black")) %>%
  layout(title = "Soil moisture per species", xaxis = list(title = "Soil moisture (%vol)"), margin = list(l = 100))





# Too many data points. Takes a looooooooooooong time
# dotchart(Heath_temp$Soil_temperature, 
#          main="Cleveland plot - Soil temperature", xlab = "Observed values", 
#          pch = 19, color = hcl.colors(12), 
#          labels = Heath_temp$Sensor,
#          #groups = Heath$,
#          gpch = 12, gcolor = 1)





# Remove empty columns and keep the sensor where PAR was measured on
fDay_ID <- flux_all %>% select(Day_ID) %>% distinct(Day_ID)
#
fPAR_removal <- fDay_ID %>%
  left_join(fPAR, by = "Day_ID") %>%
  select(c("PAR1", "PAR2", "PAR3", "PAR4", "PAR5"))
PAR_colSums <- (colSums(fPAR_removal, na.rm=T) != 0) # T if colSum is not 0, F otherwise
Positive_PAR  <- fPAR_removal[, PAR_colSums] %>% # all the non-zero columns
  add_column(fDay_ID, .before = TRUE)
Positive_PAR <- Positive_PAR %>% rename("PAR" = colnames(Positive_PAR[2]))
#
# Combine, Temperature and PAR
flux_all_TempPAR <- full_join(flux_all_Temp, Positive_PAR, by = "Day_ID")
#
#
#
#=======  ■  { The End }    ■ =======
