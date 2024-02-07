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
library(hms)
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
  mutate(Tid = case_when(id_file == "w202102" & Date_time < ymd_hm("20201025T03:00") ~ Date_time+hours(1),
                         #id_file == "w202106" & Date_time > ymd_hm("20210328T03:00") ~ Date_time+hours(1),
                         id_file == "w202108" & Date_time < ymd_hm("20210328T03:00") ~ Date_time-hours(1),
                         #id_file == "w202112" & Date_time < ymd_hm("20211031T03:00") ~ Date_time+hours(1),
                         id_file == "w202207" & Date_time > ymd_hm("20211031T03:00") & Date_time < ymd_hm("20220327T03:00") ~ Date_time-hours(1),
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
         Time = as_hms(Tid)) %>%
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
heath1_path <- "Data_raw/Loggers/EM50/Heath1/"
heath1_folder <- dir(heath1_path)

heath1_list <- list()

for (file in heath1_folder){
  
  # Load data: all 5TM soil temperature/moisture sensors
  heath1_data <- read_xls(paste(heath1_path,file, sep = ""), col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  
  # Retain only port 1, 3, 5
  heath1_data <- heath1_data[c(1:3,6:7,10:11)]
  
  # Add file id to new column
  heath1_data$id <- substr(file,1,20)
  
  # Name each file uniquely, based on filename. Add to list
  heath1_list[[substr(file,1,20)]] <- heath1_data
  
  rm(heath1_data)
}


# Heath1_all <- heath1_list %>% reduce(full_join, by = "Date_time")
# all_heath1 <- do.call(rbind, heath1_list)



# Only need two files:
# EM3075
Heath1_3jun21 <- heath1_list[["EM3075 3Jun21-1336.x"]] %>%
  filter(Date_time >= ymd_hm("2020-08-28 19:00")) 
# EM14979
Heath1_30nov21 <- heath1_list[["[Field Heath 1 20220"]] %>%
  filter(Date_time >= ymd_hm("2021-06-02 18:00"),
         Date_time <= ymd_hm("2022-09-16 14:00")) 

Heath1 <- bind_rows(Heath1_3jun21, Heath1_30nov21)

# Plot diel temperature of each sensor or average of sensors
Heath1 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  group_by(ymd(Date)) %>%
  summarise(Soil_moist1 = mean(Soil_moist1, na.rm = TRUE),
            Soil_temp1 = mean(Soil_temp1, na.rm = TRUE),
            Soil_moist3 = mean(Soil_moist3, na.rm = TRUE),
            Soil_temp3 = mean(Soil_temp3, na.rm = TRUE),
            Soil_moist5 = mean(Soil_moist5, na.rm = TRUE),
            Soil_temp5 = mean(Soil_temp5, na.rm = TRUE),
            .groups = "keep") %>%
  rename("Date" = "ymd(Date)") %>%
  #mutate(Soil_temp = mean(c(Soil_temp1, Soil_temp3, Soil_temp5), na.rm = TRUE)) %>%
  ungroup() %>%
  #ggplot(aes(Date, Soil_temp)) + geom_point()
  select(1,3,5,7) %>%
  pivot_longer(c(Soil_temp1, Soil_temp3, Soil_temp5), names_to = "Sensor", values_to = "Soil_temp") %>%
  #pivot_longer(c(Soil_moist1, Soil_moist3, Soil_moist5), names_to = "Sensor2", values_to = "Soil_moist") %>%
  ggplot(aes(Date, Soil_temp, shape = Sensor)) + geom_point()
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
#
# Plot diel temperature of each sensor or average of sensors
Heath2 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  group_by(ymd(Date)) %>%
  summarise(Soil_moist1 = mean(Soil_moist1, na.rm = TRUE),
            Soil_temp1 = mean(Soil_temp1, na.rm = TRUE),
            Soil_moist5 = mean(Soil_moist5, na.rm = TRUE),
            Soil_temp5 = mean(Soil_temp5, na.rm = TRUE),
            .groups = "keep") %>%
  rename("Date" = "ymd(Date)") %>%
  #mutate(Soil_temp = mean(c(Soil_temp1, Soil_temp3, Soil_temp5), na.rm = TRUE)) %>%
  ungroup() %>%
  #ggplot(aes(Date, Soil_temp)) + geom_point()
  select(1,3,5) %>%
  pivot_longer(c(Soil_temp1, Soil_temp5), names_to = "Sensor", values_to = "Soil_temp") %>%
  #pivot_longer(c(Soil_moist1, Soil_moist3, Soil_moist5), names_to = "Sensor2", values_to = "Soil_moist") %>%
  ggplot(aes(Date, Soil_temp, shape = Sensor)) + geom_point()
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
#
# Plot diel temperature of each sensor or average of sensors
Heath3 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  group_by(ymd(Date)) %>%
  summarise(Soil_moist1 = mean(Soil_moist1, na.rm = TRUE),
            Soil_temp1 = mean(Soil_temp1, na.rm = TRUE),
            Soil_moist2 = mean(Soil_moist2, na.rm = TRUE),
            Soil_temp2 = mean(Soil_temp2, na.rm = TRUE),
            Soil_moist3 = mean(Soil_moist3, na.rm = TRUE),
            Soil_temp3 = mean(Soil_temp3, na.rm = TRUE),
            Soil_moist4 = mean(Soil_moist4, na.rm = TRUE),
            Soil_temp4 = mean(Soil_temp4, na.rm = TRUE),
            PAR = mean(PAR, na.rm = TRUE),
            .groups = "keep") %>%
  rename("Date" = "ymd(Date)") %>%
  #mutate(Soil_temp = mean(c(Soil_temp1, Soil_temp3, Soil_temp5), na.rm = TRUE)) %>%
  ungroup() %>%
  #ggplot(aes(Date, Soil_temp)) + geom_point()
  select(1,3,5,7,9) %>%
  pivot_longer(c(Soil_temp1, Soil_temp2, Soil_temp3, Soil_temp4), names_to = "Sensor", values_to = "Soil_temp") %>%
  #pivot_longer(c(Soil_moist1, Soil_moist3, Soil_moist5), names_to = "Sensor2", values_to = "Soil_moist") %>%
  ggplot(aes(Date, Soil_temp, shape = Sensor)) + geom_point()






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
  filter(Date_time >= ymd_hm("2020-08-28 19:00"), # No sensors logging before 09-01 18:00
         Date_time <= ymd_hm("2022-09-16 14:00"))
#
#
# Plot diel temperature of each sensor or average of sensors
Heath4 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  group_by(ymd(Date)) %>%
  summarise(Soil_moist1 = mean(Soil_moist1, na.rm = TRUE),
            Soil_temp1 = mean(Soil_temp1, na.rm = TRUE),
            Soil_moist2 = mean(Soil_moist2, na.rm = TRUE),
            Soil_temp2 = mean(Soil_temp2, na.rm = TRUE),
            Soil_moist3 = mean(Soil_moist3, na.rm = TRUE),
            Soil_temp3 = mean(Soil_temp3, na.rm = TRUE),
            Soil_moist4 = mean(Soil_moist4, na.rm = TRUE),
            Soil_temp4 = mean(Soil_temp4, na.rm = TRUE),
            .groups = "keep") %>%
  rename("Date" = "ymd(Date)") %>%
  #mutate(Soil_temp = mean(c(Soil_temp1, Soil_temp3, Soil_temp5), na.rm = TRUE)) %>%
  ungroup() %>%
  #ggplot(aes(Date, Soil_temp)) + geom_point()
  select(1,3,5,7,9) %>%
  pivot_longer(c(Soil_temp1, Soil_temp2, Soil_temp3, Soil_temp4), names_to = "Sensor", values_to = "Soil_temp") %>%
  #pivot_longer(c(Soil_moist1, Soil_moist3, Soil_moist5), names_to = "Sensor2", values_to = "Soil_moist") %>%
  ggplot(aes(Date, Soil_temp, shape = Sensor)) + geom_point()






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







heath2_path <- "Data_raw/Loggers/EM50/Heath3/"
heath2_folder <- dir(heath2_path)

heath2_list <- list()

for (file in heath2_folder){
  
  # Load data: all 5TM soil temperature/moisture sensors
  heath2_data <- read_xls(paste(heath2_path,file, sep = ""), col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  
  # Retain only port 1, 5
  heath2_data <- heath2_data[c(1:3,10:11)]
  
  # Add file id to new column
  heath2_data$id <- substr(file,1,20)
  
  # Name each file uniquely, based on filename. Add to list
  heath2_list[[substr(file,1,20)]] <- heath2_data
  
  rm(heath2_data)
}




# ═══════════════════════════════════╗
#                                    ▼
Heath4 <- read_xls("Data_raw/Loggers/[Field Heath 4 20220916]EM14991 22sep22-1640.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
#
Heath4.loaded <- Heath4
Heath4 <- Heath4.loaded
#
Heath4 <- Heath4 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  separate(Date, sep = "-", into = c("Year", "Month", "Day")) %>%
  filter((Year == "2020" 
          & (Month == "09"
             | Month == "10" 
             | Month == "11" 
             | Month == "12"
          )
  ) 
  | Year == "2021" 
  | (Year == "2022" 
     & (Month == "01" 
        | Month == "02" 
        | Month == "03" 
        | Month == "04" 
        #| Month == "05" 
        #| Month == "06" 
        #| Month == "07" 
        #| Month == "08" 
        #| Month == "09"
     )
  )
  )
Heath4.NAcol <-names(Heath4)[sapply(Heath4, function(x) sum(is.na(x)) == length(x))] # Columns with only NA
Heath4.x <- Heath4 %>%
  select(-Heath4.NAcol)
#unite(hour, min, col = "Time", sep = ":") %>%
#unite(Date, Time, col = "Day_ID", sep = " ")
#







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
