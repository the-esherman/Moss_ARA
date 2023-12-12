# Cleaning EM50 and TinyTag information
#
# Moss project - N2-fixation
# By Emil A.S. Andersen
#
#
#=======  ♣   Libraries     ♣ =======
library(tidyverse)
library(readxl)
library(lubridate)
#
#
#
#=======  ♠   TinyTag     ♠ =======
# Load TinyTag temperature measurements
#
TinyTag_heath1 <- read_csv("Data_raw/Loggers/AirT Heath 202009-202101.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_heath2 <- read_csv("Data_raw/Loggers/AirT Heath 202101-202106.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_heath3 <- read_csv("Data_raw/Loggers/AirT Heath 20220721 (until 0711).csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
TinyTag_heath4 <- read_csv("Data_raw/Loggers/AirT Heath 20220916.csv", skip = 5, col_names = c("Record", "Date_time", "Max_Temp", "AirT", "Min_Temp"))
#
TinyTag_heath <- bind_rows(list(TinyTag_heath1, TinyTag_heath2, TinyTag_heath3, TinyTag_heath4))
# Change accordingly, if only using average temperature and not max and min 
TinyTag_heath <- TinyTag_heath %>% # Split temperature from the unit and Date and time. Set temperature as numeric
  separate(AirT, sep = " ", into = c("AirT", "Unit")) %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  unite(hour, min, col = "Time", sep = ":") %>%
  unite(Date, Time, col = "Day_ID", sep = " ") %>%
  select(!"sec") %>%
  mutate(across(c(AirT), as.numeric))
#

TinyTag_heath %>%  
  mutate(across(Day_ID, ~ymd_hm(.x))) %>% 
  ggplot() + geom_line(aes(x = Day_ID, y = AirT))


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
# ═══════════════════════════════════╗
#                                    ▼
Heath1 <- read_xls("Data_raw/Loggers/.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
#
Heath1.loaded <- Heath1
Heath1 <- Heath1.loaded
#
Heath1 <- Heath1 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  separate(Date, sep = "-", into = c("Year", "Month", "Day")) %>%
  filter((Year == "2020" 
          & ((Month == "08" & (Day == "29" | Day == "30" | Day == "31"))
             | Month == "09" 
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
Heath1.NAcol <-names(Heath1)[sapply(Heath1, function(x) sum(is.na(x)) == length(x))] # Columns with only NA
Heath1.x <- Heath1 %>%
  select(-Heath1.NAcol)
#unite(hour, min, col = "Time", sep = ":") %>%
#unite(Date, Time, col = "Day_ID", sep = " ")
#
#
#   # Heathland 2 #
#    Sensors:
#    P1: 5TM - P
#    P2: 5TM
#    P3: 5TM
#    P4: 5TM
#    P5: 5TM - W
# ═══════════════════════════════════╗
#                                    ▼
Heath2 <- read_xls("Data_raw/Loggers/[Field Heath 2 20220916]EM14980 21sep22-1158.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "Soil_moist5", "Soil_temp5"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
#
Heath2.loaded <- Heath2
Heath2 <- Heath2.loaded
#
Heath2 <- Heath2 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  separate(Date, sep = "-", into = c("Year", "Month", "Day")) %>%
  filter((Year == "2020" 
          & ((Month == "08" & (Day == "29" | Day == "30" | Day == "31"))
             | Month == "09" 
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
Heath2.NAcol <-names(Heath2)[sapply(Heath2, function(x) sum(is.na(x)) == length(x))] # Columns with only NA
Heath2.x <- Heath2 %>%
  select(-Heath2.NAcol)
  #unite(hour, min, col = "Time", sep = ":") %>%
  #unite(Date, Time, col = "Day_ID", sep = " ")
#
#
#   # Heathland 3 #
#    Sensors:
#    P1: 5TM - G
#    P2: 5TM - Ra
#    P3: 5TM - Au
#    P4: 5TM - Hy
#    P5: PAR
# ═══════════════════════════════════╗
#                                    ▼
Heath3 <- read_xls("Data_raw/Loggers/[Field Heath 3 20220916]EM14973 22sep22-1658.xls", col_names = c("Date_time", "Soil_moist1", "Soil_temp1", "Soil_moist2", "Soil_temp2", "Soil_moist3", "Soil_temp3", "Soil_moist4", "Soil_temp4", "PAR"), skip = 3, col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
#
Heath3.loaded <- Heath3
Heath3 <- Heath3.loaded
#
Heath3 <- Heath3 %>%
  separate(Date_time, sep = " ", into = c("Date", "Time")) %>%
  separate(Time, sep = ":", into = c("hour", "min", "sec")) %>%
  separate(Date, sep = "-", into = c("Year", "Month", "Day")) %>%
  filter((Year == "2020" 
          & ((Month == "08" & (Day == "29" | Day == "30" | Day == "31"))
             | Month == "09" 
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
        | Month == "05" 
        | Month == "06" 
        | Month == "07" 
        | Month == "08" 
        | (Month == "09" & (Day == "01" | Day == "02" | Day == "03" | Day == "04" | Day == "05" | Day == "06" | Day == "07" | Day == "08" | Day == "09" | Day == "10" | Day == "11" | Day == "12" | Day == "13" | Day == "14" | Day == "15" | Day == "16")
           )
     )
  )
  )
Heath3.NAcol <-names(Heath3)[sapply(Heath3, function(x) sum(is.na(x)) == length(x))] # Columns with only NA
Heath3.x <- Heath3 %>%
  select(-Heath3.NAcol)
#unite(hour, min, col = "Time", sep = ":") %>%
#unite(Date, Time, col = "Day_ID", sep = " ")
#
#
#   # Heathland 4 #
#    Sensors:
#    P1: 5TM - DI
#    P2: 5TM - Pti
#    P3: 5TM - Pl
#    P4: 5TM - Po
#    P5: 5TM
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
