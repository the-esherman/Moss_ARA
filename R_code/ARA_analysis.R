# Bryophyte experiment - N2-fixation
# Script author: Emil A.S. Andersen
#
# Analysis - ARA
#
# 
#=======  ♣   Libraries     ♣ =======
library(plotly)
library(tidyverse)
library(readxl)
library(lubridate)
library(car)
library(nlme)
library(glmmTMB)
library(emmeans)
library(vegan)
library(cowplot)
library(ggrepel)
#library(lme4)
#
#
#
#=======  ♠   Load data     ♠ =======
# Import ID's
ID_info <- read_xlsx("Data_raw/Data_ID.xlsx")
ID_15N <- read_xlsx("Data_raw/ID_15N.xlsx")
#
#    ╔═══════╗
# -- • Field • --
#    ╚═══════╝
#
# Import field datasets
field_ARA <- read_csv("Data_clean/field_ARA.csv", col_names = TRUE)
#
# Bryophyte density count
densityCount <- read_xlsx("Data_raw/Moss counts.xlsx")
densityArea <- (2.2/2)^2*pi # Area of one sample for density count in cm2
#
# The small corers that were used for density and for selecting vial samples:
# Inner diameter: 2.2cm
# Outer diameter: 2.5cm
#
# Air temperature
AirT_wetland <- read_csv("Data_clean/AirT_wetland.csv", col_names = TRUE)
AirT_heath <- read_csv("Data_clean/AirT_heath.csv", col_names = TRUE)
#
# Soil parameters
EM50_Heath <- read_csv("Data_clean/Heath_EM50.csv", col_names = TRUE)
EM50_Wetland <- read_csv("Data_clean/Wetland_EM50.csv", col_names = TRUE)
#
# Chamber size
Ch_H <- 7 # Chamber height (cm) above moss surface. This needs to be changed to match each plot estimated height
Ch_r <- 5 # Chamber radius (cm)
Ch_vol_L <- (Ch_r^2*pi*Ch_H)/1000 # Chamber vol in L
Ch_area_m2 <- (Ch_r^2*pi)/10000 # Chamber area in m2
#
#    ╔═══════╗
# -- • Vials • --
#    ╚═══════╝
#
# Import vial dataset
vial_ARA <- read_csv("Data_clean/vial_ARA.csv", col_names = TRUE)
vial_15N <- read_csv("Data_clean/vial_15N.csv", col_names = TRUE)
#
# Climate chamber environmental parameters
AirT_CC <- read_csv("Data_clean/AirT_CC.csv", col_names = TRUE)
PAR_CC <- read_csv("Data_clean/PAR_CC.csv", col_names = TRUE)
#
# Vial moisture
vial_moisture <- read_csv("Data_clean/vial_moisture.csv", col_names = TRUE)
#
# Vial size
Vial_vol_L <- 20/1000 # 20mL vials, the small ones, and 22 for the taller ones. Does not take into account the actual headspace after mosses
Vial_area_m2 <- (1.1^2*pi*2)/10000
#
Vial_sample_area_m2 <- (2.5^2*pi*2)/10000
#
#    ╔═══════════╗
# -- • Constants • --
#    ╚═══════════╝
#
# Gas constant ~ 8.31446261815324 L kPa K^-1 µmol^-1
# Avogadro constant × Boltzmann constant: N_A × k
# Eite Tiesinga, Peter J. Mohr, David B. Newell, Barry N. Taylor; CODATA Recommended Values of the Fundamental Physical Constants: 2018. J. Phys. Chem. Ref. Data 1 September 2021; 50 (3): 033105. https://doi.org/10.1063/5.0064853
R_const <- 6.02214076*10^23 * 1.380649*10^(-23)
#
# Pressure: 101.3 kPa
p <- 101.3
#
#
#
#=======  ►   Functions     ◄ =======
# From http://www.cookbook-r.com/Manipulating_data/Summarizing_data/
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- plyr::ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm),
                     max  = max    (xx[[col]], na.rm=na.rm),
                     min  = min    (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult 
  return(datac)
}
#
# From https://stackoverflow.com/a/7549819
# Get linear equation and R2 for plot
lm_eqn <- function(x, y) {
  m <- lm(y ~ x);
  eq <- substitute(atop(italic(y) == a + b %.% italic(x), italic(R)^2~"="~R2*","~~italic(p)~"="~pvalue),
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        R2 = format(summary(m)$r.squared, digits = 3),
                        pvalue = ifelse(summary(m)$coefficients[2,4] < 0.001, "< 0.001", format(summary(m)$coefficients[2,4], digits = 2))))
  as.character(as.expression(eq));
}
#
#
#
#=======  ♦   Main data     ♦ =======
#------- • Environmental -------
# Air temperature based on wetland measurements. Difference is minimal
AirT_wetland.1 <- AirT_wetland %>%
  select(Date, Time, AirT_C) %>%
  rename("Tid" = Time)
#
# Heath for climate chamber
AirT_heath.1 <- AirT_heath %>%
  select(Date, Time, AirT_C) %>%
  rename("Tid" = Time,
         "AirT_h" = AirT_C)
#
# EM50 loggers
# Values are averaged across the five blocks for soil temperature and moisture and moisture is converted to per cent.
#
# Soil temperature, moisture and PAR from heath
EM50_Heath.1 <- EM50_Heath %>%
  rowwise() %>%
  mutate(Soil_moisture = mean(c(Soil_moisture_B, Soil_moisture_P, Soil_moisture_R, Soil_moisture_W, Soil_moisture_Y, Soil_moisture_G), na.rm = TRUE),
         Soil_temperature = mean(c(Soil_temperature_B, Soil_temperature_P, Soil_temperature_R, Soil_temperature_W, Soil_temperature_Y, Soil_temperature_G), na.rm = TRUE)) %>%
  mutate(Soil_moisture = Soil_moisture*100) %>%
  ungroup() %>%
  select(Date_time, Soil_moisture, Soil_temperature, PAR) %>%
  separate_wider_delim(Date_time, delim = " ", names = c("Date", "Time"), too_few = "debug", too_many = "debug") %>%
  mutate(Date = ymd(Date),
         Tid = hms::as_hms(Time)) %>%
  select(Date, Tid, Soil_moisture, Soil_temperature, PAR) %>%
  filter(!is.na(Soil_moisture) & !is.na(Soil_temperature))
#
#
# Soil temperature, moisture and PAR from wetland
EM50_Wetland.1 <- EM50_Wetland %>%
  rowwise() %>%
  mutate(Soil_moisture_Mwet = mean(c(Soil_moisture_Bwet, Soil_moisture_Pwet, Soil_moisture_Wwet, Soil_moisture_Ywet), na.rm = TRUE),
         Soil_temperature_Mwet = mean(c(Soil_temperature_Bwet, Soil_temperature_Pwet, Soil_temperature_Wwet, Soil_temperature_Ywet), na.rm = TRUE),
         Soil_moisture_M = mean(c(Soil_moisture_B, Soil_moisture_P, Soil_moisture_R, Soil_moisture_W, Soil_moisture_Y, Soil_moisture_G), na.rm = TRUE),
         Soil_temperature_M = mean(c(Soil_temperature_B, Soil_temperature_P, Soil_temperature_R, Soil_temperature_W, Soil_temperature_Y, Soil_temperature_G), na.rm = TRUE)) %>%
  mutate(Soil_moisture_M = Soil_moisture_M*100,
         Soil_moisture_Mwet = Soil_moisture_Mwet*100) %>%
  ungroup() %>%
  select(Date_time, Soil_moisture_Mwet, Soil_temperature_Mwet, Soil_moisture_M, Soil_temperature_M, PAR) %>%
  separate_wider_delim(Date_time, delim = " ", names = c("Date", "Time"), too_few = "debug", too_many = "debug") %>%
  mutate(Date = ymd(Date),
         Tid = hms::as_hms(Time)) %>%
  select(Date, Tid, Soil_moisture_Mwet, Soil_temperature_Mwet, Soil_moisture_M, Soil_temperature_M, PAR) %>%
  rename("PAR_M" = PAR) %>%
  filter(!is.na(Soil_moisture_M) & !is.na(Soil_temperature_M))
#
#
#    ╔═══════════════════════════════╗
# -- • Climate chamber environmental • --
#    ╚═══════════════════════════════╝
#
# PAR
# Split Date_time into date and time
PAR_CC <- PAR_CC %>%
  separate_wider_delim(Date_time, delim = " ", names = c("Date", "Tid")) %>%
  mutate(Date = ymd(Date),
         Tid = hms::as_hms(Tid)) %>%
  rename("PAR.cc" = PAR)
#
# Air temperature
# Use vial measurements
# during one period 2 vials were used, as they were blind test. Average temperature of both
AirT_CC <- AirT_CC %>%
  filter(location == "vial") %>%
  select(Date, Time, AirT_C) %>%
  group_by(Date, Time) %>%
  summarise(AirT_C.cc = mean(AirT_C, na.rm = T)) %>%
  ungroup() %>%
  rename("Tid" = Time)
#
# Vial moisture
vial_moisture <- vial_moisture %>%
  filter(Time_nr != "c_NatAb") %>% # the moisture of natural abundance samples are not relevant for ARA, only for N2 fixation
  select(!c(Sample_ID, WW, DW, Count, H2O)) # Remove unused variables
#
# Vial field
vial_moisture.field <- vial_moisture %>%
  mutate(Round = str_to_upper(Time_nr)) %>% # Add round
  relocate(Round, .after = Species) %>%
  select(!Time_nr)
#
# Vial climate chamber
# Note that this is identical, but for the climate chamber
vial_moisture.CC <- vial_moisture %>%
  mutate(Round = str_c(str_to_upper(Time_nr), "5")) %>% # Add round, but note that for climate chamber rounds add a "5"
  relocate(Round, .after = Species) %>%
  select(!Time_nr)
#
#
#
#------- • Field data -------
#
# Remove chamber test and combine with temperature
field_ARA.2 <- field_ARA %>%
  filter(Species != "B") %>%
  # Round time stamp to nearest hour!
  mutate(Date_time = ymd(Date) + hms(Timestamp)) %>%
  mutate(Tid = round_date(ymd_hms(Date_time), unit = "hour")) %>%
  mutate(Tid = hms::as_hms(Tid)) %>%
  # Set the times
  group_by(Round, Date) %>%
  mutate(Start = floor_date(min(Date_time), unit = "hour"),
         End = ceiling_date(max(Date_time), unit = "hour")) %>%
  ungroup() %>%
  mutate(across(c(Start, End), hms::as_hms)) %>%
  relocate(c(Start, End), .after = Timestamp) %>%
  # Remove temporary variables
  select(!c("Date_time", "Tid"))
#
# Select the period, to match interval for environmental data
field_ARA.period <- field_ARA.2 %>%
  select(Round, Date, Start, End) %>%
  distinct(Round, Date, Start, End, .keep_all = TRUE)
#
# Combine loggers from site
# Environmental data averaged over the time of measurement for each day when measurements were done
field_environ <- reduce(list(EM50_Heath.1, EM50_Wetland.1, AirT_wetland.1), full_join, by = join_by(Date, Tid)) %>%
  # Introduce DST, as it was used in the field measurements
  # The fine-details of exactly hour when it shifts does not matter, as no measurements were done from 2-3 am when the time shifts
  mutate(Tid = case_when(Date < ymd("20201025") ~ Tid+hms::hms(3600),
                          Date == ymd("20201025") & Tid < hms::hms(3*3600) ~ Tid+hms::hms(3600),
                          Date > ymd("20210328") & Date < ymd("20211031") ~ Tid+hms::hms(3600),
                          Date > ymd("20220327") ~ Tid+hms::hms(3600),
                          TRUE ~ Tid)) %>%
  left_join(field_ARA.period, by = join_by(Date)) %>%
  filter(!is.na(Round)) %>%
  group_by(Date) %>%
  filter(Tid >= Start & Tid <= End) %>%
  summarise(SoilT_Mwet = mean(Soil_temperature_Mwet, na.rm = T),
            SoilM_Mwet = mean(Soil_moisture_Mwet, na.rm = T),
            SoilT_M = mean(Soil_temperature_M, na.rm = T),
            SoilM_M = mean(Soil_moisture_M, na.rm = T),
            PAR_M = mean(PAR_M, na.rm = T),
            SoilT = mean(Soil_temperature, na.rm = T),
            SoilM = mean(Soil_moisture, na.rm = T),
            AirT_C = mean(AirT_C, na.rm = T),
            PAR = mean(PAR, na.rm = T)) %>%
  ungroup()
#
# Reduce to an environmental value for each Species per block per round
# Combine with the big dataset first to connect date with block, species and round
field_environ.2 <- field_ARA.2 %>%
  left_join(field_environ, by = join_by(Date)) %>%
  # Reduce Soil moisture and temperature to one value, by per species
  mutate(PAR = case_when(Species == "S" | Species == "Sf" | Species == "Sli" ~ PAR_M,
                         TRUE ~ PAR),
         Soil_temperature = case_when(Species == "Sli" ~ SoilT_Mwet,
                           Species == "Sf" | Species == "S" ~ SoilT_M,
                           TRUE ~ SoilT),
         Soil_moisture = case_when(Species == "Sli" ~ SoilM_Mwet,
                           Species == "Sf" | Species == "S" ~ SoilM_M,
                           TRUE ~ SoilM)) %>%
  select(Block, Species, Round, Soil_moisture, Soil_temperature, PAR, AirT_C) %>%
  distinct(Block, Species, Round, Soil_moisture, Soil_temperature, PAR, AirT_C, .keep_all = T)
#
#
# Pivot wider and combine:
# Timestamp
field_ARA.Time <- field_ARA.2 %>% 
  select(c(Block, Species, Time, Round, Timestamp)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = Timestamp)
#
# Ethylene
field_ARA.Ethyl <- field_ARA.2 %>% 
  select(c(Block, Species, Time, Round, Ethyl_conc_ppm)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = Ethyl_conc_ppm) %>%
  rename("Ethyl_conc_ppm_T0" = T0,
         "Ethyl_conc_ppm_T0x" = T0x,
         "Ethyl_conc_ppm_T1" = T1,
         "Ethyl_conc_ppm_T1x" = T1x,
         "Ethyl_conc_ppm_T2" = T2,
         "Ethyl_conc_ppm_T3" = T3,
         "Ethyl_conc_ppm_T4" = T4)
#
# Acetylene
field_ARA.Acet <- field_ARA.2 %>% 
  select(c(Block, Species, Time, Round, Acet_conc_prC)) %>%
  complete(Block, Species, Time, Round) %>%
  mutate(Acet_conc_ppm = Acet_conc_prC*10000) %>%
  #mutate(Acet_conc_ppm = if_else(Acet_conc_ppm <= 0, 0, Acet_conc_ppm)) %>%
  select(!Acet_conc_prC) %>%
  pivot_wider(names_from = Time, values_from = Acet_conc_ppm) %>%
  rename("Acet_conc_ppm_T0" = T0,
         "Acet_conc_ppm_T0x" = T0x,
         "Acet_conc_ppm_T1" = T1,
         "Acet_conc_ppm_T1x" = T1x,
         "Acet_conc_ppm_T2" = T2,
         "Acet_conc_ppm_T3" = T3,
         "Acet_conc_ppm_T4" = T4)
#
# Join wide files and environmental data
field_ARA_wide <-  left_join(field_ARA.Time, field_environ.2, by = join_by(Block, Species, Round)) %>%
  left_join(field_ARA.Ethyl, by = join_by(Block, Species, Round)) %>%
  left_join(field_ARA.Acet, by = join_by(Block, Species, Round))
#
#
# Do the math
#
# Time and acetylene loss
field_ARA_wide.2 <- field_ARA_wide %>%
  # Calculate the time difference from T_n-1 to T_n
  mutate(Time1 = hour(seconds_to_period(T1 - T0))*60 + minute(seconds_to_period(T1 - T0)),
         Time2 = hour(seconds_to_period(T2 - T1))*60 + minute(seconds_to_period(T2 - T1)),
         Time3 = hour(seconds_to_period(T3 - T2))*60 + minute(seconds_to_period(T3 - T2)),
         Time4 = hour(seconds_to_period(T4 - T3))*60 + minute(seconds_to_period(T4 - T3))) %>%
  # Calculate the Loss of acetylene over time
  mutate(Acet_loss1 = (Acet_conc_ppm_T0 - Acet_conc_ppm_T1)/Acet_conc_ppm_T0*100,
         Acet_loss2 = (Acet_conc_ppm_T0 - Acet_conc_ppm_T2)/Acet_conc_ppm_T0*100,
         Acet_loss3 = (Acet_conc_ppm_T0 - Acet_conc_ppm_T3)/Acet_conc_ppm_T0*100,
         Acet_loss4 = (Acet_conc_ppm_T0 - Acet_conc_ppm_T4)/Acet_conc_ppm_T0*100)
#
# Some of the Acetylene values are well below 0, and should probably be corrected.
# Three samples have negative starting values (T0), but then subsequently high positive values at T1
#
# To calculate the ethylene production
# Et_corr = Et_ppm - Ac_ppm × [Et]tn-1 / [Ac]tn-1
#
# Where
# Et_corr  : Corrected ethylene production in parts per million (ppm)
# Et_ppm   : Ethylene production in parts per million per hour (ppm h^-1)
# Ac_ppm   : Acetylene loss per hour (ppm h^-1)
# [Et]tn-1 : Ethylene concentration at time tn-1, "start" concentration
# [Ac]tn-1 : Acetylene concentration at time tn-1, "start" concentration
#
field_ARA_wide.3 <- field_ARA_wide.2 %>%
  mutate(Et_prod_ppm_pr_h.1 = (Ethyl_conc_ppm_T1 - Ethyl_conc_ppm_T0)/(Time1/60),
         Act_lost_ppm_pr_h.1 = (Acet_conc_ppm_T1 - Acet_conc_ppm_T0)/(Time1/60)) %>%
  mutate(Corr_Et_prod_pr_h.1 = Et_prod_ppm_pr_h.1 - (Act_lost_ppm_pr_h.1*(Ethyl_conc_ppm_T0/Acet_conc_ppm_T0)))
#
# Ethylene production per hour per square meter (µmol h^-1 m^-2)
# Et_ppm × (V × P) / (R × T) / A
#
# Where
# Et_ppm  : Ethylene production in parts per million per hour (ppm h^-1)
# V       : Volume in litres (L)
# P       : Pressure in kilo Pascal (kPa)
# R       : the gas constant ~ 8.31446261815324 L kPa K^-1 µmol^-1
# T       : Temperature in Kelvin (K)
# A       : Area in square meters (m^2)
#
field_ARA_wide.4 <- field_ARA_wide.3 %>%
  mutate(Et_prod_umol_h_m2 = Corr_Et_prod_pr_h.1 * (Ch_vol_L * p) / (R_const * (AirT_C+273)) / Ch_area_m2) # Temperature set from T0 at Wetland !!!
#
# Set negative production to 0
field_ARA_wide.5 <- field_ARA_wide.4 %>%
  mutate(Et_prod_umol_h_m2 = if_else(Et_prod_umol_h_m2 < 0, 0, Et_prod_umol_h_m2)) %>%
  # Add Round names, habitat and bryophyte functional groups (BFG)
  mutate(Month = case_when(Round == 1 ~ "Sept20",
                         Round == 2 ~ "Oct20",
                         Round == 3 ~ "Nov20",
                         Round == 4 ~ "Feb21",
                         Round == 5 ~ "Mar21",
                         Round == 6 ~ "May21",
                         Round == 7 ~ "Jun21",
                         Round == 8 ~ "Jul21",
                         Round == 9 ~ "Sept21",
                         Round == 10 ~ "Oct21",
                         Round == 11 ~ "Nov21"),
         Habitat = if_else(Species == "S" | Species == "Sli" | Species == "Sf", "M", "H"),
         BFG = case_when(Species == "Au" ~ "Short unbranched turf",
                         Species == "Di" ~ "Tall unbranched turf",
                         Species == "Hy" | Species == "Pl" ~ "Weft",
                         Species == "Po" ~ "Polytrichales",
                         Species == "Pti" ~ "Leafy liverwort",
                         Species == "Ra" ~ "Large cushion",
                         Species == "S" | Species == "Sli" | Species == "Sf" ~ "Sphagnum")) %>%
  relocate(c(Month, Habitat, BFG), .after = Round)
#
# Export main results
# field_ARA_wide.export <- field_ARA_wide.5 %>%
#   select(Block, Species, Round, AirT_C, Soil_temperature, Soil_moisture, PAR, Et_prod_umol_h_m2) %>%
#   rename("AirT" = AirT_C)
#
# write_csv(field_ARA_wide.export, "export/Q1_ARA2.csv", na = "NA")
#
#
#
#------- • Vial data -------
#
# Load and select important parts of the vial data
vial_ARA.1 <- vial_ARA %>%
  mutate(Species = str_replace_all(Species, "M", "B")) %>% #Replace M for Myren/mire with B for Blank
  filter(!str_starts(Species, "v")) %>% #Remove vial tests
  select(!Temp_approx_C)
#
# Blanks
# Averaged for each round and habitat
vial_ARA.b <- vial_ARA.1 %>%
  filter(str_starts(Species, "B")) %>%
  summarise(across(c(Ethyl_conc_ppm, Acet_conc_prC), ~mean(.x)), .by = c(Block, Round)) %>%
  rename("Ethyl_blank" = Ethyl_conc_ppm,
         "Acet_blank" = Acet_conc_prC,
         "Habitat" = Block)
#
# Join blanks to non-blanks and correct
vial_ARA.2 <- vial_ARA.1 %>%
  filter(!str_starts(Species, "B")) %>% # remove blanks
  mutate(Habitat = if_else(str_starts(Species, "S"), "M", "H")) %>% # add habitat, Sphagnums are in the mire
  left_join(vial_ARA.b, by = join_by(Round, Habitat), multiple = "all") %>% # Add blanks
  mutate(Time24 = hour(seconds_to_period(hms(Time_from_T0) - hms(Timestamp)))*60 + minute(seconds_to_period(hms(Time_from_T0) - hms(Timestamp))) + 24*60) %>%
  mutate(Et_prod_ppm_pr_h = (Ethyl_conc_ppm - Ethyl_blank)/(Time24),
         Act_lost_ppm_pr_h = (Acet_conc_prC*10000 - Acet_blank*10000)/(Time24)) %>%
  mutate(Corr_Et_prod_pr_h = Et_prod_ppm_pr_h - (Act_lost_ppm_pr_h*(Ethyl_blank/Acet_blank)))
#
# Select the period, to match interval for environmental data
vial_ARA.period <- vial_ARA.2 %>%
  mutate(Date_time = ymd(Date) + hms(Timestamp)) %>%
  mutate(Tid = round_date(ymd_hms(Date_time), unit = "hour")) %>%
  mutate(Tid = hms::as_hms(Tid)) %>%
  # Set the times
  group_by(Round, Date) %>%
  mutate(Start = floor_date(min(Date_time), unit = "hour"),
         End = ceiling_date(max(Date_time), unit = "hour")) %>%
  ungroup() %>%
  mutate(across(c(Start, End), hms::as_hms)) %>%
  relocate(c(Start, End), .after = Timestamp) %>%
  # Remove temporary variables
  select(Round, Date, Start, End) %>%
  distinct(Round, Date, Start, End, .keep_all = TRUE) %>%
  # A very crude way of adding the 24h period start and end
  mutate(DateStart = ymd(Date)+hms(Start),
         DateEnd = ymd(Date+1)+hms(End)) %>%
  pivot_longer(cols = c(DateStart, DateEnd), names_to = "Datetime", values_to = "Dates") %>%
  mutate(Date = if_else(Datetime == "DateEnd", Date+1, Date)) %>%
  pivot_wider(names_from = Datetime, values_from = Dates) %>%
  mutate(DateEnd = if_else(is.na(DateEnd), ymd(Date+1)+hms(End), DateEnd),
         DateStart = if_else(is.na(DateStart), ymd(Date-1)+hms(Start), DateStart),
         # The rounds are not unique, because measurements were done over 2 x 24h periods except for round C
         Roundsub = case_when(Round == "A" & Date == ymd("2021-02-11") | Round == "A" & Date == ymd("2021-02-12") ~ "A_1",
                              Round == "A" & Date == ymd("2021-02-15") | Round == "A" & Date == ymd("2021-02-16") ~ "A_2",
                              Round == "B" & Date == ymd("2021-03-30") | Round == "B" & Date == ymd("2021-03-31") & Start == hms::as_hms("16:00:00") ~ "B_1",
                              Round == "B" & Date == ymd("2021-03-31") & Start == hms::as_hms("11:00:00") | Round == "B" & Date == ymd("2021-04-01") ~ "B_2",
                              # ↑ 
                              # Field
                              # Climate chamber
                              # ↓ 
                              Round == "A5" & Date == ymd("2021-02-12") | Round == "A5" & Date == ymd("2021-02-13") ~ "A5_1",
                              Round == "A5" & Date == ymd("2021-02-16") | Round == "A5" & Date == ymd("2021-02-17") ~ "A5_2",
                              Round == "B5" & Date == ymd("2021-03-31") | Round == "B5" & Date == ymd("2021-04-01") & Start == hms::as_hms("20:00:00") ~ "B5_1",
                              Round == "B5" & Date == ymd("2021-04-01") & Start == hms::as_hms("14:00:00") | Round == "B5" & Date == ymd("2021-04-02") ~ "B5_2",
                              TRUE ~ Round))
#
# Average environmental data for the time period
environ_vial <- reduce(list(EM50_Heath.1, EM50_Wetland.1, AirT_wetland.1, AirT_heath.1), full_join, by = join_by(Date, Tid)) %>%
  # Introduce DST, as it was used in the field measurements
  # The fine-details of exactly hour when it shifts does not matter, as no measurements were done from 2-3 am when the time shifts
  mutate(Tid = case_when(Date < ymd("20201025") ~ Tid+hms::hms(3600),
                         Date == ymd("20201025") & Tid < hms::hms(3*3600) ~ Tid+hms::hms(3600),
                         Date > ymd("20210328") & Date < ymd("20211031") ~ Tid+hms::hms(3600),
                         Date > ymd("20220327") ~ Tid+hms::hms(3600),
                         TRUE ~ Tid)) %>%
  mutate(AirT_h = if_else(is.na(AirT_h), AirT_C, AirT_h)) %>% # As the last couple of heathland tinytags were not logged, use air temperature from wetland
  left_join(PAR_CC, by = join_by(Date, Tid)) %>%
  left_join(AirT_CC, by = join_by(Date, Tid)) %>%
  left_join(vial_ARA.period, by = join_by(Date), multiple = "all") %>%
  filter(!is.na(Round)) %>%
  mutate(Date_time = ymd(Date) + hms(Tid)) %>%
  group_by(Roundsub) %>%
  filter(Date_time >= DateStart & Date_time <= DateEnd) %>%
  summarise(PAR.field = mean(PAR, na.rm = T),
            PAR_M = mean(PAR_M, na.rm = T),
            AirT_h = mean(AirT_h, na.rm = T),
            AirT_C.field = mean(AirT_C, na.rm = T),
            PAR.cc = mean(PAR.cc, na.rm = T),
            AirT_C.cc = mean(AirT_C.cc, na.rm = T)) %>%
  ungroup() %>%
  mutate(PAR = if_else(str_detect(Roundsub, "5"), PAR.cc, PAR.field),
         AirT_C = if_else(str_detect(Roundsub, "5"), AirT_C.cc, AirT_C.field)) %>%
  select(!c(PAR.field, AirT_C.field, PAR.cc, AirT_C.cc))
#
# Combine environmental data with vial data and calculate ethylene production
vial_ARA.3 <- vial_ARA.2 %>%
  mutate(Roundsub = case_when(Date == ymd("2021-02-11") ~ "A_1",
                              Date == ymd("2021-02-15") ~ "A_2",
                              Date == ymd("2021-03-30") ~ "B_1",
                              !str_detect(Round, "5") & Date == ymd("2021-03-31") ~ "B_2",
                              # ↑ 
                              # Field
                              # Climate chamber
                              # ↓ 
                              str_detect(Round, "5") & Date == ymd("2021-02-12") ~ "A5_1",
                              str_detect(Round, "5") & Date == ymd("2021-02-16") ~ "A5_2",
                              str_detect(Round, "5") & Date == ymd("2021-03-31") ~ "B5_1",
                              str_detect(Round, "5") & Date == ymd("2021-04-01") ~ "B5_2",
                              TRUE ~ Round)) %>%
  relocate(Roundsub, .after = Round) %>%
  left_join(environ_vial, by = join_by(Roundsub)) %>%
  mutate(PAR = if_else(Species == "S" | Species == "Sf" | Species == "Sli", PAR_M, PAR),
         AirT_C = if_else(Species == "S" | Species == "Sf" | Species == "Sli", AirT_C, AirT_h)) %>%
  select(!c(PAR_M, AirT_h)) %>%
  mutate(Et_prod_umol_h_m2 = Corr_Et_prod_pr_h * (Vial_vol_L * p) / (R_const * (AirT_C+273)) / Vial_area_m2) %>%
  # Ethylene production is either greater than 0 or 0. No negative values
  mutate(Et_prod_umol_h_m2 = if_else(Et_prod_umol_h_m2 < 0, 0, Et_prod_umol_h_m2))
#
#    ╔═════════════╗
# -- • Field vials • --
#    ╚═════════════╝
#
vial_ARA_field <- vial_ARA.3 %>%
  filter(Round == "A" | Round == "B" | Round == "C") %>%
  left_join(vial_moisture.field, by = join_by(Block, Species, Round)) %>%
  relocate(GWC, .before = Et_prod_umol_h_m2)
#
#    ╔═══════════════════════╗
# -- • Climate chamber vials • --
#    ╚═══════════════════════╝
#
vial_ARA_climateChamber <- vial_ARA.3 %>%
  filter(Round == "A5" | Round == "B5" | Round == "C5") %>%
  left_join(vial_moisture.CC, by = join_by(Block, Species, Round)) %>%
  relocate(GWC, .before = Et_prod_umol_h_m2)
#
#
#
#------- • 15N data -------
#
#
# 15N data ID
ID_15N <- ID_15N %>%
  mutate(across(Date, ~ymd(.x))) %>%
  select(!c(Sample_ID, Time_nr))
#
# Combine 15N data with the ethylene production from the same period.
# Useless, since climate chamber data is 0 for everything but the Sphagnum species
vial_15N.2 <- vial_ARA.3 %>% 
  filter(Round == "C5") %>%
  select(Block, Species, Habitat, Et_prod_umol_h_m2) %>%
  left_join(ID_15N, by = join_by(Block, Species)) %>%
  relocate(Et_prod_umol_h_m2, .after = Time_T1) %>%
  left_join(vial_15N, by = join_by(Block, Species)) %>%
  # µg 15N g-1 DW * g DW = µg 15N
  mutate(Excess_15N = N15_pr_DW*Vial_sample_DW) %>%
  # µg 15N / µg 15N pr µg N injected = µg N fixed pr sample!
  mutate(Fixed_N = Excess_15N/0.98) %>% # 98% 15N2 used
  # µg N pr h pr m2 (assuming 24h incubation)
  mutate(N_h_m2 = Fixed_N/Vial_sample_area_m2/24,
         N15_h_m2 = Excess_15N/Vial_sample_area_m2/24) %>%
  mutate(ARA_ratio = Et_prod_umol_h_m2/Fixed_N,
         ARA_ratio = Et_prod_umol_h_m2/N_h_m2)

vial_15N.avg <- vial_15N.2 %>%
  group_by(Species) %>%
  summarise(Et_prod_umol_h_m2 = mean(Et_prod_umol_h_m2, na.rm = TRUE),
            Fixed_N = mean(Fixed_N, na.rm = TRUE),
            N_h_m2 = mean(N_h_m2, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(ARA_ratio = Et_prod_umol_h_m2/Fixed_N)
#
#
#
#------- • Density -------
#
# Average if there are two counts. Express density per m2
densityCount <- densityCount %>%
  rowwise() %>%
  mutate(density = mean(c(Count, Count2), na.rm = TRUE)) %>%
  mutate(density = density/(densityArea*Area)) %>%
  ungroup()
#
# Average
densityCount.avg <- densityCount %>%
  summarise(density = mean(density, na.rm = TRUE), .by = Species)
#
#
#
#=======  §§  Statistics    §§ =======
#-------  »   Environmental NMDS « -------
#
# 




# OBS! Jérémy "Birgitte" Monsimet says:
# CHECK ASSUMPTIONS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# of the ordination
# If NMDS it uses glmm so check the assumptions
# PCA also has assumption

# Report stress value!





# Extract environmental data
# Done again (Notice capital F), as to maintain the order of variables with the order from ethylene calculations
field_environ.3 <- field_ARA_wide.5 %>%
  select(Block, Species, Round, AirT_C, Soil_temperature, Soil_moisture, PAR, Et_prod_umol_h_m2) %>%
  mutate(AirT = AirT_C + 273,
         Soil_temperature = Soil_temperature + 273) %>% # remove negative temperature by converting to kelvin
  select(-AirT_C) %>%
  mutate(Round = as.factor(Round)) %>%
  pivot_wider(values_from = Et_prod_umol_h_m2, names_from = Species)

# Find from station!!!
# Or from WinterEcology I -> last measurements were in beginning of October



#  mutate(PAR = if_else(Round == "1", NA, PAR))
#
# Export
# write_csv(field_environ.3, "export/N2fix_environ2.csv")
#
#
# Correlation plot of environmental data
corrplot::corrplot(cor(field_environ.3[6:3], method = "kendall"), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
corrplot::corrplot(cor(field_environ.3[16:3], method = "kendall"), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
pairs(x = field_environ.3[6:3], gap = 0, cex.labels = 0.5)
pairs(x = field_environ.3[16:3], gap = 0, cex.labels = 0.5)
# All somewhat correlated environmental factors
#
# NMDS
NMDS_environ <- metaMDS(field_environ.3[3:6], distance = "bray")#, scaling = 1, autotransform = TRUE) # Does a sqrt transformation and Wisconsin standardization
#
# Species ethylene production fit
envfit.et <- envfit(NMDS_environ,
                    field_environ.3[7:16],
                    permutations = 9999, na.rm = TRUE)
#
# Plot
# Standard with little fancyness
par (mfrow = c(1,2))
plot(NMDS_environ, type = "n")
points(NMDS_environ, display = "sites", cex = 0.8, pch=21, col="black", bg="white")
text(NMDS_environ, display = "spec", cex=0.7, col="blue")
plot(envfit.et)
stressplot(NMDS_environ)
par (mfrow = c(1,1))
#
# Extract points to create graph
# Main plot with NMDS scores
field_environ.plot <- field_environ.3
field_environ.plot$NMDS1 <- NMDS_environ$points[,1]
field_environ.plot$NMDS2 <- NMDS_environ$points[,2]
field_environ.plot <- field_environ.plot %>%
  mutate(season = case_when(Round == "1" | Round == "2" | Round == "3" ~ "Fall1",
                         Round == "10" | Round == "11" ~ "Fall2",
                         Round == "4" ~ "Winter",
                         Round == "5" | Round == "6" ~ "Spring",
                         Round == "7" | Round == "8" | Round == "9" ~ "Summer"),
         snowS = if_else(str_detect(Round, "3|4|5|6"), "Snow", "Free"),
         # snowS = case_when(str_detect(Round, "3|4|5") ~ "Snow",
         #                   Round == "6" & Block == "W" ~ "Snow",
         #                   Round == "6" & Block == "P" ~ "Snow", 
         #                   Round == "6" & Block == "R" ~ "Snow",
         #                   TRUE ~ "Free"),
         month = case_when(Round == "1" ~ "Sep20",
                           Round == "2" ~ "Oct20",
                           Round == "3" ~ "Nov20",
                           Round == "4" ~ "Feb",
                           Round == "5" ~ "Mar",
                           Round == "6" ~ "May",
                           Round == "7" ~ "Jun",
                           Round == "8" ~ "Jul",
                           Round == "9" ~ "Aug",
                           Round == "10" ~ "Sep",
                           Round == "11" ~ "Nov"))
#
# For text
# Environmental factors
field_environ.scores.env <- as.data.frame(scores(NMDS_environ, "species"))
field_environ.scores.env$Factors <- rownames(field_environ.scores.env)
#
field_environ.scores.env <- field_environ.scores.env %>%
  mutate(Factors = case_when(Factors == "Soil_temperature" ~ "Soil temperature",
                             Factors == "Soil_moisture" ~ "Soil moisture",
                             Factors == "AirT" ~ "Air temperature",
                             TRUE ~ Factors))
# Species directions
field_environ.scores.sp <- as.data.frame(scores(envfit.et$vectors$arrows))
field_environ.scores.sp$Species <- rownames(field_environ.scores.sp)
# Decrease arrow size, so direction can be plotted with the main NMDS plot
field_environ.scores.sp <- field_environ.scores.sp %>%
  mutate(NMDS1 = NMDS1/3,
         NMDS2 = NMDS2/4)
#
#
# Plot the NMDS
ggplot() +
#  geom_polygon(data = field_environ.plot, aes(x = NMDS1, y = NMDS2, fill = snowS, group = snowS), alpha = 0.30) +
#  geom_polygon(data = field_environ.plot, aes(x = NMDS1, y = NMDS2, fill = season, group = season), alpha = 0.30) +
  geom_text(data = field_environ.scores.env, aes(x = NMDS1, y = NMDS2, label = Factors)) +
  geom_point(data = field_environ.plot, aes(x = NMDS1, y = NMDS2, shape = Block, color = month), size = 3) +
  geom_segment(data = field_environ.scores.sp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), color = "#8fa3a5") +
  geom_text_repel(data = field_environ.scores.sp, aes(x = NMDS1, y = NMDS2, label  = Species), size = 5, color = "#4D495A") + 
  coord_equal() +
  theme_classic() +
  theme(legend.text = element_text(size = 21), legend.title = element_text(size = 21))

#
#
#-------  »   Q1            « -------
# 1.	How active can mosses potentially be in their N2-fixation depending on moisture, temperature, and light availability?
#
Q1_ARA <- field_ARA_wide.5 %>%
  mutate(across(Round, ~as.character(.x))) %>%
  mutate(across(c(Block, Species, Round), ~as.factor(.x)))
#
# Transform data
Q1_ARA <- Q1_ARA %>%
  select(1:6, AirT_C, Soil_temperature, Soil_moisture, PAR, Et_prod_umol_h_m2) %>%
  mutate(logEt_prod = log(Et_prod_umol_h_m2+5),
         sqrtEt_prod = sqrt(Et_prod_umol_h_m2),
         cubeEt_prod = Et_prod_umol_h_m2^(1/9),
         sqEt_prod = Et_prod_umol_h_m2^2,
         ashinEt_prod = log(Et_prod_umol_h_m2 + sqrt(Et_prod_umol_h_m2^2 + 1)), # inverse hyperbolic sine transformation
         arcEt_prod = asin(sqrt(((Et_prod_umol_h_m2)/10000))))
#
# Graph without 0 values to see distribution with transformation
Q1_ARA %>%
  #filter(Et_prod_umol_h_m2 != 0) %>% # & Et_prod_umol_h_m2 < 10 # Remove zeros (and extreme values)
  #  ggplot(aes(x = Round, y = (Et_prod_umol_h_m2))) + geom_point()
  ggplot(aes(x = sqrt(Et_prod_umol_h_m2))) + geom_histogram(bins = 1000)
# Heavily right-skewed, for pretty much all transformations
# A right-skewed Poisson distribution?
#
# Linear mixed effects model with both species and round
lme1 <- lme(logEt_prod ~ Round*Species,
            random = ~1|Block/Species,
            data = Q1_ARA, na.action = na.exclude, method = "REML")
#
# Using lme4 package:
# lmer(logEt_prod ~ Round*Species + (1 | Block/Species), data = Q1_ARA, na.action = na.exclude)
#
# Checking assumptions:
par(mfrow = c(1,2))
plot(fitted(lme1), resid(lme1), 
     xlab = "fitted", ylab = "residuals", main="Fitted vs. Residuals") 
qqnorm(resid(lme1), main = "Normally distributed?")                 
qqline(resid(lme1), main = "Homogeneity of Variances?", col = 2) #OK
plot(lme1)
par(mfrow = c(1,1))
# Values are not very good because of several zero values.
#
# model output
Anova(lme1, type=2)
#
# Plot ethylene production across the seasons
Q1_ARA %>%
  mutate(across(Month, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  ggplot(aes(y = Et_prod_umol_h_m2, x = Month)) + geom_boxplot() + facet_wrap(~Species, scales = "free")
# There are mostly zero values
# These zeros could either be true zeros and suggest no activity at all, or activity below detection.
#
#
# Given the possibility of zero inflation a generalized linear mixed effects model using the glmmTMB package was used
# Production is square-root transformed
#
#       ╔═══════════╗
# -- »»» Seasonality ««« --
#       ╚═══════════╝
#
model <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ Round*Species, data=Q1_ARA, ziformula=~1, family=gaussian)
Anova(model, type = c("II"), test.statistic = c("Chi"), component = "cond")
emmeans(model, ~ Species*Round)
#
#       ╔═════════════════════╗
# -- »»» Environmental drivers ««« --
#       ╚═════════════════════╝
#
model2 <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ Species*(Soil_temperature+Soil_moisture+PAR+AirT_C), data=Q1_ARA, ziformula=~1, family=gaussian)
Anova(model2, type = c("II"), test.statistic = c("Chi"), component = "cond")
emmeans(model2,"Species")
summary(model2)
#
plot(residuals(model2)) # best for zero inflated
qqnorm(residuals(model2))
qqline(residuals(model2), col = 2)
plotmo::plotres(model2) # Bad for zero inflated models
#
#       ╔═══╗
# -- »»» BFG ««« --
#       ╚═══╝
#
model3 <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ (AirT_C+Soil_temperature+Soil_moisture+PAR)*BFG, data=Q1_ARA, ziformula=~1, family=gaussian)
Anova(model3, type = c("II"), test.statistic = c("Chi"), component = "cond")
emmeans(model3,"BFG")
#
#       ╔═══════╗
# -- »»» Habitat ««« --
#       ╚═══════╝
#
# Heath
modelHeath <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ (AirT_C+Soil_temperature+Soil_moisture+PAR)*Species ,data=Q1_ARA[Q1_ARA$Habitat=="H",], ziformula=~1, family=gaussian)
Anova(modelHeath, type = c("II"), test.statistic = c("Chi"), component = "cond")
emmeans(modelHeath,"Species")
#
plot(residuals(modelHeath))
#
# Mire
modelMire <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ (AirT_C+Soil_temperature+Soil_moisture+PAR)*Species ,data=Q1_ARA[Q1_ARA$Habitat=="M",], ziformula=~1, family=gaussian)
Anova(modelMire, type = c("II"), test.statistic = c("Chi"), component = "cond")
emmeans(modelMire,"Species")
#
plot(residuals(modelMire))
#
#       ╔════════════════════════════╗
# -- »»» Model on a per species level ««« --
# -- »»» Simply modeled against round ««« --
#       ╚════════════════════════════╝
#
# Au
modelAu <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round) ,data=Q1_ARA[Q1_ARA$Species=="Au",], ziformula=~1, family=gaussian)
Anova(modelAu, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 110.15  10  < 2.2e-16 
emmeans(modelAu,"Round")
#
#
# Di
modelDi <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Di",], ziformula=~1, family=gaussian)
Anova(modelDi, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 200.1   10  < 2.2e-16
emmeans(modelDi,"Round")
#
#
# Hy
modelHy <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round) ,data=Q1_ARA[Q1_ARA$Species=="Hy",], ziformula=~1, family=gaussian)
Anova(modelHy, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 206.92  10  < 2.2e-16
emmeans(modelHy,"Round")
#
#
# Pl
modelPl <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Pl",], ziformula=~1, family=gaussian)
Anova(modelPl, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 184.29  10  < 2.2e-16
emmeans(modelPl,"Round")
#
#
# Po
modelPo <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Po",], ziformula=~1, family=gaussian)
Anova(modelPo, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 224.88  10  < 2.2e-16
emmeans(modelPo,"Round")
#
#
# Pti
modelPti <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Pti",], ziformula=~1, family=gaussian)
Anova(modelPti, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 58.75   10    6.242e-09
emmeans(modelPti,"Round")
#
#
# Ra
modelRa <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Ra",], ziformula=~1, family=gaussian)
Anova(modelRa, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 40.133  10    1.605e-05
emmeans(modelRa,"Round")
#
#
# S
modelS <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="S",], ziformula=~1, family=gaussian)
Anova(modelS, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 32.743  10    0.0003008
emmeans(modelS,"Round")
#
#
# Sf
modelSf <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Sf",], ziformula=~1, family=gaussian)
Anova(modelSf, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 89188   10    7.76e-15
emmeans(modelSf,"Round")
#
#
# Sli
modelSli <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Sli",], ziformula=~1, family=gaussian)
Anova(modelSli, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ       DF    p
# 154.95  10  < 2.2e-16
emmeans(modelSli,"Round")
#
# mean value per measuring period per species
ARAmeans <- summarySE(data = Q1_ARA, measurevar = "Et_prod_umol_h_m2", groupvars = c("Species", "Round"))
#
#
#
#-------  »   Vial          « -------
#
# As with the field data, but done for the three rounds
#
#
#       ╔═══════════╗
# -- »»» Field vials ««« --
#       ╚═══════════╝
#
Qvial_ARA.field <- vial_ARA_field %>%
  mutate(across(Round, ~as.character(.x))) %>%
  mutate(across(c(Block, Species, Round), ~as.factor(.x)))
#
# Transform data
Qvial_ARA.field <- Qvial_ARA.field %>%
  select(1:2, 4, Et_prod_umol_h_m2, PAR, AirT_C, GWC) %>%
  mutate(logEt_prod = log(Et_prod_umol_h_m2+5),
         sqrtEt_prod = sqrt(Et_prod_umol_h_m2),
         cubeEt_prod = Et_prod_umol_h_m2^(1/9),
         sqEt_prod = Et_prod_umol_h_m2^2,
         ashinEt_prod = log(Et_prod_umol_h_m2 + sqrt(Et_prod_umol_h_m2^2 + 1)), # inverse hyperbolic sine transformation
         arcEt_prod = asin(sqrt(((Et_prod_umol_h_m2)/10000))),
         AirT_C = AirT_C+273,
         GWC = GWC*100)
#
# Graph without 0 values to see distribution with transformation
Qvial_ARA.field %>%
  filter(Et_prod_umol_h_m2 != 0) %>%
  #  ggplot(aes(x = Round, y = (Et_prod_umol_h_m2))) + geom_point()
  ggplot(aes(x = sqrt(Et_prod_umol_h_m2))) + geom_histogram()
# removing 0's and using square-root transformation gives a slightly less right-skewed distribution (log might be better, but several low values give negative values)
#
# Model  - LME
# Linear mixed effects model with both species and round
lmeVial <- lme(sqrtEt_prod ~ Round*Species,
               random = ~1|Block/Species,
               data = Qvial_ARA.field, na.action = na.exclude, method = "REML")
#
# Checking assumptions:
par(mfrow = c(1,2))
plot(fitted(lmeVial), resid(lmeVial), 
     xlab = "fitted", ylab = "residuals", main="Fitted vs. Residuals") 
qqnorm(resid(lmeVial), main = "Normally distributed?")                 
qqline(resid(lmeVial), main = "Homogeneity of Variances?", col = 2) #OK
plot(lmeVial)
par(mfrow = c(1,1))
# Values are not very good because of several zero values. -> use glmmTMB with zero-inflation
#
# model output
Anova(lmeVial, type=2)
#
# Model - glmmTMB
# Given the possibility of zero inflation a generalized linear mixed effects model using the glmmTMB package was used
# Production is square-root transformed
model_vial <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ Round*Species, data=Qvial_ARA.field, ziformula=~1, family=gaussian)
Anova(model_vial, type = c("II"), test.statistic = c("Chi"), component = "cond")
#
plot(residuals(model_vial))
qqnorm(residuals(model_vial))
qqline(residuals(model_vial), col = 2)
#
#
# Environmental model
model_vial.env <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ (PAR+GWC)*Species, data=Qvial_ARA.field, ziformula=~1, family=gaussian)
Anova(model_vial.env, type = c("II"), test.statistic = c("Chi"), component = "cond")
#
# residuals
qqnorm(residuals(model_vial.env))
qqline(residuals(model_vial.env), col = 2)
plot(residuals(model_vial.env))
#
# mean value per measuring period per species
vial.field.means <- summarySE(data = Qvial_ARA.field, measurevar = "Et_prod_umol_h_m2", groupvars = c("Species", "Round"))
vial.GWC.means <- summarySE(data = Qvial_ARA.field, measurevar = "GWC", groupvars = c("Species", "Round"))
#
#
#       ╔═════════════════════╗
# -- »»» Climate chamber vials ««« --
#       ╚═════════════════════╝
#
Qvial_ARA.CC <- vial_ARA_climateChamber %>%
  mutate(across(Round, ~as.character(.x))) %>%
  mutate(across(c(Block, Species, Round), ~as.factor(.x)))
#
# Transform data
Qvial_ARA.CC <- Qvial_ARA.CC %>%
  select(1:2, 4, Et_prod_umol_h_m2, PAR, AirT_C, GWC) %>%
  mutate(logEt_prod = log(Et_prod_umol_h_m2+5),
         sqrtEt_prod = sqrt(Et_prod_umol_h_m2),
         cubeEt_prod = Et_prod_umol_h_m2^(1/9),
         sqEt_prod = Et_prod_umol_h_m2^2,
         ashinEt_prod = log(Et_prod_umol_h_m2 + sqrt(Et_prod_umol_h_m2^2 + 1)), # inverse hyperbolic sine transformation
         arcEt_prod = asin(sqrt(((Et_prod_umol_h_m2)/10000))),
         AirT_C = AirT_C+273)
#
# Graph without 0 values to see distribution with transformation
Qvial_ARA.CC %>%
  filter(Et_prod_umol_h_m2 != 0) %>%
  #  ggplot(aes(x = Round, y = (Et_prod_umol_h_m2))) + geom_point()
  ggplot(aes(x = sqrt(Et_prod_umol_h_m2))) + geom_histogram()
# removing 0's and using square-root transformation gives something closer to a normal distribution
#
# Model - glmmTMB
# Given the possibility of zero inflation a generalized linear mixed effects model using the glmmTMB package was used
model_vial.CC <- glmmTMB(sqrtEt_prod ~ Round*Species, data=Qvial_ARA.CC, ziformula=~1, family=gaussian)
Anova(model_vial.CC, type = c("II"), test.statistic = c("Chi"), component = "cond")
emmeans(model_vial.CC, ~ Species*Round)
#
# Environmental - GWC
model_vial.gwc <- glmmTMB(sqrtEt_prod ~ GWC*Species, data=Qvial_ARA.CC, ziformula=~1, family=gaussian)
Anova(model_vial.gwc, type = c("II"), test.statistic = c("Chi"), component = "cond")
#
# mean value per measuring period per species
vial.CC.means <- summarySE(data = Qvial_ARA.CC, measurevar = "Et_prod_umol_h_m2", groupvars = c("Species", "Round"))
#
#
#
#=======  ♫♫  Graphs        ♫♫ =======
#-------  ♪   Environmental ♪ -------
#
# Moisture from three different loggers
EM50_Heath %>%
  ggplot() +
  geom_point(aes(x = Date_time, y = Soil_moisture_B), colour = "#0072B2") +
  #geom_point(aes(x = Date_time, y = Soil_moisture_P), colour = "#CC79A7") +
  geom_point(aes(x = Date_time, y = Soil_moisture_R), colour = "#D55E00") +
  #geom_point(aes(x = Date_time, y = Soil_moisture_W), colour = "#009E73") +
  geom_point(aes(x = Date_time, y = Soil_moisture_Y), colour = "#F0E442")
#
# GWC graph
vial_ARA_field %>%
  mutate(Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species),
         GWC = GWC*100,
         Month = case_when(Round == "A" ~ "February",
                           Round == "B" ~ "March",
                           Round == "C" ~ "July")) %>%
  mutate(across(Month, ~ factor(.x, levels=c("February", "March", "July")))) %>%
  summarise(meanGWC = mean(GWC, na.rm = TRUE), se = sd(GWC)/sqrt(length(GWC)), .by = c(Month, Species)) %>%
  ggplot() +
  geom_errorbar(aes(x = Month, y = meanGWC, ymin=meanGWC, ymax=meanGWC+se), position=position_dodge(.9)) +
  geom_col(aes(x = Month, y = meanGWC), color = "black") + 
  facet_wrap(~Species, scales = "free", ncol = 5) +
  labs(x = "Measuring period (Month)", y = expression("Gravimetric water content (GWC, % "*DW^-1*")"), title = expression("Bryophyte water content")) + 
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(1, "lines"))
#
#
#
#-------  ♪   ARA           ♪ -------
#
# Combine the important features of chambers and vials
# Ethylene production only
# Field chamber
field.basic <- field_ARA_wide.5 %>%
  select(Block, Species, Round, Month, Habitat, BFG, Et_prod_umol_h_m2) %>%
  rename("Et_prod_field" = Et_prod_umol_h_m2)
#
# Vials in the field
vial_field.basic <- vial_ARA_field %>%
  select(Block, Species, Round, Et_prod_umol_h_m2) %>%
  rename("Et_prod_vial.Field" = Et_prod_umol_h_m2)
#
# Vials in the climate chamber
vial_cc.basic <- vial_ARA_climateChamber  %>%
  select(Block, Species, Round, Et_prod_umol_h_m2) %>%
  rename("Et_prod_vial.CC" = Et_prod_umol_h_m2) %>%
  mutate(Round = case_when(Round == "A5" ~ "A",
                           Round == "B5" ~ "B",
                           Round == "C5" ~ "C"))
#
# Combine vial results
vial.basic <- left_join(vial_field.basic, vial_cc.basic, by = join_by(Block, Species, Round)) %>%
  mutate(Round = case_when(Round == "A" ~ 4,
                           Round == "B" ~ 5,
                           Round == "C" ~ 8))
#
# Combine field measurements and vial measurements
ARA_all.basic <- left_join(field.basic, vial.basic, by = join_by(Block, Species, Round))
#
# Filter down to vial data points
ARA_vialRound.basic <- ARA_all.basic %>%
  filter(Round == 4 | Round == 5 | Round == 8)
#
# Export dataset
#write_csv(ARA_vialRound.basic, "export/ARA_field_and_vial.csv")
#
#
#      ╔════════════════╗
# -- ♪ ♪ Field chambers ♪ ♪ --
#      ╚════════════════╝
#
# A grayscale graph
field_ARA_wide.5 %>%
  mutate(Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  summarise(meanEt_pro = mean(Et_prod_umol_h_m2, na.rm = TRUE), se = sd(Et_prod_umol_h_m2)/sqrt(length(Et_prod_umol_h_m2)), .by = c(Month, Species)) %>%
  ggplot() +
  geom_errorbar(aes(x = Month, y = meanEt_pro, ymin=meanEt_pro, ymax=meanEt_pro+se), position=position_dodge(.9)) +
  geom_col(aes(x = Month, y = meanEt_pro), color = "black") + 
  facet_wrap(~Species, scales = "free", ncol = 2) +
  labs(x = "Measuring period (Month)", y = expression("Ethylene production (µmol  "*h^-1*" "*m^-2*")"), title = expression("Bryophyte ethylene production")) + 
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(2, "lines"),axis.text.x=element_text(angle=60, hjust=1))
#
# Same graph as before, but with functional groups coloured in
field_ARA_wide.5 %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  summarise(meanEt_pro = mean(Et_prod_umol_h_m2, na.rm = TRUE), se = sd(Et_prod_umol_h_m2)/sqrt(length(Et_prod_umol_h_m2)), .by = c(Month, Species, Sp)) %>%
  mutate(BFG = case_when(Sp == "Au" ~ "Short unbranched turf",
                         Sp == "Di" ~ "Tall unbranched turf",
                         Sp == "Hy" | Sp == "Pl" ~ "Weft",
                         Sp == "Po" ~ "Polytrichales",
                         Sp == "Pti" ~ "Leafy liverwort",
                         Sp == "Ra" ~ "Large cushion",
                         Sp == "S" | Sp == "Sli" | Sp == "Sf" ~ "Sphagnum")) %>%
  ggplot() +
  geom_errorbar(aes(x = Month, y = meanEt_pro, ymin=meanEt_pro, ymax=meanEt_pro+se), position=position_dodge(.9)) +
  geom_col(aes(x = Month, y = meanEt_pro, fill = BFG)) + 
  facet_wrap(~Species, scales = "free", ncol = 2) +
  labs(x = "Measuring period (Month)", y = expression("Ethylene production (µmol  "*h^-1*" "*m^-2*")"), title = expression("Bryophyte ethylene production")) + 
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(2, "lines"),axis.text.x=element_text(angle=60, hjust=1))
#
#
#
#      ╔═══════╗
# -- ♪ ♪ Vials ♪ ♪ --
#      ╚═══════╝
#
# Vials in the field
ARA_vialRound.basic %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species),
         Month = case_when(Month == "Feb21" ~ "February",
                           Month == "Mar21" ~ "March",
                           Month == "Jul21" ~ "July")) %>%
  mutate(across(Month, ~ factor(.x, levels=c("February", "March", "July")))) %>%
  summarise(meanEt_pro = mean(Et_prod_vial.Field, na.rm = TRUE), se = sd(Et_prod_vial.Field)/sqrt(length(Et_prod_vial.Field)), .by = c(Month, Species, Sp)) %>%
  mutate(BFG = case_when(Sp == "Au" ~ "Short unbranched turf",
                         Sp == "Di" ~ "Tall unbranched turf",
                         Sp == "Hy" | Sp == "Pl" ~ "Weft",
                         Sp == "Po" ~ "Polytrichales",
                         Sp == "Pti" ~ "Leafy liverwort",
                         Sp == "Ra" ~ "Large cushion",
                         Sp == "S" | Sp == "Sli" | Sp == "Sf" ~ "Sphagnum")) %>%
  ggplot() +
  geom_errorbar(aes(x = Month, y = meanEt_pro, ymin=meanEt_pro, ymax=meanEt_pro+se), position=position_dodge(.9)) +
  geom_col(aes(x = Month, y = meanEt_pro, fill = BFG)) + 
  facet_wrap(~Species, scales = "free", ncol = 5) +
  labs(x = "Measuring period (Month)", y = expression("Ethylene production (µmol  "*h^-1*" "*m^-2*")"), title = expression("Bryophyte ethylene production in vials")) + 
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(2, "lines")) #,axis.text.x=element_text(angle=60, hjust=1)
#
#
# Vials in climate chamber
ARA_vialRound.basic %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species),
         Month = case_when(Month == "Feb21" ~ "February",
                           Month == "Mar21" ~ "March",
                           Month == "Jul21" ~ "July")) %>%
  mutate(across(Month, ~ factor(.x, levels=c("February", "March", "July")))) %>%
  summarise(meanEt_pro = mean(Et_prod_vial.CC, na.rm = TRUE), se = sd(Et_prod_vial.CC)/sqrt(length(Et_prod_vial.CC)), .by = c(Month, Species, Sp)) %>%
  mutate(BFG = case_when(Sp == "Au" ~ "Short unbranched turf",
                         Sp == "Di" ~ "Tall unbranched turf",
                         Sp == "Hy" | Sp == "Pl" ~ "Weft",
                         Sp == "Po" ~ "Polytrichales",
                         Sp == "Pti" ~ "Leafy liverwort",
                         Sp == "Ra" ~ "Large cushion",
                         Sp == "S" | Sp == "Sli" | Sp == "Sf" ~ "Sphagnum")) %>%
  ggplot() +
  geom_errorbar(aes(x = Month, y = meanEt_pro, ymin=meanEt_pro, ymax=meanEt_pro+se), position=position_dodge(.9)) +
  geom_col(aes(x = Month, y = meanEt_pro, fill = BFG)) + 
  facet_wrap(~Species, scales = "free", ncol = 5) +
  labs(x = "Measuring period (Month)", y = expression("Ethylene production (µmol  "*h^-1*" "*m^-2*")"), title = expression("Bryophyte ethylene production in vials in climate chamber")) + 
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(2, "lines")) #,axis.text.x=element_text(angle=60, hjust=1)
#
#
#
#      ╔═════════════════════════╗
# -- ♪ ♪ Field chambers vs vials ♪ ♪ --
#      ╚═════════════════════════╝
#
# Regression plots
#
# Vial (field) vs vial (climate chamber)
vialvsVial_plot <- ARA_vialRound.basic %>%
  mutate(Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species),
         Month = case_when(Month == "Feb21" ~ "February",
                           Month == "Mar21" ~ "March",
                           Month == "Jul21" ~ "July")) %>%
  ggplot(aes(x = Et_prod_vial.Field, y = Et_prod_vial.CC, color = Month)) +
  geom_point() +
  scale_color_viridis_d() +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~Species, scales = "free", ncol = 2) +
  labs(x = expression("Vial in the field (µmol  "*C[2]*H[4]~~h^-1~m^-2*")"), y = expression("Vial in the climate chamber (µmol  "*C[2]*H[4]~~h^-1~m^-2*")"), title = "Vial (field) vs vial (climate chamber)") +
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(1, "lines"), legend.position = "none")
#
# Field chamber vs vial (field)
chambervsVialfield_plot <- ARA_vialRound.basic %>%
  mutate(Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species),
         Month = case_when(Month == "Feb21" ~ "February",
                           Month == "Mar21" ~ "March",
                           Month == "Jul21" ~ "July")) %>%
  ggplot(aes(x = Et_prod_field, y = Et_prod_vial.Field, color = Month)) +
  geom_point() +
  scale_color_viridis_d() +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~Species, scales = "free", ncol = 2) +
  labs(x = expression("Field chamber (µmol  "*C[2]*H[4]~~h^-1~m^-2*")"), y = expression("Vial in the field (µmol  "*C[2]*H[4]~~h^-1~m^-2*")"), title = "Field chamber vs vial (field)") +
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(1, "lines"), legend.position = "none")
#
# Field chamber vs vial (climate chamber)
chambervsVialcc_plot <- ARA_vialRound.basic %>%
  mutate(Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species),
         Month = case_when(Month == "Feb21" ~ "February",
                           Month == "Mar21" ~ "March",
                           Month == "Jul21" ~ "July")) %>%
  ggplot(aes(x = Et_prod_field, y = Et_prod_vial.CC, color = Month)) +
  geom_point() +
  scale_color_viridis_d() +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~Species, scales = "free", ncol = 2) +
  labs(x = expression("Field chamber (µmol  "*C[2]*H[4]~~h^-1~m^-2*")"), y = expression("Vial in the climate chamber (µmol  "*C[2]*H[4]~~h^-1~m^-2*")"), title = "Field chamber vs vial (climate chamber)") +
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(1, "lines"), legend.position = "bottom")
#
# Plot all graphs together
plot_grid(vialvsVial_plot, chambervsVialfield_plot, chambervsVialcc_plot, align = "h", ncol = 3)
#
# For vertical combination each plot facet_wrap(ncol = 5). plot_grid(align = "v", ncol = 3)
# For horizontal combination each plot facet_wrap(ncol = 2). plot_grid(align = "h", ncol = 1)
#
# For plots with color by month:
# ggplot(aes(color = Month)) + scale_color_viridis_d()
# Remove legend from all but the one used for extracting the value: +theme(legend.position = "none")
#
# Extract month legend
chambervsVialcc_legend <- get_legend(chambervsVialcc_plot)
#
# Remove from plot
chambervsVialcc_plot.1 <- chambervsVialcc_plot + theme(legend.position = "none")
#
# Plot
et_plot_grid <- plot_grid(vialvsVial_plot, chambervsVialfield_plot, chambervsVialcc_plot.1, align = "h", ncol = 3)
plot_grid(et_plot_grid, chambervsVialcc_legend, ncol = 1, rel_heights = c(15,1))
#
#
#
#-------  ♪   ARA & drivers ♪ -------
#
# Plot Environmental drivers with AR
#
#
#      ╔══════════════════════════╗
# -- ♪ ♪ Field chambers & Drivers ♪ ♪ --
#      ╚══════════════════════════╝
#
#
# Select necessary columns
field_ARA.plot <- field_ARA_wide.5 %>%
  select(Block:BFG, Soil_moisture:AirT_C, Et_prod_umol_h_m2) %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(Sp = case_when(Sp == "Sli" ~ "Sm",
                        Sp == "S" ~ "S",
                        TRUE ~ Sp)) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21"))))
#
# Format long
field_ARA.plot.long <- field_ARA.plot %>%
  pivot_longer(cols = Soil_moisture:AirT_C, names_to = "Driver", values_to = "Environmental")
#
# Species together
field_ARA.plot.long %>%
  mutate(Driver = case_when(Driver == "AirT_C" ~ "Air temperature (°C)",
                            Driver == "Soil_moisture" ~ "Soil moisture (VWC, %)",
                            Driver == "Soil_temperature" ~ "Soil temperature (°C)",
                            Driver == "PAR" ~ "PAR (µmol pr m² pr s)",
                            TRUE ~ Driver)) %>%
  ggplot(aes(x = Environmental, y = Et_prod_umol_h_m2)) +
  #geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = Month, shape = Species)) +
  scale_shape_manual(values = 1:10) +
  facet_wrap(~Driver, ncol = 2, scales = "free") +
  labs(x = "Environmental driver", y = expression("Ethylene production (µmol "*~~h^-1*" "*m^-2*")"), title = "Bryophyte AR") +
  theme_classic(base_size = 15)
#
# For each species separately
field_ARA.plot.long %>%
  mutate(Driver = case_when(Driver == "AirT_C" ~ "Air temperature",
                            Driver == "Soil_moisture" ~ "Soil moisture",
                            Driver == "Soil_temperature" ~ "Soil temperature",
                            TRUE ~ Driver)) %>%
  #filter(Driver == "Air temperature" | Driver == "Soil moisture") %>%
  ggplot(aes(x = Environmental, y = Et_prod_umol_h_m2)) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_point(aes(color = Month)) +
  ggh4x::facet_grid2(Driver ~ Sp, scales = "free", independent = "all") +
  labs(x = "Environmental driver", y = expression("Ethylene production (µmol  "*~~h^-1*" "*m^-2*")"), title = "Bryophyte AR") +
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=60, hjust=1))
#
#
#
#      ╔═════════════════╗
# -- ♪ ♪ Vials & Drivers ♪ ♪ --
#      ╚═════════════════╝
#
#
# Vials in the field
#
# Select necessary columns
vial_ARA_field.plot <- vial_ARA_field %>%
  select(Block:Species, Round, Habitat, PAR:GWC, Et_prod_umol_h_m2) %>%
  # Add Round names and bryophyte functional groups (BFG)
  mutate(Month = case_when(Round == "A" ~ "Feb21",
                           Round == "B" ~ "Mar21",
                           Round == "C" ~ "Jul21"),
         BFG = case_when(Species == "Au" ~ "Short unbranched turf",
                         Species == "Di" ~ "Tall unbranched turf",
                         Species == "Hy" | Species == "Pl" ~ "Weft",
                         Species == "Po" ~ "Polytrichales",
                         Species == "Pti" ~ "Leafy liverwort",
                         Species == "Ra" ~ "Large cushion",
                         Species == "S" | Species == "Sli" | Species == "Sf" ~ "Sphagnum")) %>%
  relocate(c(Month, Habitat, BFG), .after = Round) %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(Sp = case_when(Sp == "Sli" ~ "Sm",
                        Sp == "S" ~ "S",
                        TRUE ~ Sp)) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Feb21", "Mar21", "Jul21"))))
#
# Format long
vial_ARA_field.plot.long <- vial_ARA_field.plot %>%
  pivot_longer(cols = PAR:GWC, names_to = "Driver", values_to = "Environmental")
#
# Species together
vial_ARA_field.plot.long %>%
  mutate(Driver = case_when(Driver == "AirT_C" ~ "Air temperature (°C)",
                            Driver == "GWC" ~ "Gravimetric water content (GWC, %)",
                            Driver == "PAR" ~ "PAR (µmol pr m² pr s)",
                            TRUE ~ Driver)) %>%
  ggplot(aes(x = Environmental, y = Et_prod_umol_h_m2)) +
  #geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = Month, shape = Species)) +
  scale_shape_manual(values = 1:10) +
  facet_wrap(~Driver, ncol = 2, scales = "free") +
  labs(x = "Environmental driver", y = expression("Ethylene production (µmol "*~~h^-1*" "*m^-2*")"), title = "Bryophyte AR in vials") +
  theme_classic(base_size = 15)
#
# For each species separately
vial_ARA_field.plot.long %>%
  mutate(Driver = case_when(Driver == "AirT_C" ~ "Air temperature",
                            TRUE ~ Driver)) %>%
  ggplot(aes(x = Environmental, y = Et_prod_umol_h_m2)) +
  #geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_point(aes(color = Month)) +
  ggh4x::facet_grid2(Driver ~ Sp, scales = "free", independent = "all") +
  labs(x = "Environmental driver", y = expression("Ethylene production (µmol  "*~~h^-1*" "*m^-2*")"), title = "Bryophyte AR in vials") +
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=60, hjust=1))
#
#
# Vials in climate chamber
#
# Select necessary columns
vial_ARA_CC.plot <- vial_ARA_climateChamber %>%
  select(Block:Species, Round, Habitat, PAR:GWC, Et_prod_umol_h_m2) %>%
  # Add Round names and bryophyte functional groups (BFG)
  mutate(Month = case_when(Round == "A5" ~ "Feb21",
                           Round == "B5" ~ "Mar21",
                           Round == "C5" ~ "Jul21"),
         BFG = case_when(Species == "Au" ~ "Short unbranched turf",
                         Species == "Di" ~ "Tall unbranched turf",
                         Species == "Hy" | Species == "Pl" ~ "Weft",
                         Species == "Po" ~ "Polytrichales",
                         Species == "Pti" ~ "Leafy liverwort",
                         Species == "Ra" ~ "Large cushion",
                         Species == "S" | Species == "Sli" | Species == "Sf" ~ "Sphagnum")) %>%
  relocate(c(Month, Habitat, BFG), .after = Round) %>%
  mutate(Sp = Species,
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(Sp = case_when(Sp == "Sli" ~ "Sm",
                        Sp == "S" ~ "S",
                        TRUE ~ Sp)) %>%
  mutate(across(Month, ~ factor(.x, levels=c("Feb21", "Mar21", "Jul21"))))
#
# Format long
vial_ARA_CC.plot.long <- vial_ARA_CC.plot %>%
  pivot_longer(cols = PAR:GWC, names_to = "Driver", values_to = "Environmental")
#
# Species together
vial_ARA_CC.plot.long %>%
  mutate(Driver = case_when(Driver == "AirT_C" ~ "Air temperature (°C)",
                            Driver == "GWC" ~ "Gravimetric water content (GWC, %)",
                            Driver == "PAR" ~ "PAR (µmol pr m² pr s)",
                            TRUE ~ Driver)) %>%
  ggplot(aes(x = Environmental, y = Et_prod_umol_h_m2)) +
  #geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_point(aes(color = Month, shape = Species)) +
  scale_shape_manual(values = 1:10) +
  facet_wrap(~Driver, ncol = 2, scales = "free") +
  labs(x = "Environmental driver", y = expression("Ethylene production (µmol "*~~h^-1*" "*m^-2*")"), title = "Bryophyte AR in vials") +
  theme_classic(base_size = 15)
#
# For each species separately
vial_ARA_CC.plot.long %>%
  mutate(Driver = case_when(Driver == "AirT_C" ~ "Air temperature",
                            TRUE ~ Driver)) %>%
  ggplot(aes(x = Environmental, y = Et_prod_umol_h_m2)) +
  #geom_smooth(method = "lm", se = TRUE, color = "black") +
  geom_point(aes(color = Month)) +
  ggh4x::facet_grid2(Driver ~ Sp, scales = "free", independent = "all") +
  labs(x = "Environmental driver", y = expression("Ethylene production (µmol  "*~~h^-1*" "*m^-2*")"), title = "Bryophyte AR in vials") +
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom", axis.text.x=element_text(angle=60, hjust=1))
#
#
#
#-------  ♪   N2 fixation   ♪ -------
#
# Regression plot of N2-fixation and ethylene production 
# Only for ethylene production values greater than 0
# The only species that had positive ethylene production were the three sphagnum species
vial_15N.2 %>%
  filter(Et_prod_umol_h_m2 > 0) %>% # Remove values below detection limit
  mutate(Species = case_when(Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>% 
  ggplot(aes(x = N_h_m2, y = Et_prod_umol_h_m2)) + #, color = Species)) +
  geom_point(aes(shape = Species)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept=0, slope=3)+ # Theoretical model relationship of 3:1 AR:N2
  #facet_wrap(~Species) +
  coord_cartesian(xlim = c(0,40)) +
  geom_text(x = 15, y = 4, label = lm_eqn(vial_15N.2$N_h_m2, vial_15N.2$Et_prod_umol_h_m2), parse = TRUE) +
  annotate("text", x = 6, y = 6, label = as.character(expression(paste(italic(y) ==  3 %.% italic(x)))), parse = TRUE) +
  labs(x = expression("Fixed nitrogen (µg "*N[2]~~h^-1~m^-2*")"), y = expression("Ethylene production (µmol  "*C[2]*H[4]~~h^-1~m^-2*")"), title = expression("Sphagnum "*N[2]*"-fixation and ethylene production")) +
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom")
#
# The reverse:
# y = N2-fixed, x = ethylene produced
vial_15N.2 %>%
  filter(Et_prod_umol_h_m2 > 0) %>% # Remove values below detection limit
  mutate(Species = case_when(Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum majus",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>% 
  ggplot(aes(y = N_h_m2, x = Et_prod_umol_h_m2)) + #, color = Species)) +
  geom_point(aes(shape = Species)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept=0, slope=1/3)+ # Theoretical model relationship of 3:1 AR:N2
  #facet_wrap(~Species) +
  coord_cartesian(ylim = c(0,40)) +
  geom_text(x = 4, y = 35, label = lm_eqn(vial_15N.2$Et_prod_umol_h_m2, vial_15N.2$N_h_m2), parse = TRUE) +
  annotate("text", x = 4, y = 6, label = as.character(expression(paste(italic(y) ==  frac(1,3) %.% italic(x)))), parse = TRUE) +
  labs(y = expression("Fixed nitrogen (µg "*~~N[2]~h^-1~m^-2*")"), x = expression("Ethylene production (µmol  "*C[2]*H[4]~h^-1~m^-2*")"), title = expression("Sphagnum "*N[2]*"-fixation and ethylene production")) +
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom")
#
#
#
#-------  ♪   Outliers      ♪ -------
#
# Ethylene production has several negative values:
# Cleveland dot plot
dotchart(field_ARA_wide.4$Et_prod_umol_h_m2, 
         main="Cleveland plot - Ethylene production", xlab = "Observed values", 
         pch = 19, color = hcl.colors(12), 
         labels = field_ARA_wide.4$Block, 
         groups = field_ARA_wide.4$Round,
         gpch = 12, gcolor = 1)
#
# Ethylene production per block
field_ARA_wide.4_block <- field_ARA_wide.4 %>%
  select(1:3, Et_prod_umol_h_m2) %>%
  pivot_wider(names_from = Block, values_from = Et_prod_umol_h_m2)
#
# Ethylene production per Round
field_ARA_wide.4_round <- field_ARA_wide.4 %>%
  select(1:3, Et_prod_umol_h_m2) %>%
  mutate(across(Round, ~as.character(.x))) %>%
  mutate(Round = case_when(Round == "1" ~ "One",
                           Round == "2" ~ "Two",
                           Round == "3" ~ "Three",
                           Round == "4" ~ "Four",
                           Round == "5" ~ "Five",
                           Round == "6" ~ "Six",
                           Round == "7" ~ "Seven",
                           Round == "8" ~ "Eight",
                           Round == "9" ~ "Nine",
                           Round == "10" ~ "Ten",
                           Round == "11" ~ "Eleven",
                           TRUE ~ Round)) %>%
  pivot_wider(names_from = Round, values_from = Et_prod_umol_h_m2)
#
# Ethylene production per species
field_ARA_wide.4_species <- field_ARA_wide.4 %>%
  select(1:3, Et_prod_umol_h_m2) %>%
  pivot_wider(names_from = Species, values_from = Et_prod_umol_h_m2)
#
#
plot_ly(field_ARA_wide.4, x = ~Et_prod_umol_h_m2, y = ~Species, name = "Ethylene production", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  layout(title = "Ethylene production per species", xaxis = list(title = "Ethylene production (µmol pr h pr m2)"), margin = list(l = 100))
#
# Separate by block
plot_ly(field_ARA_wide.4_block, x = ~B, y = ~Species, name = "Blue", type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>% 
  add_trace(x = ~P, y = ~Species, name = "Purple",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~R, y = ~Species, name = "Red",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~W, y = ~Species, name = "White",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Y, y = ~Species, name = "Yellow",type = 'scatter', mode = "markers", marker = list(color = "#F0E442")) %>%
  layout(title = "Ethylene production per species", xaxis = list(title = "Ethylene production (µmol pr h pr m2)"), margin = list(l = 100))
#
# Separate by Time
plot_ly(field_ARA_wide.4_round, x = ~One, y = ~Species, name = "1", type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Two, y = ~Species, name = "2",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Three, y = ~Species, name = "3",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Four, y = ~Species, name = "4",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Five, y = ~Species, name = "5",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Six, y = ~Species, name = "6",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Seven, y = ~Species, name = "7",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Eight, y = ~Species, name = "8",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Nine, y = ~Species, name = "9",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Ten, y = ~Species, name = "10",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Eleven, y = ~Species, name = "11",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  layout(title = "Ethylene production per species", xaxis = list(title = "Ethylene production (µmol pr h pr m2)"), margin = list(l = 100))
#
# Separate by Time
plot_ly(field_ARA_wide.4_round, x = ~One, y = ~Species, name = "一", type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Two, y = ~Species, name = "二",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Three, y = ~Species, name = "三",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Four, y = ~Species, name = "四",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Five, y = ~Species, name = "五",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Six, y = ~Species, name = "六",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Seven, y = ~Species, name = "七",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Eight, y = ~Species, name = "八",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Nine, y = ~Species, name = "九",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Ten, y = ~Species, name = "十",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Eleven, y = ~Species, name = "十一",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  layout(title = "Ethylene production per species", xaxis = list(title = "Ethylene production (µmol pr h pr m2)"), margin = list(l = 100))
#
# Separate by Species over time
plot_ly(field_ARA_wide.4_species, x = ~Au, y = ~Round, name = "Aulacomnium turgidum", type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Di, y = ~Round, name = "Dicranum scoparium",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Hy, y = ~Round, name = "Hylocomium splendens",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Pl, y = ~Round, name = "Pleurozium schreberi",type = 'scatter', mode = "markers", marker = list(color = "#0072B2")) %>%
  add_trace(x = ~Po, y = ~Round, name = "Polytrichum commune",type = 'scatter', mode = "markers", marker = list(color = "#CC79A7")) %>%
  add_trace(x = ~Pti, y = ~Round, name = "Ptilidium ciliare",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Ra, y = ~Round, name = "Racomitrium lanuginosum",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~S, y = ~Round, name = "Sphagnum sp",type = 'scatter', mode = "markers", marker = list(color = "#009E73")) %>%
  add_trace(x = ~Sf, y = ~Round, name = "Sphagnum fuscum",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  add_trace(x = ~Sli, y = ~Round, name = "Sphagnum lindbergii",type = 'scatter', mode = "markers", marker = list(color = "#D55E00")) %>%
  layout(title = "Ethylene production per species", xaxis = list(title = "Ethylene production (µmol pr h pr m2)"), margin = list(l = 100))
#
#
x <- field_ARA_wide %>%
  mutate(Acet_diff = Acet_conc_ppm_T0 - Acet_conc_ppm_T1) %>%
  filter(Acet_diff <= 0)


y <- field_ARA_wide %>%
  mutate(Ethyl_diff = Ethyl_conc_ppm_T0 - Ethyl_conc_ppm_T1) %>%
  filter(Ethyl_diff <= 0)


z <- field_ARA_wide.4 %>%
  mutate(Et_prod_pos = Et_prod_umol_h_m2 >= 0)
z.N <- z %>%
  filter(!Et_prod_pos)
z.P <- z %>%
  filter(Et_prod_pos)

z.N %>% count(Round)
z.P %>% count(Round)

z.N %>% count(Species)
z.P %>% count(Species)



# Check if any moss colony never does N2-fixation
emptyBryophyte <- field_ARA_wide.5 %>%
  select(Block, Species, Round, Et_prod_umol_h_m2) %>%
  group_by(Block, Species) %>%
  summarise(Et_prod = sum(Et_prod_umol_h_m2)) %>%
  ungroup()
#
# All have at least some ethylene production at some point
#
# Which seasons
emptyBryophyte_when <- field_ARA_wide.5 %>%
  select(Block, Species, Round, Et_prod_umol_h_m2) %>%
  mutate(season = case_when(Round == "1" | Round == "2" | Round == "3" ~ "Fall1",
                            Round == "4" ~ "Winter",
                            Round == "5" | Round == "6" ~ "Spring",
                            Round == "7" | Round == "8" | Round == "9" ~ "Summer",
                            Round == "10" | Round == "11" ~ "Fall2")) %>%
  group_by(Block, Species, season) %>%
  summarise(Et_prod = sum(Et_prod_umol_h_m2)) %>%
  ungroup()
#
# Colonies where nothing happens after the first autumn
x <- emptyBryophyte_when %>%
  pivot_wider(names_from = season, values_from = Et_prod) %>%
  mutate(late = Fall2 + Winter + Spring + Summer) %>%
  filter(late == 0)
#
# A few colonies (8) only show any production during the first autumn
# Remove
field_ARA_wide.5 %>%
  filter(Block != "B" | Species != "Au") %>%
  filter(Block != "P" | Species != "Pti") %>%
  filter(Block != "P" | Species != "Sf") %>%
  filter(Block != "P" | Species != "Hy") %>%
  filter(Block != "R" | Species != "Pti") %>%
  filter(Block != "R" | Species != "Sf") %>%
  filter(Block != "W" | Species != "Po") %>%
  filter(Block != "Y" | Species != "Di")

#
#
#
#=======  ■  { The End }    ■ =======