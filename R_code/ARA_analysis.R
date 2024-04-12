# Analysis - ARA
#
#
# By Emil A.S. Andersen
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
#library(lme4)
#
#
#
#=======  ♠   Load data     ♠ =======
# Import ID's
ID_info <- read_xlsx("Data_raw/Data_ID.xlsx")
#
# Import field and vial datasets
field_ARA <- read_csv("Data_clean/field_ARA.csv", col_names = TRUE)
vial_ARA <- read_csv("Data_clean/vial_ARA.csv", col_names = TRUE)
#
# Air temperature
AirT_wetland <- read_csv("Data_clean/AirT_wetland.csv", col_names = TRUE)
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
# Vial size
Vial_vol_L <- 20/1000 # 20mL vials, the small ones, and 22 for the taller ones. Does not take into account the actual headspace after mosses
Vial_area_m2 <- (1.1^2*pi*2)/10000
#
#
# Constants
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
#
#
#=======  ♦   Main data     ♦ =======
#
#
#------- • Environmental -------
AirT_wetland.1 <- AirT_wetland %>%
  select(Date, Time, AirT_C) %>%
  rename("Tid" = Time)


EM50_Heath.1 <- EM50_Heath %>%
  rowwise() %>%
  mutate(Soil_moisture = mean(c(Soil_moisture_B, Soil_moisture_P, Soil_moisture_R, Soil_moisture_W, Soil_moisture_Y, Soil_moisture_G), na.rm = TRUE),
         Soil_temperature = mean(c(Soil_temperature_B, Soil_temperature_P, Soil_temperature_R, Soil_temperature_W, Soil_temperature_Y, Soil_temperature_G), na.rm = TRUE)) %>%
  mutate(Soil_moisture = Soil_moisture*100) %>%
  ungroup() %>%
  select(Date_time, Soil_moisture, Soil_temperature, PAR) %>%
  separate_wider_delim(Date_time, delim = " ", names = c("Date", "Time"), too_few = "debug", too_many = "debug") %>%
  mutate(Date = ymd(Date),
         Tid = hms::as_hms(Time)) 

EM50_Heath.2 <- EM50_Heath.1 %>%
  select(Date, Tid, Soil_moisture, Soil_temperature, PAR) %>%
  filter(!is.na(Soil_moisture) & !is.na(Soil_temperature))

# write_csv(EM50_Heath.2, "Data_clean/Heath_EM50_simple.csv")


EM50_Wetland.1 <- EM50_Wetland %>%
  rowwise() %>%
  mutate(Soil_moisture = mean(c(Soil_moisture_B, Soil_moisture_P, Soil_moisture_R, Soil_moisture_W, Soil_moisture_Y, Soil_moisture_G), na.rm = TRUE),
         Soil_temperature = mean(c(Soil_temperature_B, Soil_temperature_P, Soil_temperature_R, Soil_temperature_W, Soil_temperature_Y, Soil_temperature_G), na.rm = TRUE)) %>%
  mutate(Soil_moisture = Soil_moisture*100) %>%
  ungroup() %>%
  select(Date_time, Soil_moisture, Soil_temperature, PAR) %>%
  separate_wider_delim(Date_time, delim = " ", names = c("Date", "Time"), too_few = "debug", too_many = "debug") %>%
  mutate(Date = ymd(Date),
         Tid = hms::as_hms(Time)) 

EM50_Wetland.2 <- EM50_Wetland.1 %>%
  select(Date, Tid, Soil_moisture, Soil_temperature, PAR) %>%
  filter(!is.na(Soil_moisture) & !is.na(Soil_temperature))




EM50_Heath.1 %>%
  ggplot() + geom_point(aes(x = Date_time, y = Soil_moisture))

EM50_Heath.1 %>%
  ggplot() + geom_point(aes(x = Date_time, y = Soil_temperature))

EM50_Heath.1 %>%
  #filter(Date_time >= ymd(20210601) & Date_time <= ymd(20210630)) %>%
  ggplot() + geom_point(aes(x = Date_time, y = PAR))

EM50_Wetland.1 %>%
  ggplot() + geom_point(aes(x = Date_time, y = Soil_moisture))

EM50_Wetland.1 %>%
  ggplot() + geom_point(aes(x = Date_time, y = Soil_temperature))

EM50_Wetland.1 %>%
  ggplot() + geom_point(aes(x = Date_time, y = PAR))


#
#
#------- • Field data -------
#
# Remove chamber test and combine with temperature
field_ARA.2 <- field_ARA %>%
  filter(Species != "B") %>%
  # Round timestamp to nearest hour!
  mutate(Date_time = ymd(Date) + hms(Timestamp)) %>%
  mutate(Tid = round_date(ymd_hms(Date_time), unit = "hour")) %>%
  mutate(Tid = hms::as_hms(Tid)) %>%
  # Combine with air temperature
  left_join(AirT_wetland.1, by = join_by(Date, Tid)) %>%
  left_join(EM50_Heath.2, by = join_by(Date, Tid)) %>%
  # Remove temporary variables
  select(!c("Date_time", "Tid"))
#
# Pivot wider and combine:
# Timestamp
field_ARA.Time <- field_ARA.2 %>% 
  select(!c(Time_nr, Date, Chamber_no, Chain_type, Temp_approx_C, Ethyl_conc_ppm, Acet_conc_prC, AirT_C, Soil_moisture, Soil_temperature, PAR)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = Timestamp)
#
# Ethylene
field_ARA.Ethyl <- field_ARA.2 %>% 
  select(!c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C, Acet_conc_prC, AirT_C, Soil_moisture, Soil_temperature, PAR)) %>%
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
  select(!c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C, Ethyl_conc_ppm, AirT_C, Soil_moisture, Soil_temperature, PAR)) %>%
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
# Use the mean of T0 and T1 for air and soil temperature and soil moisture
#
# Air temperature
field_ARA.AirT <- field_ARA.2 %>% 
  select(!c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C, Ethyl_conc_ppm, Acet_conc_prC, Soil_moisture, Soil_temperature, PAR)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = AirT_C) %>%
  rowwise() %>%
  mutate(AirT_C = mean(c(T0, T1), na.rm = TRUE)) %>%
  ungroup() %>%
  select(!c(T0, T0x, T1, T1x, T2, T3, T4)) #%>%
  #rename("AirT_C" = T0)
#
# Soil temperature
field_ARA.SoilT <- field_ARA.2 %>% 
  select(!c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C, Ethyl_conc_ppm, Acet_conc_prC, AirT_C, Soil_moisture, PAR)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = Soil_temperature) %>%
  rowwise() %>%
  mutate(Soil_temperature = mean(c(T0,T1), na.rm = TRUE)) %>%
  ungroup() %>%
  select(!c(T0, T0x, T1, T1x, T2, T3, T4)) #%>%
  #rename("Soil_temperature" = T0)
#
# Soil moisture
field_ARA.SoilM <- field_ARA.2 %>% 
  select(!c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C, Ethyl_conc_ppm, Acet_conc_prC, AirT_C, Soil_temperature, PAR)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = Soil_moisture) %>%
  rowwise() %>%
  mutate(Soil_moisture = mean(c(T0,T1), na.rm = TRUE)) %>%
  ungroup() %>%
  select(!c(T0, T0x, T1, T1x, T2, T3, T4)) #%>%
  #rename("Soil_moisture" = T0)
#
# PAR
field_ARA.PAR <- field_ARA.2 %>% 
  select(!c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C, Ethyl_conc_ppm, Acet_conc_prC, AirT_C, Soil_temperature, Soil_moisture)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = PAR) %>%
  rowwise() %>%
  mutate(PAR = mean(c(T0,T1), na.rm = TRUE)) %>%
  ungroup() %>%
  select(!c(T0, T0x, T1, T1x, T2, T3, T4))
#
# Join 
field_ARA_wide <-  left_join(field_ARA.Time, field_ARA.AirT, by = join_by(Block, Species, Round)) %>%
  left_join(field_ARA.SoilT, by = join_by(Block, Species, Round)) %>%
  left_join(field_ARA.SoilM, by = join_by(Block, Species, Round)) %>%
  left_join(field_ARA.PAR, by = join_by(Block, Species, Round)) %>%
  left_join(field_ARA.Ethyl, by = join_by(Block, Species, Round)) %>%
  left_join(field_ARA.Acet, by = join_by(Block, Species, Round))
#
# Do the math
#
#
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
  mutate(Et_prod_umol_h_m2 = Corr_Et_prod_pr_h.1 * (Ch_vol_L * p) / (R_const * AirT_C+273) / Ch_area_m2) # Temperature set from T0 at Wetland !!!
#
# Set negative production to 0
field_ARA_wide.5 <- field_ARA_wide.4 %>%
  mutate(Et_prod_umol_h_m2 = if_else(Et_prod_umol_h_m2 < 0, 0, Et_prod_umol_h_m2))

field_ARA_wide.export <- field_ARA_wide.5 %>%
  select(Block, Species, Round, AirT_C, Soil_temperature, Soil_moisture, PAR, Et_prod_umol_h_m2) %>%
  rename("AirT" = AirT_C)


#write_csv(field_ARA_wide.export, "export/Q1_ARA2.csv", na = "NA")


ggplot(data = field_ARA_wide.5, aes(x = Et_prod_umol_h_m2)) + geom_histogram()

#
#
#------- • Vial data -------
#
# 
vial_ARA.1 <- vial_ARA %>%
  mutate(Species = str_replace_all(Species, "M", "B")) %>% #Replace M for Myren with B for Blank
  filter(!str_starts(Species, "v")) #Remove vial tests
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
  filter(!str_starts(Species, "B")) %>%
  mutate(Habitat = if_else(str_starts(Species, "S"), "M", "H")) %>%
  left_join(vial_ARA.b, by = join_by(Round, Habitat), multiple = "all") %>%
  mutate(Time24 = hour(seconds_to_period(hms(Time_from_T0) - hms(Timestamp)))*60 + minute(seconds_to_period(hms(Time_from_T0) - hms(Timestamp))) + 24*60) %>%
  mutate(Et_prod_ppm_pr_h = (Ethyl_conc_ppm - Ethyl_blank)/(Time24),
         Act_lost_ppm_pr_h = (Acet_conc_prC*10000 - Acet_blank*10000)/(Time24)) %>%
  mutate(Corr_Et_prod_pr_h = Et_prod_ppm_pr_h - (Act_lost_ppm_pr_h*(Ethyl_blank/Acet_blank))) %>%
  mutate(Temp_approx_C = case_when(is.na(Temp_approx_C) & Round == "C" ~ 15,
                                   is.na(Temp_approx_C) & Round == "C5" ~ 5,
                                   TRUE ~ Temp_approx_C)) %>%
  mutate(Et_prod_umol_h_m2 = Corr_Et_prod_pr_h * (Vial_vol_L * p) / (R_const * Temp_approx_C+273) / Vial_area_m2)
#
vial_ARA.3 <- vial_ARA.2 %>%
  mutate(Et_prod_umol_h_m2 = if_else(Et_prod_umol_h_m2 < 0, 0, Et_prod_umol_h_m2))

vial_ARA.3 %>%
  ggplot(aes(y = Et_prod_umol_h_m2, x = Round)) + geom_boxplot() + facet_wrap(~Species)


# ??
#mutate(across(Timestamp, ~hm(.x)))# %>%
#
#
#------- • Temperature, moisture and PAR -------
#
# 

# Multivariate analysis on environmental data
# NMDS

morse.dist <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/morsecodes-dist.txt', row.names = 1, head = T)
names (morse.dist) <- rownames (morse.dist)
NMDS <- metaMDS (morse.dist)
NMDS
par (mfrow = c(1,2))
ordiplot (NMDS, cex = 1.5, type = 't')
stressplot (NMDS)

morse.attr <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/morsecodes-attr.txt', row.names = 1, head = T)
ef <- envfit (NMDS, morse.attr)
ordiplot (NMDS, cex = 1.5, type = 't')
plot (ef)





vltava.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1)
NMDS <- metaMDS (vltava.spe)


Field_environ <- field_ARA_wide.5 %>%
  select(Block, Species, Round, AirT_C, Soil_temperature, Soil_moisture, PAR) %>%
  mutate(AirT = AirT_C + 273,
         Soil_temperature = Soil_temperature + 273) %>% # remove negative temperature by converting to kelvin
  select(-AirT_C)

Field_environ.1 <- Field_environ %>%
  select(!c(Block, Species, Round))

Field_sp <- as.data.frame(model.matrix( ~ Species - 1, data=field_ARA_wide.5 ))
Field_environ.2 <- Field_environ %>%
  cbind(Field_sp) %>%
  rename("Au" = "SpeciesAu",
         "Di" = "SpeciesDi",
         "Hy" = "SpeciesHy",
         "Pl" = "SpeciesPl",
         "Po" = "SpeciesPo",
         "Pti" = "SpeciesPti",
         "Ra" = "SpeciesRa",
         "Sf" = "SpeciesSf",
         "Sli" = "SpeciesSli",
         "S" = "SpeciesS")

Field_environ_AirT <- Field_environ %>%
  select(Block, Species, Round, AirT) %>%
  pivot_wider(names_from = Species, values_from = AirT) %>%
  rename("AirT_Au" = "Au",
         "AirT_Di" = "Di",
         "AirT_Hy" = "Hy",
         "AirT_Pl" = "Pl",
         "AirT_Po" = "Po",
         "AirT_Pti" = "Pti",
         "AirT_Ra" = "Ra",
         "AirT_Sf" = "Sf",
         "AirT_Sli" = "Sli",
         "AirT_S" = "S")
Field_environ_SoilT <- Field_environ %>%
  select(Block, Species, Round, Soil_temperature) %>%
  pivot_wider(names_from = Species, values_from = Soil_temperature) %>%
  rename("SoilT_Au" = "Au",
         "SoilT_Di" = "Di",
         "SoilT_Hy" = "Hy",
         "SoilT_Pl" = "Pl",
         "SoilT_Po" = "Po",
         "SoilT_Pti" = "Pti",
         "SoilT_Ra" = "Ra",
         "SoilT_Sf" = "Sf",
         "SoilT_Sli" = "Sli",
         "SoilT_S" = "S")
Field_environ_SoilM <- Field_environ %>%
  select(Block, Species, Round, Soil_moisture) %>%
  pivot_wider(names_from = Species, values_from = Soil_moisture) %>%
  rename("SoilM_Au" = "Au",
         "SoilM_Di" = "Di",
         "SoilM_Hy" = "Hy",
         "SoilM_Pl" = "Pl",
         "SoilM_Po" = "Po",
         "SoilM_Pti" = "Pti",
         "SoilM_Ra" = "Ra",
         "SoilM_Sf" = "Sf",
         "SoilM_Sli" = "Sli",
         "SoilM_S" = "S")
Field_environ_PAR <- Field_environ %>%
  select(Block, Species, Round, PAR) %>%
  pivot_wider(names_from = Species, values_from = PAR) %>%
  rename("PAR_Au" = "Au",
         "PAR_Di" = "Di",
         "PAR_Hy" = "Hy",
         "PAR_Pl" = "Pl",
         "PAR_Po" = "Po",
         "PAR_Pti" = "Pti",
         "PAR_Ra" = "Ra",
         "PAR_Sf" = "Sf",
         "PAR_Sli" = "Sli",
         "PAR_S" = "S")

Field_environ.3 <- full_join(Field_environ_AirT, Field_environ_SoilT, by = join_by(Block, Round)) %>%
  full_join(Field_environ_SoilM, by = join_by(Block, Round)) %>%
  full_join(Field_environ_PAR, by = join_by(Block, Round)) %>%
  select(!c(Block, Round))


x <- Field_environ %>%
  filter(Species == "Au") %>%
  select(-c(Block, Species, Round))

# Modified from https://stirlingcodingclub.github.io/ordination/
x <- Field_environ.1
x <- scale(x)
PCA_environ <- prcomp(x)
B_rw <- which(Field_environ[,1] == "B")
P_rw <- which(Field_environ[,1] == "P")
R_rw <- which(Field_environ[,1] == "R")
W_rw <- which(Field_environ[,1] == "W")
Y_rw <- which(Field_environ[,1] == "Y")
plot(x = PCA_environ$x[,1], y = PCA_environ$x[,2], asp = 1, cex.lab = 1.25, 
     cex.axis = 1.25, xlab = "PC1", ylab = "PC2")
points(x = PCA_environ$x[B_rw,1], y = PCA_environ$x[B_rw,2], pch = 20, col = "blue")
points(x = PCA_environ$x[P_rw,1], y = PCA_environ$x[P_rw,2], pch = 20, col = "purple")
points(x = PCA_environ$x[R_rw,1], y = PCA_environ$x[R_rw,2], pch = 20, col = "red")
points(x = PCA_environ$x[W_rw,1], y = PCA_environ$x[W_rw,2], pch = 20, col = "white")
points(x = PCA_environ$x[Y_rw,1], y = PCA_environ$x[Y_rw,2], pch = 20, col = "yellow")
biplot(PCA_environ, cex = 0.8, asp = 1)


NMDS_environ <- metaMDS(Field_environ.1, distance = "bray")#, autotransform = TRUE)
par (mfrow = c(1,2))
plot(NMDS_environ, type = "n")
points(NMDS_environ, display = "sites", cex = 0.8, pch=21, col="black", bg="white")
text(NMDS_environ, display = "spec", cex=0.7, col="blue")
stressplot(NMDS_environ)
par (mfrow = c(1,1))


pairs(x = Field_environ.1, gap = 0, cex.labels = 0.5)
Field_environ_cor <- cor(Field_environ.2, method = "kendall")
corrplot::corrplot(Field_environ_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt =  45)


PCA_field <- rda( ~ . ,Field_environ)
ordiplot(PCA_environ, type = "t")


#
#
#
#=======  §§  Statistics    §§ =======
#-------  »   Q1            « -------
# 1.	How active can mosses potentially be in their N2-fixation depending on moisture, temperature, and light availability?
#
Q1_ARA <- field_ARA_wide.5 %>%
  mutate(across(Round, ~as.character(.x))) %>%
  mutate(across(c(Block, Species, Round), ~as.factor(.x)))
#
# Transform data
Q1_ARA <- Q1_ARA %>%
  select(1:3, AirT_C, Soil_temperature, Soil_moisture, PAR, Et_prod_umol_h_m2) %>%
  mutate(logEt_prod = log(Et_prod_umol_h_m2+2),
         sqrtEt_prod = sqrt(Et_prod_umol_h_m2),
         cubeEt_prod = Et_prod_umol_h_m2^(1/9),
         sqEt_prod = Et_prod_umol_h_m2^2,
         ashinEt_prod = log(Et_prod_umol_h_m2 + sqrt(Et_prod_umol_h_m2^2 + 1)), # inverse hyperbolic sine transformation
         arcEt_prod = asin(sqrt(((Et_prod_umol_h_m2)/10000))))
#
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
#
# model output
Anova(lme1, type=2)
#
#


Q1_ARA %>%
  mutate(MP = case_when(Round == 1 ~ "Sept20",
                        Round == 2 ~ "Oct20",
                        Round == 3 ~ "Nov20",
                        Round == 4 ~ "Feb21",
                        Round == 5 ~ "Mar21",
                        Round == 6 ~ "May21",
                        Round == 7 ~ "Jun21",
                        Round == 8 ~ "Jul21",
                        Round == 9 ~ "Sept21",
                        Round == 10 ~ "Oct21",
                        Round == 11 ~ "Nov21")) %>%
  mutate(across(MP, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  ggplot(aes(y = Et_prod_umol_h_m2, x = MP)) + geom_boxplot() + facet_wrap(~Species, scales = "free")



model <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round)*Species ,data=Q1_ARA, ziformula=~1, family=gaussian)
Anova(model, type = c("II"), test.statistic = c("Chi"), component = "cond")

# Au
modelAu <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round) ,data=Q1_ARA[Q1_ARA$Species=="Au",], ziformula=~1, family=gaussian)
Anova(modelAu, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ     DF    p
# 113.6 10  < 2.2e-16 
emmeans(modelAu,"Round")
#
#
# Di
modelDi <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Di",], ziformula=~1, family=gaussian)
Anova(modelDi, type = c("II"), test.statistic = c("Chi"), component = "cond")
#
# PAR removed as very low probability
modelDi2 <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ AirT_C*Soil_temperature*Soil_moisture,data=Q1_ARA[Q1_ARA$Species=="Di",], ziformula=~1, family=gaussian)
Anova(modelDi2, type = c("II"), test.statistic = c("Chi"), component = "cond")

modelDi3 <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ AirT_C*Soil_moisture,data=Q1_ARA[Q1_ARA$Species=="Di",], ziformula=~1, family=gaussian)
Anova(modelDi3, type = c("II"), test.statistic = c("Chi"), component = "cond")

Q1_ARA %>%
  filter(Species == "Di") %>%
  ggplot() +
  geom_point(aes(x = Soil_moisture, y = sqrt(Et_prod_umol_h_m2)))

Q1_ARA %>%
  filter(Species == "Di") %>%
  ggplot() +
  geom_point(aes(x = factor(Round, levels = order(levels(Round))), y = Soil_moisture))

Q1_ARA %>%
  #filter(Species == "Di") %>%
  ggplot() +
  geom_point(aes(x = Soil_temperature, y = Soil_moisture))

Q1_ARA %>%
  #filter(Species == "Di") %>%
  ggplot() +
  geom_point(aes(x = Soil_temperature, y = Soil_moisture))


# χ     DF    p
# 202.97 10  < 2.2e-16
emmeans(modelDi,"Round")
#
#
# Hy
modelHy <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round) ,data=Q1_ARA[Q1_ARA$Species=="Hy",], ziformula=~1, family=gaussian)
Anova(modelHy, type = c("II"), test.statistic = c("Chi"), component = "cond")

modelHy2 <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ AirT_C+Soil_temperature+Soil_moisture+PAR,data=Q1_ARA[Q1_ARA$Species=="Hy",], ziformula=~1, family=gaussian)
modelHy2 <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ AirT_C*Soil_temperature*Soil_moisture+PAR,data=Q1_ARA[Q1_ARA$Species=="Hy",], ziformula=~1, family=gaussian)
Anova(modelHy2, type = c("II"), test.statistic = c("Chi"), component = "cond")

Q1_ARA %>%
  filter(Species == "Hy") %>%
  ggplot() +
  geom_point(aes(x = Soil_temperature, y = sqrt(Et_prod_umol_h_m2)))

Q1_ARA %>%
  filter(Species == "Di") %>%
  ggplot() +
  geom_point(aes(x = PAR, y = Soil_moisture))

# χ     DF    p
# 249.08 10  < 2.2e-16
emmeans(modelHy,"Round")
#
#
# Pl
modelPl <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Pl",], ziformula=~1, family=gaussian)
Anova(modelPl, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ     DF    p
# 182.64 10  < 2.2e-16
emmeans(modelPl,"Round")
#
#
# Po
modelPo <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Po",], ziformula=~1, family=gaussian)
Anova(modelPo, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ     DF    p
# 215.56 10  < 2.2e-16
emmeans(modelPo,"Round")
#
#
# Pti
modelPti <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Pti",], ziformula=~1, family=gaussian)
Anova(modelPti, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ     DF    p
# 53.974 10  4.906e-08
emmeans(modelPti,"Round")
#
#
# Ra
modelRa <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Ra",], ziformula=~1, family=gaussian)
Anova(modelRa, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ     DF    p
# 40.684 10  1.283e-05
emmeans(modelRa,"Round")
#
#
# S
modelS <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="S",], ziformula=~1, family=gaussian)
Anova(modelS, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ     DF    p
# 35.241 10  0.0001136
emmeans(modelS,"Round")
#
#
# Sf
modelSf <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Sf",], ziformula=~1, family=gaussian)
Anova(modelSf, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ     DF    p
# 81.666 10  2.366e-13
emmeans(modelSf,"Round")
#
#
# Sli
modelSli <- glmmTMB(sqrt(Et_prod_umol_h_m2) ~ factor(Round),data=Q1_ARA[Q1_ARA$Species=="Sli",], ziformula=~1, family=gaussian)
Anova(modelSli, type = c("II"), test.statistic = c("Chi"), component = "cond")
# χ     DF    p
# 161.95 10  < 2.2e-16
emmeans(modelSli,"Round")
#
#





x <- summarySE(Q1_ARA, measurevar = "Et_prod_umol_h_m2", groupvars = c("Species", "Round"))


#
#
#
#-------  »   Q2            « -------
# 2.	How do the different bryophyte functional groups differ their N2-fixation potential through the year of the Arctic?
#

#
#
#
#=======  ♫♫  Graphs        ♫♫ =======
#-------  ♪   Environmental ♪ -------


#
#
#
#-------  ♪   ARA           ♪ -------

field_ARA_wide.5 %>%
  mutate(MP = case_when(Round == 1 ~ "Sept20",
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
         Species = case_when(Species == "Au" ~ "Aulacomnium turgidum",
                             Species == "Di" ~ "Dicranum scoparium",
                             Species == "Hy" ~ "Hylocomium splendens",
                             Species == "Pl" ~ "Pleurozium schreberi",
                             Species == "Po" ~ "Polytrichum commune",
                             Species == "Pti" ~ "Ptilidium ciliare",
                             Species == "Ra" ~ "Racomitrium lanuginosum",
                             Species == "Sf" ~ "Sphagnum fuscum",
                             Species == "Sli" ~ "Sphagnum flexuosum",
                             Species == "S" ~ "S. ???",
                             TRUE ~ Species)) %>%
  mutate(across(MP, ~ factor(.x, levels=c("Sept20", "Oct20", "Nov20", "Feb21", "Mar21", "May21", "Jun21", "Jul21", "Sept21", "Oct21", "Nov21")))) %>%
  summarise(meanEt_pro = mean(Et_prod_umol_h_m2, na.rm = TRUE), se = sd(Et_prod_umol_h_m2)/sqrt(length(Et_prod_umol_h_m2)), .by = c(MP, Species)) %>%
  ggplot() +
  geom_errorbar(aes(x = MP, y = meanEt_pro, ymin=meanEt_pro, ymax=meanEt_pro+se), position=position_dodge(.9)) +
  geom_col(aes(x = MP, y = meanEt_pro), color = "black") + 
  facet_wrap(~Species, scales = "free", ncol = 2) +
  labs(x = "Measuring period (MP)", y = expression("Ethylene production (Ethylene  "*h^-1*" "*m^2*")"), title = expression("Moss ethylene production")) + 
  theme_classic(base_size = 15) +
  theme(panel.spacing = unit(2, "lines"),axis.text.x=element_text(angle=60, hjust=1))
  
  
#
#
#
#-------  ♪   N2 fixation   ♪ -------

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

#
#
#
#=======  ■  { The End }    ■ =======