# Analysis
# By Emil A.S. Andersen
# 
#=======  ♣   Libraries     ♣ =======
library(plotly)
library(tidyverse)
library(readxl)
library(lubridate)
library(car)
library(nlme)
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
# Chamber size
Ch_H <- 7 # Chamber height (cm) above moss surface. This needs to be changed to match each plot estimated height
Ch_r <- 5 # Chamber radius (cm)
Ch_vol_L <- (Ch_r^2*pi*Ch_H)/1000 # Chamber vol in L
Ch_area_m2 <- (Ch_r^2*pi)/10000 # Chamber area in m2
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

#
#
#
#=======  ♦   Main data     ♦ =======
#
#
#------- • Field data -------
#
# Remove chamber test
field_ARA.2 <- field_ARA %>%
  filter(Species != "B")
#
# Pivot wider and combine:
# Timestamp
field_ARA.Time <- field_ARA.2 %>% 
  select(!c(Time_nr, Date, Chamber_no, Chain_type, Temp_approx_C, Ethyl_conc_ppm, Acet_conc_prC)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = Timestamp)
#
# Ethylene
field_ARA.Ethyl <- field_ARA.2 %>% 
  select(!c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C, Acet_conc_prC)) %>%
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
  select(!c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C, Ethyl_conc_ppm)) %>%
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
# Join 
field_ARA_wide <- left_join(field_ARA.Time, field_ARA.Ethyl, by = join_by(Block, Species, Round)) %>%
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
  mutate(Et_prod_umol_h_m2 = Corr_Et_prod_pr_h.1 * (Ch_vol_L * p) / (R_const * 283) / Ch_area_m2) # Temperature set at constant 10°C !!!
#
# Set negative production to 0
field_ARA_wide.5 <- field_ARA_wide.4 %>%
  mutate(Et_prod_umol_h_m2 = if_else(Et_prod_umol_h_m2 < 0, 0, Et_prod_umol_h_m2))


#
#
#------- • Vial data -------

# ??
mutate(across(Timestamp, ~hm(.x)))# %>%
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
  select(1:3, Et_prod_umol_h_m2) %>%
  mutate(logEt_prod = log(Et_prod_umol_h_m2+1),
         sqrtEt_prod = sqrt((Et_prod_umol_h_m2/100)),
         arcEt_prod = asin(sqrt((Et_prod_umol_h_m2/1000))))
#
lme1 <- lme(arcEt_prod ~ Round*Species,
            random = ~1|Block/Species,
            data = Q1_ARA, na.action = na.exclude, method = "REML")
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