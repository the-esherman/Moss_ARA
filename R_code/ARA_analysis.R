# Analysis
# By Emil A.S. Andersen
# 
#=======  ♣   Libraries     ♣ =======
library(tidyverse)
library(car)
library(nlme)
#
#
#
#=======  ♠   Load data     ♠ =======
# Import ID's
ID_info <- read_xlsx("Data_raw/Data_ID.xlsx")

#
field_ARA <- read_csv("Data_clean/field_ARA.csv", col_names = TRUE)
vial_ARA <- read_csv("Data_clean/vial_ARA.csv", col_names = TRUE)


#
#
#
#=======  ►   Functions     ◄ =======

#
#
#
#=======  ♦   Main data     ♦ =======

#
# Ethylene
field_ARA.Ethyl <- field_ARA %>% 
  select(!c(Sample_ID, Acet_conc_prC)) %>%
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
field_ARA.Acet <- field_ARA %>% 
  select(!c(Sample_ID, Ethyl_conc_ppm)) %>%
  complete(Block, Species, Time, Round) %>%
  pivot_wider(names_from = Time, values_from = Acet_conc_prC) %>%
  rename("Acet_conc_prC_T0" = T0,
         "Acet_conc_prC_T0x" = T0x,
         "Acet_conc_prC_T1" = T1,
         "Acet_conc_prC_T1x" = T1x,
         "Acet_conc_prC_T2" = T2,
         "Acet_conc_prC_T3" = T3,
         "Acet_conc_prC_T4" = T4)
#
field_ARA.2 <- left_join(field_ARA.Ethyl, field_ARA.Acet, by = join_by(Block, Species, Round))
#
field_ARA.3 <- left_join(field_ARA, ID_info, by = join_by())


#
#
#
#=======  §§  Statistics    §§ =======
#-------  »   Q1            « -------

#
#
#
#-------  »   Q2            « -------

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
#=======  ■  { The End }    ■ =======