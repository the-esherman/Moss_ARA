# Organize files
#
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
#=======  ♠   Load data     ♠ =======
# Import ID's
ID_info <- read_xlsx("Data_raw/Data_ID.xlsx")
ID_field <- read_xlsx("Data_raw/ID_base_field.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "text"))
ID_vial <- read_xlsx("Data_raw/ID_base_vial.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text", "numeric", "text", "text", "text"))
#
# Import ARA raw files
# Load each of 32 raw files
ARA_18a <- read_xlsx("Data_raw/EtylAcet/Anders M 18a (531 prøver) - Etylen & Acetylen.xlsx", sheet = 1, skip = 3, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_18b <- read_xlsx("Data_raw/EtylAcet/Anders M 18b (531 prøver) - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_18c <- read_xlsx("Data_raw/EtylAcet/Anders M 18c (531 prøver) - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_18d <- read_xlsx("Data_raw/EtylAcet/Anders M 18d (531 prøver) - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_18e <- read_xlsx("Data_raw/EtylAcet/Anders M 18e (531 prøver) - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_18f <- read_xlsx("Data_raw/EtylAcet/Anders M 18f (531 prøver) - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_19a <- read_xlsx("Data_raw/EtylAcet/Anders M 19a - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_19b <- read_xlsx("Data_raw/EtylAcet/Anders M 19b - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_19c <- read_xlsx("Data_raw/EtylAcet/Anders M 19c - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_19d <- read_xlsx("Data_raw/EtylAcet/Anders M 19d - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_20a <- read_xlsx("Data_raw/EtylAcet/Anders M 20a - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_20b <- read_xlsx("Data_raw/EtylAcet/Anders M 20b - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_20c <- read_xlsx("Data_raw/EtylAcet/Anders M 20c - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_20d <- read_xlsx("Data_raw/EtylAcet/Anders M 20d - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
#
# These are from run 6, but have mistakenly been labelled "5"!
ARA_21a <- read_xlsx("Data_raw/EtylAcet/Anders M 21a - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_21b <- read_xlsx("Data_raw/EtylAcet/Anders M 21b - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_22a <- read_xlsx("Data_raw/EtylAcet/Anders M 22a - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_22b <- read_xlsx("Data_raw/EtylAcet/Anders M 22b - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_22c <- read_xlsx("Data_raw/EtylAcet/Anders M 22c - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_23a <- read_xlsx("Data_raw/EtylAcet/Anders M 23a - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_23b <- read_xlsx("Data_raw/EtylAcet/Anders M 23b - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_23c <- read_xlsx("Data_raw/EtylAcet/Anders M 23c - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_23d <- read_xlsx("Data_raw/EtylAcet/Anders M 23d - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Ethyl_conc_ppm", "Acet_conc_prC"))
#
# ID_nr moved to different position
ARA_24a <- read_xlsx("Data_raw/EtylAcet/Anders M 24a - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_24b <- read_xlsx("Data_raw/EtylAcet/Anders M 24b - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_24c <- read_xlsx("Data_raw/EtylAcet/Anders M 24c - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:12), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr", "Ethyl_conc_ppm", "Acet_conc_prC"))
#
# Doubles the ID_nr
ARA_25a <- read_xlsx("Data_raw/EtylAcet/Anders M 25a - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:13), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr2", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_25b <- read_xlsx("Data_raw/EtylAcet/Anders M 25b - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:13), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr2", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_25c <- read_xlsx("Data_raw/EtylAcet/Anders M 25c - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:13), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr2", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_26a <- read_xlsx("Data_raw/EtylAcet/Anders M 26a - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:13), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr2", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_26b <- read_xlsx("Data_raw/EtylAcet/Anders M 26b - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:13), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr2", "Ethyl_conc_ppm", "Acet_conc_prC"))
ARA_26c <- read_xlsx("Data_raw/EtylAcet/Anders M 26c - Etylen & Acetylen.xlsx", sheet = 1, cell_cols(1:13), col_names = c("Comments", "Area_ethyl", "Area_acet", "Sample_order", "ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "ID_nr2", "Ethyl_conc_ppm", "Acet_conc_prC"))
#
#
#
#=======  🧹   Cleaning     🧹 =======
#
# Format base data with timestamps correctly
ID_field.2 <- ID_field %>%
  mutate(Time = case_when(Round == "9x" & Time == "T1" ~ "T1x",
                          Time_nr == "T0x_2" ~ "T0x",
                          TRUE ~ Time)) %>%
  mutate(Round = if_else(Time == "T1x" & Round == "9x", "9", Round)) %>%
  mutate(across(Date, ~ymd(.x))) %>%
  select(!c(Comments, Sample_ID))
#
ID_vial.2 <- ID_vial %>%
  mutate(across(Date, ~ymd(.x))) %>%
  select(!c(Comments, Sample_ID))
#
#
# Combine all raw datasheets
comb_ARA <- bind_rows(list(ARA_18a, ARA_18b, ARA_18c, ARA_18d, ARA_18e, ARA_18f, ARA_19a, ARA_19b, ARA_19c, ARA_19d, ARA_20a, ARA_20b, ARA_20c, ARA_20d, ARA_21a, ARA_21b, ARA_22a, ARA_22b, ARA_22c, ARA_23a, ARA_23b, ARA_23c, ARA_23d, ARA_24a, ARA_24b, ARA_24c, ARA_25a, ARA_25b, ARA_25c, ARA_26a, ARA_26b, ARA_26c), .id = "Raw") %>%
  select(!c(Sample_order, ID_nr2)) # Ignore ID_nr2, as this is a duplicate from 25a-c
#
# Remove the original header, and rows of standards and blanks 
comb_ARA.2 <- comb_ARA %>%
  filter(!is.na(Sample_ID)) %>%
  filter(Sample_ID != "Sample_ID" & Sample_ID != "Std 1" & Sample_ID != "Std 2" & Sample_ID != "Std 3" & Sample_ID != "Std 4" & Sample_ID != "Std 5" & Sample_ID != "Std 6" & Sample_ID != "Std 7" & Sample_ID != "Blank") %>%
  mutate(across(c(Area_ethyl, Area_acet, Ethyl_conc_ppm, Acet_conc_prC), ~as.numeric(.x))) %>%
  mutate(Raw = case_when(Raw == "1" ~"18a", 
                         Raw == "2" ~"18b", 
                         Raw == "3" ~"18c", 
                         Raw == "4" ~"18d", 
                         Raw == "5" ~"18e", 
                         Raw == "6" ~"18f", 
                         Raw == "7" ~"19a", 
                         Raw == "8" ~"19b", 
                         Raw == "9" ~"19c", 
                         Raw == "10" ~"19d", 
                         Raw == "11" ~"20a", 
                         Raw == "12" ~"20b", 
                         Raw == "13" ~"20c", 
                         Raw == "14" ~"20d", 
                         Raw == "15" ~"21a", # Change Round from 5 to 6
                         Raw == "16" ~"21b", # Change Round from 5 to 6
                         Raw == "17" ~"22a", 
                         Raw == "18" ~"22b", 
                         Raw == "19" ~"22c", 
                         Raw == "20" ~"23a", 
                         Raw == "21" ~"23b", 
                         Raw == "22" ~"23c", 
                         Raw == "23" ~"23d", 
                         Raw == "24" ~"24a", 
                         Raw == "25" ~"24b", 
                         Raw == "26" ~"24c", 
                         Raw == "27" ~"25a", 
                         Raw == "28" ~"25b", 
                         Raw == "29" ~"25c", 
                         Raw == "30" ~"26a", 
                         Raw == "31" ~"26b", 
                         Raw == "32" ~"26c"))
#
comb_ARA.3 <- comb_ARA.2 %>%
  separate_wider_delim(Time_nr, delim = "_", names = c("Time", "Round"), too_few = "debug", too_many = "debug") %>%
  mutate(Round = case_when(is.na(Block) & is.na(Species) & is.na(Time) & is.na(Round) ~ "2x",
                        is.na(Round) & !is.na(Time) ~ "11",
                        Raw == "21a" | Raw == "21b" ~ "6",
                        TRUE ~Round),
         Block = if_else(is.na(Block), "B", Block),
         Species = if_else(is.na(Species), "Di", Species),
         Time = if_else(is.na(Time), "T1", Time)) %>%
  mutate(Time = case_when(Round == "9x" & Time == "T1" ~ "T1x",
                          Round == "2x" & Time == "T1" ~ "T1x",
                          TRUE ~ Time)) %>%
  mutate(Round = case_when(Round == "9x" ~ "9",
                        Round == "2x" ~ "9", # Was on the samples from round 9, which fits with noted data
                        TRUE ~ Round))
#
# Extract vial samples
vials_ARA <- comb_ARA.3 %>%
  filter(Time == "T24" | Species == "v1" | Species == "v2" | Species == "v1cc" | Species == "v2cc") %>%
  select(7:11, Ethyl_conc_ppm, Acet_conc_prC) %>%
  left_join(ID_vial.2, by = join_by(Block, Species, Time, Round, Time_nr)) %>% # Time_nr is identical here
  relocate(c(Date, Timestamp, Time_from_T0, Temp_approx_C), .after = Time_nr)
#
# Extract field samples
field_ARA <- comb_ARA.3 %>%
  filter(Time != "T24") %>%
  filter(Species != "v1" & Species != "v2" & Species != "v1cc" & Species != "v2cc") %>% # Presumably vials
  select(7:11, Ethyl_conc_ppm, Acet_conc_prC) %>%
  select(!Time_nr) %>% # The raw data has mistakes in the Time_nr, that have been corrected for in Block and Time columns
  left_join(ID_field.2, by = join_by(Block, Species, Time, Round)) %>%
  relocate(c(Time_nr, Date, Timestamp, Chamber_no, Chain_type, Temp_approx_C), .after = Round)
#
# Simplest solution for changing 5 Acetylene values that have increased from T0 or were negative at T0
# Acetylene concentrations are replaced with average for the same round and species at T0, if T0 is lower than T1
field_ARA <- field_ARA %>%
  mutate(Acet_conc_prC = case_when(Block == "B" & Species == "Di" & Round == "11" & Time == "T0" ~ mean(Acet_conc_prC[which(
                                     Block != "B" & Species == "Di" & Round == "11" & Time == "T0")]),
                                   Block == "Y" & Species == "Po" & Round == "11" & Time == "T0" ~ mean(Acet_conc_prC[which(
                                     Block != "Y" & Species == "Po" & Round == "11" & Time == "T0")]),
                                   Block == "R" & Species == "Hy" & Round == "6" & Time == "T0" ~ mean(Acet_conc_prC[which(
                                     Block != "R" & Species == "Hy" & Round == "6" & Time == "T0")]),
                                   Block == "W" & Species == "Pti" & Round == "5" & Time == "T0" ~ mean(Acet_conc_prC[which(
                                     Block != "W" & Species == "Pti" & Round == "5" & Time == "T0")]),
                                   Block == "Y" & Species == "Pti" & Round == "5" & Time == "T0" ~ mean(Acet_conc_prC[which(
                                     Block != "Y" & Species == "Pti" & Round == "5" & Time == "T0")]),
                                   TRUE ~ Acet_conc_prC))
#
#
#------- • Export clean data -------
# CAREFUL!
1
#
#
# Save Field and vial data separately
write_csv(vials_ARA, "Data_clean/vial_ARA.csv", na = "NA")
write_csv(field_ARA, "Data_clean/field_ARA.csv", na = "NA")




filter(field_ARA, Block == "Y" & Species == "Pti" & Round == "5" & Time == "T0")
















# #### Remove? or move? ####




# Cleaning data a bit
# 
clean_ARA_full <- ARA_full2 %>%
  # Correct all day stamps to match when they were actually taken
  mutate(Date = case_when(Round == 11 & (Block == "B" | Block == "Y" | Block == "W") ~ 20211108,
                          Round == 11 & (Block == "R" | Block == "P") ~ 20211109,
                          Round == 10 & (Block == "R" | Block == "P" | Block == "W" | Block == "Y") ~ 20210929,
                          Round == 10 & Block == "B" ~ 20210930,
                          Round == 9 & Block == "B" ~ 20210829,
                          Round == 9 & (Block == "W" | Block == "P" | Block == "R" | Block == "Y") ~ 20210830,
                          Round == 8 & (Block == "W" | Block == "P") ~ 20210705,
                          Round == 8 & (Block == "R" | Block == "Y" | Block == "B") ~ 20210706,
                          Round == 7 & (Block == "Y" | Block == "R") ~ 20210604,
                          Round == 7 & (Block == "B" | Block == "P" | Block == "W") ~ 20210605,
                          Round == 6 & (Block == "B" | Block == "Y" | Block == "W") ~ 20210507,
                          Round == 6 & (Block == "R" | Block == "P") ~ 20210510,
                          Round == 5 & (Block == "B" | Block == "Y" | Block == "W") ~ 20210329,
                          Round == 5 & (Block == "R" | Block == "P") ~ 20210330,
                          Round == 4 & (Block == "P" | Block == "R") ~ 20210202,
                          Round == 4 & (Block == "Y" | Block == "B" | Block == "W") ~ 20210201,
                          Round == 3 & (Block == "Y") ~ 20201111,
                          Round == 3 & (Block == "R") ~ 20201113,
                          Round == 3 & (Block == "B") ~ 20201114,
                          Round == 3 & (Block == "P") ~ 20201115,
                          Round == 3 & (Block == "W") ~ 20201116,
                          Round == 2 & (Block == "R") ~ 20201001,
                          Round == 2 & (Block == "W") ~ 20201002,
                          Round == 2 & (Block == "Y" | Block == "B" | Block == "P") ~ 20201003,
                          Round == 1 & (Block == "W") ~ 20200910,
                          Round == 1 & (Block == "P" | Block == "R" | Block == "Y") ~ 20200911,
                          Round == 1 & (Block == "B") ~ 20200912)
  ) %>%
  mutate(across(c("Acetylene conc (%)",	"Ethylene conc (ppm)"), as.numeric)) %>%
  # Remove Tray ID column
  select(-Bakke_ID) %>%
  # Remove rows with extra sampling, where rows are NA
  filter(!is.na(Round)) %>%
  # Remove samples without a date
  filter(!is.na(Date)) %>%
  # Remove duplicate T0 at row
  slice(-316) # P_Sli_T0 number two at time 14:33:00
  #filter(-(Sample_ID == "P_Sli_2" & Time == "T0" & Timestamp == "14:33:00")) # Does not work
  #distinct(Sample_ID,Time) # Only keeps those two columns
#
#
# Have columns for each time, and add minutes passed
#
ARA_full_wide <- clean_ARA_full %>%
  filter((Time == "T0" | Time == "T1" | Time == "T2")) %>%
  #mutate(across(c("Timestamp"), hms::as_hms)) %>%
  pivot_wider(names_from = c("Time"), values_from = c("Timestamp",	"Acetylene conc (%)",	"Ethylene conc (ppm)")) %>%
  mutate("T0" = period_to_seconds(hms(Timestamp_T0)), .after = Timestamp_T0) %>%
  mutate("T1" = period_to_seconds(hms(Timestamp_T1)), .after = Timestamp_T1) %>%
  mutate("T2" = period_to_seconds(hms(Timestamp_T2)), .after = Timestamp_T2) %>%
  mutate("Time_0-1_min" = (T1-T0)/60, .before = "Acetylene conc (%)_T0") %>%
  mutate("Time_0-2_min" = (T2-T0)/60, .before = "Acetylene conc (%)_T0") %>%
  mutate("Time_1-2_min" = (T2-T1)/60, .before = "Acetylene conc (%)_T0") %>%
  select(-c(T0,T1,T2))
#
#
# Gas constant
Gas_const = 8.31446261815324 # L kPa K^-1 umol^-1
# 
# Calculate ethylene production
ARA_full_pro <- ARA_full_wide %>%
  #
  # Area and vol of chamber
  mutate("Ch_area_m2" = (`Chamber radius (cm)`^2 * pi)/10000, .before = Date) %>%
  mutate("Ch_vol_L" = (`Chamber radius (cm)`^2 * pi * `Chamber height (cm)`)/1000, .before = Date) %>%
  #
  # 60 min time and Acetylene and Ethylene
  mutate("T_60min" = if_else(ARA_full_wide$`Time_0-1_min` >= 55 & ARA_full_wide$`Time_0-1_min` <= 65, ARA_full_wide$`Time_0-1_min`, ARA_full_wide$`Time_0-2_min`), .after = "Time_1-2_min") %>%
  mutate("Ace_%_60min" = if_else(ARA_full_wide$`Time_0-1_min` >= 55 & ARA_full_wide$`Time_0-1_min` <= 65, ARA_full_wide$`Acetylene conc (%)_T1`, ARA_full_wide$`Acetylene conc (%)_T2`)) %>%
  mutate("Eth_ppm_60min" = if_else(ARA_full_wide$`Time_0-1_min` >= 55 & ARA_full_wide$`Time_0-1_min` <= 65, ARA_full_wide$`Ethylene conc (ppm)_T1`, ARA_full_wide$`Ethylene conc (ppm)_T2`)) %>%
  #
  # Calculate Ethylene concentration per hour from T1
  mutate("ppm_Et_h-1" = (`Ethylene conc (ppm)_T1` - `Ethylene conc (ppm)_T0`)/(`Time_0-1_min`/60)) %>%
  #
  # Calculate lost Acetylene from T0 to T1
  mutate("Lost_Ac_h-1" = ((`Acetylene conc (%)_T1`*10000 - `Acetylene conc (%)_T0`*10000) / (`Time_0-1_min`/60))) %>%
  #
  # Correct Ethylene production based on Acetylene loss
  mutate("Corr_Et_h-1" = (`ppm_Et_h-1` - (`Lost_Ac_h-1`*`Ethylene conc (ppm)_T0`/(`Acetylene conc (%)_T0`*10000)))) %>%
  #
  # Ethylene production
  mutate("umol_Et_pro" = `ppm_Et_h-1` * (Ch_vol_L * 101.3) / (Gas_const*273)/Ch_area_m2) %>%
  # Formula:    Et h^-1 = n*(V*p) / (R*T)/A
  # Units: µmol Et h^-1 = ppm Et h^-1 *(L * kPa) / (8.314 * K) / m^2
  #
  # Corrected Ethylene production
  mutate("Corr_umol_Et_pro" = `Corr_Et_h-1` * (Ch_vol_L * 101.3) / (Gas_const*273)/Ch_area_m2)
#
# Set negative values
ARA_full_pro <- ARA_full_pro %>%
  # Define Ethylene production as definite to work with
  #
  # Ethylene production with negative values from corrected Ethylene production
  mutate("Et_pro" = Corr_umol_Et_pro) %>%
  #
  # Replace negative values with 0
  mutate("Et_0_pro" = if_else(ARA_full_pro$Corr_umol_Et_pro >= 0, ARA_full_pro$Corr_umol_Et_pro, 0)) %>%
  #
  # Remove negative values (replace with 0 then remove 0's)
  mutate("Et_pos_pro" = Et_0_pro) %>%
  mutate("Et_pos_pro" = replace(Et_pos_pro, which(Et_pos_pro == 0), NA))
#
ggplot(ARA_full_pro, aes(Round, Et_0_pro)) + stat_summary(fun = mean, geom="col") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + labs(x = "Time", y = "Ethylene production per hour (umol per h per m^2)", colour = "Species", title = "Ethylene production") + facet_wrap(~Species) + coord_cartesian(ylim=c(0,5)) + guides(x = guide_axis(n.dodge = 2)) + theme_light()


# Works, but showed that duplicate problem is with Tray ID which is not same for same sample series all the time
ARA_full_wide <- ARA_full2 %>%
  filter((Time == "T0" | Time == "T1" | Time == "T2")) %>%
  select(-Bakke_ID) %>%
  mutate(across(c("Timestamp"), hms::as_hms)) %>%
  group_by(Time) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = c("Time"), values_from = c("Timestamp",	"Acetylene conc (%)",	"Ethylene conc (ppm)")) %>%
  select(-row)

write_csv2(ARA_full_wide, "Data_raw/moss_ARA_all_check.csv")
  
#

# #### To be removed ####

# Import ethylene/acetylen data from many different excel files and combine
# Import
ARA1 <- read_xlsx("Data_raw/EtylAcet/Anders M 18a (531 prøver) - Etylen & Acetylen.xlsx", sheet = 1,)#, skip = 3)#, cols_only(2:11), col_names = c("Area Acetylen", "Sample order","ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Etylen conc (ppm)", "Acetylen conc (%)"))


ARA1 <- read_xlsx("Data_raw/EtylAcet/Anders M 18a (531 prøver) - Etylen & Acetylen.xlsx", sheet = 1, skip = 3, cols_only(1:11), col_names = c("Comments","Area Ethylen", "Area Acetylen", "Sample order","ID_nr", "Sample_ID", "Block", "Species", "Time_nr", "Tray_ID", "Etylen conc (ppm)", "Acetylen conc (%)"))

ARA_full <- read_xlsx("Data_raw/moss_ARA_all.xlsx", col_types = c("text","text","text","text","text","text","numeric","numeric","text","numeric", "numeric", "numeric"))
ARA_full2 <- read_csv2("Data_raw/moss_ARA_all.csv")#, col_types = c("text","text","text","text","text","text","numeric","numeric","text","text", "numeric", "numeric"))



#
# Alternative combination
comb_ARA <- ARA_18a %>%
  bind_rows(ARA_18c) %>%
  bind_rows(ARA_18d) %>%
  bind_rows(ARA_18e) %>%
  bind_rows(ARA_18f) %>%
  bind_rows(ARA_19a) %>%
  bind_rows(ARA_19b) %>%
  bind_rows(ARA_19c) %>%
  bind_rows(ARA_19d) %>%
  bind_rows(ARA_20a) %>%
  bind_rows(ARA_20b) %>%
  bind_rows(ARA_20c) %>%
  bind_rows(ARA_20d) %>%
  bind_rows(ARA_21a) %>%
  bind_rows(ARA_21b) %>%
  bind_rows(ARA_22a) %>%
  bind_rows(ARA_22b) %>%
  bind_rows(ARA_22c) %>%
  bind_rows(ARA_23a) %>%
  bind_rows(ARA_23b) %>%
  bind_rows(ARA_23c) %>%
  bind_rows(ARA_23d) %>%
  bind_rows(ARA_24a) %>%
  bind_rows(ARA_24b) %>%
  bind_rows(ARA_24c) %>%
  bind_rows(ARA_25a) %>%
  bind_rows(ARA_25b) %>%
  bind_rows(ARA_25c) %>%
  bind_rows(ARA_26a) %>%
  bind_rows(ARA_26b) %>%
  bind_rows(ARA_26c) %>%
  select(!c(Sample_order, ID_nr2)) # Ignore ID_nr2, as this is a duplicate from 25a-c




# Correct ID info
ID_info <- ID_info %>%
  mutate(Time = case_when(Round == "9x" & Time == "T1" ~ "T1x",
                          Time_nr == "T0x_2" ~ "T0x",
                          TRUE ~ Time)) %>%
  mutate(Round = if_else(Time == "T1x" & Round == "9x", "9", Round))