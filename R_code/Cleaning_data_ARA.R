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
# Notice that the "character" timestamp only works if they are "text" fields in excel. Be prepared for some fun!
ID_info <- read_xlsx("Data_raw/Data_ID.xlsx")
ID_field <- read_xlsx("Data_raw/ID_base_field.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric", "text", "text"))
ID_vial <- read_xlsx("Data_raw/ID_base_vial.xlsx", col_types = c("text", "text", "text", "text", "text", "text", "text", "numeric", "text", "text", "text"))
ID_15N <- read_xlsx("Data_raw/ID_15N.xlsx", col_types = c("text", "text", "text", "text", "numeric", "text", "text", "text"))
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
# remove loaded files
rm(ARA_18a, ARA_18b, ARA_18c, ARA_18d, ARA_18e, ARA_18f, ARA_19a, ARA_19b, ARA_19c, ARA_19d, ARA_20a, ARA_20b, ARA_20c, ARA_20d, ARA_21a, ARA_21b, ARA_22a, ARA_22b, ARA_22c, ARA_23a, ARA_23b, ARA_23c, ARA_23d, ARA_24a, ARA_24b, ARA_24c, ARA_25a, ARA_25b, ARA_25c, ARA_26a, ARA_26b, ARA_26c)
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






# Check if there are duplicates or wrong dates
# With Species
# Should be 550
field_ARA_unique.1 <- field_ARA %>%
  filter(!grepl("Ch", Block)) %>% # Remove chamber control measurements
  distinct(Block, Species, Round, Date)
# Without species
# Should be 55
field_ARA_unique.2 <- field_ARA %>%
  filter(!grepl("Ch", Block)) %>% # Remove chamber control measurements
  distinct(Block, Round, Date)

#=======  ■  { The End }    ■ =======