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
library(ggrepel)
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
# Environmental data averaged over the time of measurement for each day when measurements were done
field_environ <- left_join(EM50_Heath.2, AirT_wetland.1, by = join_by(Date, Tid)) %>%
  left_join(field_ARA.period, by = join_by(Date)) %>%
  filter(!is.na(Round)) %>%
  group_by(Date) %>%
  filter(Tid >= Start & Tid <= End) %>%
  summarise(Soil_moisture = mean(Soil_moisture),
            Soil_temperature = mean(Soil_temperature),
            PAR = mean(PAR),
            AirT_C = mean(AirT_C)) %>%
  ungroup()
#
# Reduce to an environmental value for each Species per block per round
# Combine with the big dataset first to connect date with block, species and round
field_environ.2 <- field_ARA.2 %>%
  left_join(field_environ, by = join_by(Date)) %>%
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
# Extract environmental data
# Done again (Notice capital F), as to maintain the order of variables with the order from ethylene calculations
field_environ.3 <- field_ARA_wide.5 %>%
  select(Block, Species, Round, AirT_C, Soil_temperature, Soil_moisture, PAR, Et_prod_umol_h_m2) %>%
  mutate(AirT = AirT_C + 273,
         Soil_temperature = Soil_temperature + 273) %>% # remove negative temperature by converting to kelvin
  select(-AirT_C) %>%
  mutate(Round = as.factor(Round)) %>%
  pivot_wider(values_from = Et_prod_umol_h_m2, names_from = Species)

# Export
# write_csv(field_environ.3, "export/N2fix_environ2.csv")
#
#
# Correlation plot of environmental data
corrplot::corrplot(cor(field_environ.3[6:3], method = "kendall"), type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
# All somewhat correlated
#
# NMDS
NMDS_environ <- metaMDS(field_environ.3[3:6], distance = "bray")#, scaling = 1, autotransform = TRUE) # Does a sqrt transformation and Wisconsin standardization
#
# Plot
# Standard with little fancyness
par (mfrow = c(1,2))
plot(NMDS_environ, type = "n")
points(NMDS_environ, display = "sites", cex = 0.8, pch=21, col="black", bg="white")
text(NMDS_environ, display = "spec", cex=0.7, col="blue")
stressplot(NMDS_environ)
par (mfrow = c(1,1))
#
# Species ethylene production fit
envfit.et <- envfit(NMDS_environ,
                 field_environ.3[7:16],
                 permutations = 9999, na.rm = TRUE)
#
# Extract points to create graph
# Main plot with NMDS scores
field_environ.plot <- field_environ.3
field_environ.plot$NMDS1 <- NMDS_environ$points[,1]
field_environ.plot$NMDS2 <- NMDS_environ$points[,2]
#
# For text
field_environ.scores.env <- as.data.frame(scores(NMDS_environ, "species"))
field_environ.scores.env$Factors <- rownames(field_environ.scores.env)
field_environ.scores.sp <- as.data.frame(scores(envfit.et$vectors$arrows))
field_environ.scores.sp$Species <- rownames(field_environ.scores.sp)
#
#

ggplot() +
  geom_text(data = field_environ.scores.env, aes(x = NMDS1, y = NMDS2, label = Factors)) +
  geom_point(data = field_environ.plot, aes(x = NMDS1, y = NMDS2, shape = Block, color = Round), size = 3) +
  #geom_text(data = field_environ.plot, aes(x = NMDS1, y = NMDS2, label = Round)) +
  geom_segment(data = field_environ.scores.sp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), color = "#8fa3a5") +
  geom_text_repel(data = field_environ.scores.sp, aes(x = NMDS1, y = NMDS2, label  = Species), size = 3, color = "#4D495A") + 
  #coord_equal() +
  theme_classic()

ggplot() +
  geom_text(data = field_environ.scores.env, aes(x = NMDS1, y = NMDS2, label = Factors)) +
  geom_point(data = field_environ.plot, aes(x = NMDS1, y = NMDS2, shape = Block, color = Round), size = 3) +
  #geom_text(data = field_environ.plot, aes(x = NMDS1, y = NMDS2, label = Round)) +
  geom_segment(data = field_environ.scores.sp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), arrow = arrow(length = unit(0.25, "cm")), color = "#8fa3a5") +
  geom_text_repel(data = field_environ.scores.sp, aes(x = NMDS1, y = NMDS2, label  = Species), size = 3, color = "#4D495A") + 
  #coord_equal() +
  theme_classic()

ordiplot(NMDS_environ)



###################


plot_21_ITS <- ggplot(data = subset(data$map_19, Treatment != "Gradient"))+
  geom_point(aes(x=PC1_ITS, y=PC2_ITS, col= Treatment, pch =Block), size = 6)+ 
  theme_classic() + 
  theme(legend.text = element_text(size=21), legend.title = element_text(size = 21))+ 
  scale_color_manual("Removal Treatment", values = colors_trt, labels = label_trt) + 
  stat_ellipse(aes(x=PC1_ITS, y=PC2_ITS, col = Treatment), level = 0.75, lty = 2) + 
  #annotate("text",x=c(0.4),y=c(-0.75),label="Treatment n.s.\n Block***",size=5)+  
  geom_segment(data = envfit_split$ITS_21_trt,
               aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "#8fa3a5") +
  geom_text_repel(data = envfit_split$ITS_21_trt, aes(x = Dim1, y = Dim2, 
                                                      label  = Var, size = 7, color = "#4D495A")+ 
                    xlab (label.x_21_ITS) + ylab(label.y_21_ITS))





ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2),size=3) + # add the point markers
  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()






x <- scores(NMDS_environ, "site")
x$sites[,1]

data.scores <- as.data.frame(scores(NMDS_environ, "site"))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
head(data.scores)  #look at the data
species.scores <- as.data.frame(scores(NMDS_environ, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2),size=3) + # add the point markers
  geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("A" = "red", "B" = "blue")) +
  coord_equal() +
  theme_bw()





plot(envfit.et, cex=1.2, axis=TRUE, bg = rgb(1, 1, 1, 0.5))





corrplot::corrplot(Field_environ.y[4:8], type = "upper", order = "hclust", tl.col = "black", tl.srt =  45)




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


rankindex(Field_environ.x)

NMDS_environ <- metaMDS(Field_environ.x, distance = "bray")#, scaling = 1, autotransform = TRUE)



NMDS_environ$points[,2]






# PCoA instead?

Field_environ.y <- Field_environ %>%
  left_join(Field_species, by = join_by(Block, Species, Round))

#write_csv(Field_environ.y, "export/N2fix_environ.csv")

# Only Aulocomium
Field_environ.Au <- Field_environ.y %>%
  mutate(Round = as.factor(Round)) %>%
  filter(Species == "Au")

Field_environ_dis.Au <- vegdist(Field_environ.Au[4:7]) # Bray-Curtis
PCOA_environ.Au <- cmdscale(Field_environ_dis.Au, add=T, eig=T)

plot(PCOA_environ.Au$points, xlab="PCoA 1", ylab="PCoA 2")

Field_environ.Au$PC1 <- PCOA_environ.Au$points[,1] # axis 1
Field_environ.Au$PC2 <- PCOA_environ.Au$points[,2] # axis 2

envfit_Au <- envfit(PCOA_environ.Au,
       Field_environ.Au[8],
       permutations = 99999, na.rm = TRUE)
env_Au <- data.frame(scores(envfit_Au, display = "vectors"))
env_Au <- cbind(env_Au,
               Var = rownames(env_Au),
               Pvalues = envfit_Au$vectors$pvals,
               R_squared = envfit_Au$vectors$r)
env_Au$comm <- "Au"


label.x_Au <- paste("PC1 (", plyr::round_any((PCOA_environ.Au$eig/sum(PCOA_environ.Au$eig))[1]*100, accuracy = 0.01), "%)", sep = "") # PC1 % variance explained
label.y_Au <- paste("PC2 (", plyr::round_any((PCOA_environ.Au$eig/sum(PCOA_environ.Au$eig))[2]*100, accuracy = 0.01), "%)", sep = "") # PC2 % variance explained

# Field_environ.yy <- Field_environ.Au.x %>%
#   mutate(Round = as.factor(Round)) %>%
#   filter(Species == "Au")



#plot_environ <- 
Field_environ.Au %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2, col = Round, pch = Block), size = 6) + 
  theme_classic() + 
  theme(legend.text = element_text(size = 21), legend.title = element_text(size = 21)) + 
  #scale_color_manual("Species", values = colors_trt, labels = label_trt) + 
  stat_ellipse(aes(x = PC1, y = PC2, col = Round), level = 0.75, lty = 2) +
#annotate("text",x=c(0.4),y=c(-0.75),label="Treatment n.s.\n Block***",size=5)+  
  geom_segment(data = env_Au,
               aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "#8fa3a5") +
  ggrepel::geom_text_repel(data = env_Au, aes(x = Dim1, y = Dim2, label  = "Ethylene"), size = 7, color = "#4D495A") + 
  xlab(label.x_Au) + 
  ylab(label.y_Au)


# PCA instead?
# Scale environmental factors for Au
Field_environ.Au.scaled <- scale(Field_environ.Au[4:7])
#PCA_environ.Au <- prcomp(Field_environ.Au.scaled) # Alternative scaling by: scale. = TRUE
PCA_environ.Au <- prcomp(Field_environ.Au[4:7], center = TRUE, scale. = TRUE)
PCA_environ.Au.2 <- princomp(Field_environ.Au.scaled) # Alternative not scaling, but using correlation matrix: cor = TRUE
PCA_environ.Au.3 <- rda(Field_environ.Au.scaled) # Is it really the same as a PCA, if nothing is changed within the function?

# Plot the different PCA versions
ordiplot(PCA_environ.Au)
ordiplot(PCA_environ.Au.2) # exactly the same as previous, but rotated differently
ordiplot(PCA_environ.Au.3) # Something else

summary(PCA_environ.Au.2, loadings = TRUE, cutoff = 0)

# Fit the ethylene production
envfit_Au.2 <- envfit(PCA_environ.Au,
                    Field_environ.Au[8],
                    permutations = 99999, na.rm = TRUE)
env_Au.2 <- data.frame(scores(envfit_Au.2, display = "vectors"))
env_Au.2 <- cbind(env_Au.2,
                Var = rownames(env_Au.2),
                Pvalues = envfit_Au.2$vectors$pvals,
                R_squared = envfit_Au.2$vectors$r)

# Extract PC scores
Field_environ.Au.2 <- Field_environ.Au
Field_environ.Au.2$PC1 <- PCA_environ.Au$x[,1]
Field_environ.Au.2$PC2 <- PCA_environ.Au$x[,2]


# PC scores for label
label.x_Au.2 <- paste("PC1 (", plyr::round_any((PCA_environ.Au$sdev/sum(PCA_environ.Au$sdev))[1]*100, accuracy = 0.01), "%)", sep = "") # PC1 % variance explained
label.y_Au.2 <- paste("PC2 (", plyr::round_any((PCA_environ.Au$sdev/sum(PCA_environ.Au$sdev))[2]*100, accuracy = 0.01), "%)", sep = "") # PC2 % variance explained


Field_environ.Au.2 %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2, col = Round, pch = Block)) +
  stat_ellipse(aes(x = PC1, y = PC2, col = Round), level = 0.75, lty = 2) +
  #annotate("text",x=c(0.4),y=c(-0.75),label="Treatment n.s.\n Block***",size=5)+  
  geom_segment(data = env_Au.2,
               aes(x = 0, xend = PC1, y = 0, yend = PC2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "#8fa3a5") +
  ggrepel::geom_text_repel(data = env_Au.2, aes(x = PC1, y = PC2, label  = "Ethylene"), size = 7, color = "#4D495A") +
  xlab(label.x_Au.2) +
  ylab(label.y_Au.2) +
  theme_classic() +
  theme(legend.text = element_text(size = 21), legend.title = element_text(size = 21))
  




# From Leah
## combine community and environmental data and calculate distances
otus_21_ITS <-   #this is the community data
map_21_ITS <-  # this is the environmental  data
all.equal(rownames(map_21_ITS),rownames(otus_21_ITS)) #check that row order is the same
dm_21_ITS<-vegdist(otus_21_ITS) #calculate distances with bray-curtis

#PCoA and envfit
pcoa_21_ITS<-cmdscale(dm_21_ITS,add=T,eig=T) #do PCoA
map_21_ITS$PC1_ITS<-pcoa_21_ITS$points[,1] #extract axis 1
map_21_ITS$PC2_ITS<-pcoa_21_ITS$points[,2] #extract axis 2
relevant_varaibles <- c("insert your variables for envfit here")
#run envfit
envfit_ITS_21 <- envfit(pcoa_21_ITS, 
                        select(map_21_ITS, all_of(relevant_variables_21)), 
                        permutations = 99999,na.rm = TRUE) 
#convert enfvit results to dataframe
env_21_ITS_a <- data.frame(scores(envfit_ITS_21, display = "vectors"))
env_21_ITS_a <- cbind(env_21_ITS_a, 
                      Var = rownames(env_21_ITS_a), 
                      Pvalues = envfit_ITS_21$vectors$pvals, 
                      R_squared = envfit_ITS_21$vectors$r)
env_21_ITS_a$comm <- "ITS_21_trt"


label.x_21_ITS<-paste("PC1 (",plyr::round_any((data$pcoa_21_ITS$eig/sum(data$pcoa_21_ITS$eig))[1]*100,accuracy=0.01),"%)",sep="") # PC1 % variance explained
label.y_21_ITS<-paste("PC2 (",plyr::round_any((data$pcoa_21_ITS$eig/sum(data$pcoa_21_ITS$eig))[2]*100,accuracy=0.01),"%)",sep="") # PC2 % variance explained

plot_21_ITS <- ggplot(data = subset(data$map_19, Treatment != "Gradient"))+
  geom_point(aes(x=PC1_ITS, y=PC2_ITS, col= Treatment, pch =Block), size = 6)+ 
  theme_classic() + 
  theme(legend.text = element_text(size=21), legend.title = element_text(size = 21))+ 
  scale_color_manual("Removal Treatment", values = colors_trt, labels = label_trt) + 
  stat_ellipse(aes(x=PC1_ITS, y=PC2_ITS, col = Treatment), level = 0.75, lty = 2) + 
  #annotate("text",x=c(0.4),y=c(-0.75),label="Treatment n.s.\n Block***",size=5)+  
  geom_segment(data = envfit_split$ITS_21_trt,
               aes(x = 0, xend = Dim1, y = 0, yend = Dim2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "#8fa3a5") +
  geom_text_repel(data = envfit_split$ITS_21_trt, aes(x = Dim1, y = Dim2, 
                                                      label  = Var, size = 7, color = "#4D495A")+ 
                    xlab (label.x_21_ITS) + ylab(label.y_21_ITS))




# Modified from
# https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_nmds
# Code https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_nmds_rscript
plot (NMDS_environ, main = 'NMDS', type = 'n')#, display = 'si')
points (NMDS_environ, col = field_environ.3$Round, pch = field_environ.3$Round) # , display = 'si'
legend ('bottomleft', pch = 1:11, col = 1:11, legend = 1:11, title = 'Round', cex = 0.8)
text(NMDS_environ, display = "spec", cex=0.7, col="blue")

text (NMDS_environ, col = "#FF000080", cex = 0.6, select = colSums (vltava.spe>0)>20) # , display = 'sp'

vltava.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-spe.txt', row.names = 1)
vltava.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/vltava-env.txt')




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


EM50_Heath %>%
  ggplot() +
  geom_point(aes(x = Date_time, y = Soil_moisture_B), colour = "#0072B2") +
  #geom_point(aes(x = Date_time, y = Soil_moisture_P), colour = "#CC79A7") +
  geom_point(aes(x = Date_time, y = Soil_moisture_R), colour = "#D55E00") +
  #geom_point(aes(x = Date_time, y = Soil_moisture_W), colour = "#009E73") +
  geom_point(aes(x = Date_time, y = Soil_moisture_Y), colour = "#F0E442")

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