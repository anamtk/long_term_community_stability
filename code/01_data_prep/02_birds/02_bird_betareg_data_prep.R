# Covariate prep for SAM for Konza bird dataset
# October 11, 2023

#this script preps covariates for the SAM model for Konza birds

# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse",
                  'data.table', 'ggcorrplot')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#we'll be using some summary of the weather station precip and temperatures
#(maybe monthly??) 
#and also plant biomass data from the plots - this is yearly I think, but there
#are lots of quadrats - so we can get an average
#daily climate
climate <- read.csv(here("data_raw",
                         "02_birds",
                         'AWE012.csv'))

#yearly npp for a bunch of quadrats in each watershed
npp <- read.csv(here("data_raw",
                     "02_birds",
                     'PAB011.csv'))


#to get the sites and transects we need for the other 
  #two datasets
data <- readRDS(here("model_summaries",
                     "02_birds",
                     "bird_MSAM_results_bray_meanSD.RDS"))

IDs <- read.csv(here('data_output',
                     "02_birds",
                     "01_MSAM",
                     "other_data",
                     "site_year_IDs.csv"))

konza_obs <- readRDS(here('data_output',
                          "02_birds",
                          "01_MSAM",
                          "other_data",
                          'konza_observed_bray.RDS'))

# Summarize monthly climate values ----------------------------------------

#average mean temp for the month
#average precipitation for the month

temp <- climate %>%
  dplyr::select(RECYEAR, RECMONTH, TAVE) %>%
  mutate(TAVE = as.numeric(TAVE)) %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarise(TAVE = mean(TAVE, na.rm = T)) %>%
  ungroup()

ppt <- climate %>%
  dplyr::select(RECYEAR, RECMONTH, DPPT) %>%
  mutate(DPPT = case_when(DPPT == "." ~"0",
                          TRUE ~ DPPT)) %>%
  mutate(DPPT = as.numeric(DPPT)) %>%
  group_by(RECYEAR, RECMONTH) %>%
  summarise(PPT = mean(DPPT, na.rm = T)) %>%
  ungroup()

ggplot(temp, aes(x = RECMONTH, y = TAVE)) +
  geom_point(position = position_jitter(width = 0.25))

ggplot(ppt, aes(x = RECMONTH, y = PPT)) +
  geom_point(position = position_jitter(width = 0.25))

#standardize:
#whatever months have + are one season, whatever months
#have - values are the other season
#1,2,3
#11, 12
temp %>%
  mutate(TAVE = scale(TAVE)) %>%
  group_by(RECMONTH) %>%
  summarise(mean = mean(TAVE))

#1, 2, 3
#10, 11, 12
ppt %>%
  mutate(PPT = scale(PPT)) %>%
  group_by(RECMONTH) %>%
  summarise(mean = mean(PPT))

#going to summarise both by the same months - using the PPT values
#since these split things nicely 6-6, and month 10 is *barely* above
#0 in the temp dataset

temp2 <- climate %>%
  dplyr::select(RECYEAR, RECMONTH, TAVE) %>%
  mutate(TAVE = as.numeric(TAVE)) %>%
  mutate(season = case_when(RECMONTH %in% c(1, 2, 3, 10, 11, 12) ~ "winter",
                            RECMONTH %in% c(4, 5, 6, 7, 8, 9) ~ "summer",
                            TRUE ~ NA_character_)) %>%
  group_by(RECYEAR, season) %>%
  summarise(TAVE = mean(TAVE, na.rm = T)) %>%
  ungroup()

ppt2 <- climate %>%
  dplyr::select(RECYEAR, RECMONTH, DPPT) %>%
  mutate(DPPT = as.numeric(DPPT)) %>%
  mutate(season = case_when(RECMONTH %in% c(1, 2, 3, 10, 11, 12) ~ "winter",
                            RECMONTH %in% c(4, 5, 6, 7, 8, 9) ~ "summer",
                            TRUE ~ NA_character_)) %>%
  group_by(RECYEAR, season) %>%
  summarise(PPT = mean(DPPT, na.rm = T)) %>%
  ungroup()


# Get seasonal values into lag format for model ---------------------------

#since sampling usually occured in June and January, but sometimes
#in February, April, October, and December,
#going to consider lags to be "end of year and back",
#so starting in "winter" of each year and going back in time
#get temp lags:
temp_lags <- temp2 %>%
  arrange(RECYEAR, season) %>%
  #this creates a column for every lag this season (winter) and 5 seasons ago
  do(data.frame(., setNames(shift(.$TAVE, 1:5), c("TAVE_l1", 'TAVE_l2', "TAVE_l3",
                                                   "TAVE_l4", "TAVE_l5")))) %>%
  ungroup() %>%
  filter(season == "winter") %>%
  dplyr::select(-season)

#get ppt lags:
ppt_lags <- ppt2 %>%
  arrange(RECYEAR, season) %>%
  #this creates a column for every lag this season (winter) and 5 seasons ago
  do(data.frame(., setNames(shift(.$PPT, 1:11), c("PPT_l1", 'PPT_l2', "PPT_l3",
                                                   "PPT_l4", "PPT_l5", "PPT_l6",
                                                  "PPT_l7", "PPT_l8", "PPT_l9",
                                                  "PPT_l10", "PPT_l11")))) %>%
  ungroup() %>%
  filter(season == "winter") %>%
  dplyr::select(-season)



# Summarise NPP data ------------------------------------------------------

npp2 <- npp %>%
  rowwise() %>%
  mutate(totallive = sum(LVGRASS, FORBS, na.rm = T)) %>%
  group_by(RECYEAR, WATERSHED) %>%
  summarise(NPP = mean(totallive, na.rm = T)) %>%
  ungroup()


# Set up NPP to be lags ---------------------------------------------------

#get npp lags:
npp_lags <- npp2 %>%
  group_by(WATERSHED) %>%
  arrange(RECYEAR) %>%
  #this creates a column for every lag this year to 5 years ago
  do(data.frame(., setNames(shift(.$NPP, 1:5), c("NPP_l1", 'NPP_l2', "NPP_l3",
                                                  "NPP_l4", "NPP_l5")))) %>%
  ungroup() %>%
  mutate(WATERSHED = case_when(WATERSHED == "001d" ~ "001D",
                               WATERSHED == "004b" ~ "004B",
                               WATERSHED == "020b" ~ "020B",
                               TRUE ~ NA_character_))


# Prep stability data -----------------------------------------------------

stability2 <- as.data.frame(data) %>%
  rownames_to_column(var = "var") %>%
  filter(var != "deviance") %>%
  separate(var,
           into = c('TransID', 'yrID'),
           sep = ",") %>%
  mutate(TransID = str_sub(TransID, 6, nchar(TransID))) %>%
  mutate(yrID = str_sub(yrID, 1, (nchar(yrID)-1))) %>%
  rename("bray" = "Mean") %>%
  dplyr::select(yrID, TransID, bray, SD) %>%
  mutate(yrID = as.numeric(yrID),
         TransID = as.numeric(TransID)) %>%
  left_join(IDs, by = c("TransID", "yrID")) %>%
  dplyr::select(-X)

# Get site and transect IDs -----------------------------------------------

sites <- stability2 %>%
  distinct(TransID, TRANSNUM) 

site <- sites$SITE
sitetrans <- sites$SITE_TRANS

# Combine all data --------------------------------------------------------

all_data <- stability2 %>%
  left_join(npp_lags, by = c("RECYEAR",
                             "WATERSHED")) %>%
  left_join(temp_lags, by = c("RECYEAR")) %>%
  left_join(ppt_lags, by = c("RECYEAR")) 


# Prep observed data to bind ----------------------------------------------

konza_obs2 <- konza_obs %>%
  pivot_wider(names_from = "type",
              values_from = "bray")

all_data2 <- all_data %>%
  left_join(konza_obs2, by = c("yrID", "TransID"))

# Check for correlation in predictors -------------------------------------

corr_check <- all_data %>%
  dplyr::select(NPP:PPT_l5) %>%
  filter(if_all(everything(), ~!is.na(.))) 
  
corr_check2 <- cor(corr_check)
ggcorrplot(cor(corr_check), type = "lower", lab = T)

# Export ------------------------------------------------------------------

write.csv(all_data2, here('data_output',
                          '02_birds',
                          '02_betareg',
                          'other_data',
                          'bird_betareg_tidydata.csv'))


# Prep data for jags ------------------------------------------------------


# Prep modeled data for jags ----------------------------------------------

# Remove years before 1982 ------------------------------------------------
all_data <- all_data %>%
  filter(RECYEAR >1981)
# Prep data for jags ------------------------------------------------------


# Loop indexing -----------------------------------------------------------

n.data <- nrow(all_data)

n.templag <- all_data %>%
  dplyr::select(TAVE:TAVE_l5) %>%
  ncol()

n.npplag <- all_data %>%
  dplyr::select(NPP:NPP_l5) %>%
  ncol()

n.transects <- length(unique(all_data$TransID))


# Random effects ----------------------------------------------------------

Transect.ID <- all_data %>%
  dplyr::select(TransID) %>%
  as_vector()

# Response data -----------------------------------------------------------

bray <- as.vector(all_data$bray)
var.estimate <- as.vector(all_data$SD^2)

# Covariates --------------------------------------------------------------

Temp <- all_data %>%
  dplyr::select(yrID, TransID, TAVE:TAVE_l5) %>%
  pivot_longer(TAVE:TAVE_l5,
               names_to = 'lag',
               values_to = 'temp') %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "temp") %>%
  dplyr::select(TAVE:TAVE_l5) %>%
  as.matrix()

sum(is.na(Temp))/(sum(is.na(Temp)) + sum(!is.na(Temp)))

PPT <- all_data %>%
  dplyr::select(yrID, TransID, PPT:PPT_l5) %>%
  pivot_longer(PPT:PPT_l5,
               names_to = 'lag',
               values_to = 'ppt') %>%
  mutate(ppt = scale(ppt)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "ppt") %>%
  dplyr::select(PPT:PPT_l5) %>%
  as.matrix()

sum(is.na(PPT))/(sum(is.na(PPT)) + sum(!is.na(PPT)))

# 0% missing data for PPT and TEmp

NPP <- all_data %>%
  dplyr::select(yrID, TransID, NPP:NPP_l5) %>%
  pivot_longer(NPP:NPP_l5,
               names_to = 'lag',
               values_to = 'NPP') %>%
  mutate(NPP = scale(NPP)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "NPP") %>%
  dplyr::select(NPP:NPP_l5) %>%
  as.matrix()

sum(is.na(NPP))/(sum(is.na(NPP)) + sum(!is.na(NPP)))

#with subset data, 10% missing

# Make data list ----------------------------------------------------------

data <- list(n.data = n.data,
             n.templag = n.templag,
             n.npplag = n.npplag,
             n.transects = n.transects,
             bray = bray,
             var.estimate = var.estimate,
             Transect.ID = Transect.ID,
             Temp = Temp,
             PPT = PPT,
             NPP = NPP)

saveRDS(data, here("data_output",
                   '02_birdss',
                   '02_betareg',
                   'betareg_inputs',
                   "model_inputs",
                   "bird_betareg_input_data_list_impdetect.RDS"))



# Prep empirical data for jags --------------------------------------------

# Remove years before 1982 ------------------------------------------------
all_data <- all_data %>%
  filter(RECYEAR >1981)

all_data <- all_data %>%
  filter(!is.na(observed_all))

# Prep data for jags ------------------------------------------------------


# Loop indexing -----------------------------------------------------------

n.data <- nrow(all_data)

n.templag <- all_data %>%
  dplyr::select(TAVE:TAVE_l5) %>%
  ncol()

n.npplag <- all_data %>%
  dplyr::select(NPP:NPP_l5) %>%
  ncol()

n.transects <- length(unique(all_data$TransID))


# Random effects ----------------------------------------------------------

Transect.ID <- all_data %>%
  dplyr::select(TransID) %>%
  as_vector()

# Response data -----------------------------------------------------------

#transform like: https://stats.stackexchange.com/questions/31300/dealing-with-0-1-values-in-a-beta-regression
bray <- as.vector((all_data$observed_all*(n.data-1) + 0.5)/n.data)


# Covariates --------------------------------------------------------------

Temp <- all_data %>%
  dplyr::select(yrID, TransID, TAVE:TAVE_l5) %>%
  pivot_longer(TAVE:TAVE_l5,
               names_to = 'lag',
               values_to = 'temp') %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "temp") %>%
  dplyr::select(TAVE:TAVE_l5) %>%
  as.matrix()

sum(is.na(Temp))/(sum(is.na(Temp)) + sum(!is.na(Temp)))

PPT <- all_data %>%
  dplyr::select(yrID, TransID, PPT:PPT_l5) %>%
  pivot_longer(PPT:PPT_l5,
               names_to = 'lag',
               values_to = 'ppt') %>%
  mutate(ppt = scale(ppt)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "ppt") %>%
  dplyr::select(PPT:PPT_l5) %>%
  as.matrix()

sum(is.na(PPT))/(sum(is.na(PPT)) + sum(!is.na(PPT)))

# 0% missing data for PPT and TEmp

NPP <- all_data %>%
  dplyr::select(yrID, TransID, NPP:NPP_l5) %>%
  pivot_longer(NPP:NPP_l5,
               names_to = 'lag',
               values_to = 'NPP') %>%
  mutate(NPP = scale(NPP)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "NPP") %>%
  dplyr::select(NPP:NPP_l5) %>%
  as.matrix()

sum(is.na(NPP))/(sum(is.na(NPP)) + sum(!is.na(NPP)))

#with subset data, 10% missing

# Make data list ----------------------------------------------------------

data <- list(n.data = n.data,
             n.templag = n.templag,
             n.npplag = n.npplag,
             n.transects = n.transects,
             bray = bray,
             Transect.ID = Transect.ID,
             Temp = Temp,
             PPT = PPT,
             NPP = NPP)

saveRDS(data, here("data_output",
                   '02_birdss',
                   '02_betareg',
                   'betareg_inputs',
                   "model_inputs",
                   "bird_betareg_input_data_list_empirical.RDS"))



