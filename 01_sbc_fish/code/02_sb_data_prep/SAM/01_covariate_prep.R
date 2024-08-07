#Prepping environmental data for stability SAM
#Ana Miller-ter Kuile
#June 26, 2023

#this is a script that can prep environmental
#data for the SAM model

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 

package.list <- c("here", "tidyverse",
                  "data.table")

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

## And loading them

for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#giant kelp biomass (yearly)

#bottom temperature (every 15 minutes - maybe 
#summarise monthly for now? and later think about
#more biologically-relevant "seasons"?)

biomass <- read.csv(here('01_sbc_fish',
                         "data_raw",
                         "environmental",
                         "Annual_All_Species_Biomass_at_transect_20230814.csv"))

#from temp "data prep" script for SAM. these are HUGE datasets
#so didn't want to put summary in here because it takes forever
bottemp <- read.csv(here('01_sbc_fish',
                         "data_outputs",
                         'SAM',
                         "data_prep",
                         "seasonal_bottom_temps.csv"))

chl_a <- read.csv(here('01_sbc_fish',
                       "data_outputs",
                       "SAM",
                       'data_prep',
                       "monthly_chla.csv"))

#to get the sites and transects we need for the other 
#two datasets
stability <- readRDS(here("01_sbc_fish",
                           "monsoon",
                           "fish_MSAM",
                           "outputs",
                           "fish_bray_meanSD.RDS"))

sbc_obs <- readRDS(here('05_visualizations',
                        'viz_data',
                        'sbc_observed_bray.RDS'))

IDs <- read.csv(here('01_sbc_fish',
                      "data_outputs",
                      "metadata",
                      "site_year_IDs.csv"))
# Biomass by Year by Site -------------------------------------------------

colnames(biomass)
#i'll filter only giant kelp "MAPY"
#and the dry biomass value "DRY_GM2"

biomass <- biomass %>%
  dplyr::select(YEAR, MONTH, SITE, 
                TRANSECT, SP_CODE, DRY_GM2) %>%
  filter(SP_CODE == "MAPY")

# Get seasonal Chlorophyl_a -----------------------------------------------

chl_a2 <- chl_a %>%
  mutate(SEASON = case_when(MONTH %in% c(12, 1, 2,
                                         3, 4, 5) ~ "COLD",
                            MONTH %in% c(6, 7, 8, 9,
                                         10, 11) ~ "WARM")) %>%
  group_by(site, YEAR, SEASON) %>%
  summarise(sd_chla = sd(chla, na.rm = T),
            chla = mean(chla, na.rm = T)) %>%
  ungroup()
  
ggplot(chl_a2, aes(x = SEASON, y = chla)) +
  geom_point(position = position_jitter(width = 0.25))


# Prep stability dataset --------------------------------------------------

stability2 <- as.data.frame(stability) %>%
  rownames_to_column(var = "var") %>%
  filter(var != "deviance") %>%
  separate(var,
           into = c('siteID', 'yrID'),
           sep = ",") %>%
  mutate(siteID = str_sub(siteID, 6, nchar(siteID))) %>%
  mutate(yrID = str_sub(yrID, 1, (nchar(yrID)-1))) %>%
  rename("bray" = "Mean") %>%
  dplyr::select(yrID, siteID, bray, SD) %>%
  mutate(yrID = as.numeric(yrID),
         siteID = as.numeric(siteID)) %>%
  left_join(IDs, by = c("siteID", "yrID"))


# Combine raw data --------------------------------------------------------

sbc_obs2 <- sbc_obs %>%
  pivot_wider(names_from = 'type',
              values_from = "bray")

stability3 <- stability2 %>%
  left_join(sbc_obs2, by = c("yrID", "siteID"))

# Get site and transect IDs -----------------------------------------------

sites <- stability3 %>%
  distinct(siteID, SITE_TRANS) %>%
  separate(SITE_TRANS, into= c("SITE", "TRANSECT"),
           sep = "_",
           remove = F) 

site <- sites$SITE
sitetrans <- sites$SITE_TRANS

# Filter environmental to sites and transects in dataset ------------------

bottemp2 <- bottemp %>%
  filter(SITE %in% site)

bottemp2 %>%
  group_by(SEASON) %>%
  summarise(mean = mean(TEMP_C))



biomass2 <- biomass %>%
  unite(c(SITE, TRANSECT),
        col = "SITE_TRANS",
        sep = "_",
        remove = F) %>%
  filter(SITE_TRANS %in% sitetrans) %>%
  mutate(DRY_GM2 = case_when(DRY_GM2 == -99999 ~ NA_real_,
                             TRUE ~ DRY_GM2))

chl_a2 <- chl_a2 %>%
  filter(site %in% site)
  
# Make Lags ---------------------------------------------------------------

#biomass2:
#yearly lags - maybe start with 5-6 years back?
#site, transect, site_Transect, year
#Bio_1, lag back 5 years

bio_lags <- biomass2 %>%
  group_by(SITE_TRANS) %>%
  arrange(SITE_TRANS, YEAR) %>%
  #this creates a column for every lag 1:6 years ago
  do(data.frame(., setNames(shift(.$DRY_GM2, 1:5), c("DRY_GM2_l1",
                                                 "DRY_GM2_l2", "DRY_GM2_l3",
                                                 "DRY_GM2_l4", "DRY_GM2_l5")))) %>%
  ungroup() %>%
  dplyr::select(YEAR, SITE_TRANS, SITE,
                TRANSECT, DRY_GM2:DRY_GM2_l5)

#temperatuer lags - seasonally - going back 
#how many seasons now??? HMMM....

temp_lags <- bottemp2 %>%
  group_by(SITE) %>%
  arrange(SITE, YEAR, SEASON) %>%
  #this creates a column for a lag 1:5 seasons ago
  do(data.frame(., setNames(shift(.$TEMP_C, 1:5), c("TEMP_C_l1",
                                                     "TEMP_C_l2", "TEMP_C_l3",
                                                     "TEMP_C_l4", "TEMP_C_l5")))) %>%
  ungroup() %>%
  dplyr::select(SITE, YEAR, SEASON,
                TEMP_C:TEMP_C_l5)

#set first season of temp lags to be 
#"WARM" since this is when all the surveys took place
temp_lags2 <- temp_lags %>%
  filter(SEASON == "WARM") %>%
  dplyr::select(-SEASON) %>%
  mutate(YEAR = as.integer(YEAR))

temp_lags_l <- bottemp2 %>%
  group_by(SITE) %>%
  arrange(SITE, YEAR, SEASON) %>%
  #this creates a column for a lag 1:10 seasons ago
  do(data.frame(., setNames(shift(.$TEMP_C, 1:10), c("TEMP_C_l1",
                                                    "TEMP_C_l2", "TEMP_C_l3",
                                                    "TEMP_C_l4", "TEMP_C_l5",
                                                    "TEMP_C_l6", "TEMP_C_l7",
                                                    "TEMP_C_l8", "TEMP_C_l9", 
                                                    "TEMP_C_l10")))) %>%
  ungroup() %>%
  dplyr::select(SITE, YEAR, SEASON,
                TEMP_C:TEMP_C_l10)

temp_lags_l2 <- temp_lags_l %>%
  filter(SEASON == "WARM") %>%
  dplyr::select(-SEASON) %>%
  mutate(YEAR = as.integer(YEAR))

chla_lags <- chl_a2 %>%
  group_by(site) %>%
  arrange(site, YEAR, SEASON) %>%
  #this creates a column for a lag 1:5 seasons ago
  do(data.frame(., setNames(shift(.$chla, 1:5), c("chla_l1",
                                                    "chla_l2", "chla_l3",
                                                    "chla_l4", "chla_l5")))) %>%
  ungroup() %>%
  dplyr::select(site, YEAR, SEASON,
                chla:chla_l5)

chla_lags2 <- chla_lags %>%
  filter(SEASON == "WARM") %>%
  dplyr::select(-SEASON) 

# Combine all data --------------------------------------------------------

all_data <- stability3 %>%
  left_join(bio_lags, by = c("YEAR",
                             "SITE_TRANS")) %>%
  left_join(temp_lags2, by = c("SITE", "YEAR")) %>%
  left_join(chla_lags2, by = c("SITE" = "site", "YEAR"))
  

write.csv(all_data, here("01_sbc_fish",
                         "data_outputs",
                         'SAM',
                         'data_prep',
                        "stability_metrics_with_covariates.csv"))

all_data2 <- stability3 %>%
  left_join(bio_lags, by = c("YEAR",
                             "SITE_TRANS")) %>%
  left_join(temp_lags_l2, by = c("SITE", "YEAR")) %>%
  left_join(chla_lags2, by = c("SITE" = "site", "YEAR"))

write.csv(all_data2, here("01_sbc_fish",
                         "data_outputs",
                         'SAM',
                         'data_prep',
                         "stability_metrics_with_covariates_long.csv"))
