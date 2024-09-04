#Prepping environmental data for stability regression
#June 26, 2023

#this is a script that can prep environmental
#data for the beta regression model

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
#this is also a really big file that can be downloaded here:
#https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sbc.18.27
# biomass <- read.csv(here("data_raw",
#                          '01_fish',
#                          "Annual_All_Species_Biomass_at_transect_20230814.csv"))

#from temp "data prep" script. these are HUGE datasets
#so didn't want to put summary in here because it takes forever
bottemp <- read.csv(here("data_raw",
                         '01_fish',
                         "seasonal_bottom_temps.csv"))

#to get the sites and transects we need for the other 
#two datasets
stability <- readRDS(here('model_summaries',
                          "01_fish",
                           "fish_MSAM_results_bray_meanSD.RDS"))

sbc_obs <- readRDS(here('data_output',
                        "01_fish",
                        "01_MSAM",
                        'other_data',
                        'sbc_observed_bray.RDS'))

IDs <- read.csv(here('data_output',
                     "01_fish",
                     "01_MSAM",
                     'other_data',
                      "site_year_IDs.csv"))

# Biomass by Year by Site -------------------------------------------------

colnames(biomass)
#i'll filter only giant kelp "MAPY"
#and the dry biomass value "DRY_GM2"

biomass <- biomass %>%
  dplyr::select(YEAR, MONTH, SITE, 
                TRANSECT, SP_CODE, DRY_GM2) %>%
  filter(SP_CODE == "MAPY")

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


# Combine all data --------------------------------------------------------

all_data2 <- stability3 %>%
  left_join(bio_lags, by = c("YEAR",
                             "SITE_TRANS")) %>%
  left_join(temp_lags_l2, by = c("SITE", "YEAR")) 

write.csv(all_data2, here("data_output",
                          '01_fish',
                         '02_betareg',
                         'other_data',
                         "fish_betareg_tidydata.csv"))

# Prep data objects for JAGS ----------------------------------------------


# Prep modeled data for jags ----------------------------------------------


n.data <- nrow(all_data)

bray <- as.vector(all_data$bray)
var.estimate <- as.vector(all_data$SD^2)

n.transects <- length(unique(all_data$SITE_TRANS))

Transect.ID <- all_data$siteID

n.years <- length(unique(all_data$YEAR))

Year.ID <- all_data$yrID

n.sites <- length(unique(all_data$SITE))

Site.ID <- all_data %>%
  mutate(SITE = as.numeric(as.factor(SITE))) %>%
  dplyr::select(SITE) %>%
  as_vector()

# Site.ID <- all_data %>%
#   distinct(siteID, SITE) %>%
#   arrange(siteID) %>%
#   mutate(SITE = as.numeric(as.factor(SITE))) %>%
#   dplyr::select(SITE) %>%
#   as_vector()

n.kelplag <- all_data %>%
  dplyr::select(DRY_GM2:DRY_GM2_l5) %>%
  ncol()

Kelp <- all_data %>%
  dplyr::select(SITE_TRANS, YEAR, DRY_GM2:DRY_GM2_l5) %>%
  pivot_longer(DRY_GM2:DRY_GM2_l5,
               names_to = 'lag',
               values_to = 'kelp') %>%
  mutate(kelp = scale(kelp)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "kelp") %>%
  dplyr::select(DRY_GM2:DRY_GM2_l5) %>%
  as.matrix()

sum(is.na(Kelp))/(sum(!is.na(Kelp)) + sum(is.na(Kelp)))

#~8% missing data

n.templag <- all_data %>%
  dplyr::select(TEMP_C:TEMP_C_l10) %>%
  ncol()

Temp <- all_data %>%
  dplyr::select(SITE_TRANS, YEAR, TEMP_C:TEMP_C_l10) %>%
  pivot_longer(TEMP_C:TEMP_C_l10,
               names_to = 'lag',
               values_to = 'temp') %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "temp") %>%
  dplyr::select(TEMP_C:TEMP_C_l10) %>%
  as.matrix()

sum(is.na(Temp))/(sum(is.na(Temp)) + sum(!is.na(Temp)))
#13% with longer lag missing data


data <- list(n.data = n.data,
             n.transects = n.transects,
             n.sites = n.sites,
             n.years = n.years,
             Transect.ID = Transect.ID,
             Year.ID = Year.ID,
             Site.ID = Site.ID,
             bray = bray,
             var.estimate = var.estimate,
             n.kelplag = n.kelplag,
             Kelp = Kelp,
             ones = rep(1, n.data),
             n.templag = n.templag,
             Temp = Temp)

saveRDS(data, here("data_output",
                   '01_fish',
                   '02_betareg',
                   'betareg_inputs',
                   "fish_betareg_input_data_list_impdetect.RDS"))


# Prep empirical data for jags --------------------------------------------


# Remove missing data -----------------------------------------------------

all_data <- all_data %>%
  filter(!is.na(observed_all))

n.data <- nrow(all_data)

#transform like: https://stats.stackexchange.com/questions/31300/dealing-with-0-1-values-in-a-beta-regression
bray <- as.vector((all_data$observed_all*(n.data-1) + 0.5)/n.data)

n.transects <- length(unique(all_data$SITE_TRANS))

Transect.ID <- all_data$siteID

n.years <- length(unique(all_data$YEAR))

Year.ID <- all_data$yrID

n.sites <- length(unique(all_data$SITE))

Site.ID <- all_data %>%
  mutate(SITE = as.numeric(as.factor(SITE))) %>%
  dplyr::select(SITE) %>%
  as_vector()
# Site.ID <- all_data %>%
#   distinct(siteID, SITE) %>%
#   arrange(siteID) %>%
#   mutate(SITE = as.numeric(as.factor(SITE))) %>%
#   dplyr::select(SITE) %>%
#   as_vector()

n.kelplag <- all_data %>%
  dplyr::select(DRY_GM2:DRY_GM2_l5) %>%
  ncol()

Kelp <- all_data %>%
  dplyr::select(SITE_TRANS, YEAR, DRY_GM2:DRY_GM2_l5) %>%
  pivot_longer(DRY_GM2:DRY_GM2_l5,
               names_to = 'lag',
               values_to = 'kelp') %>%
  mutate(kelp = scale(kelp)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "kelp") %>%
  dplyr::select(DRY_GM2:DRY_GM2_l5) %>%
  as.matrix()

sum(is.na(Kelp))/(sum(!is.na(Kelp)) + sum(is.na(Kelp)))

#~8% missing data

n.templag <- all_data %>%
  dplyr::select(TEMP_C:TEMP_C_l10) %>%
  ncol()

Temp <- all_data %>%
  dplyr::select(SITE_TRANS, YEAR, TEMP_C:TEMP_C_l10) %>%
  pivot_longer(TEMP_C:TEMP_C_l10,
               names_to = 'lag',
               values_to = 'temp') %>%
  mutate(temp = scale(temp)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "temp") %>%
  dplyr::select(TEMP_C:TEMP_C_l10) %>%
  as.matrix()

sum(is.na(Temp))/(sum(is.na(Temp)) + sum(!is.na(Temp)))
#13% with longer lag missing data


data <- list(n.data = n.data,
             n.transects = n.transects,
             n.sites = n.sites,
             n.years = n.years,
             Transect.ID = Transect.ID,
             Year.ID = Year.ID,
             Site.ID = Site.ID,
             bray = bray,
             n.kelplag = n.kelplag,
             Kelp = Kelp,
             n.templag = n.templag,
             Temp = Temp)

saveRDS(data, here("data_output",
                   '01_fish',
                   '02_betareg',
                   'betareg_inputs',
                   "fish_betareg_input_data_list_emperical.RDS"))




