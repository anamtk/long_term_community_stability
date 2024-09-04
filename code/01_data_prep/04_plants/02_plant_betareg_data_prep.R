
#NPS plants environmental variable prep
# November 15, 2023

#this script preps the environmental variables with lags
#for the NPS Plants dataset


# Load packages -----------------------------------------------------------

package.list <- c("here", "tidyverse", 'data.table')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

# Load data ---------------------------------------------------------------

climate <- read.csv(here('data_raw',
                         '04_plants',
                        'seasonal_ppt_vpd.csv'))

climate <- climate[,-c(1:2)]

#to get the sites and transects we need for the other 
#two datasets
stability <- readRDS(here('model_summaries',
                          '04_plants',
                          "plant_MSOM_results_turnover_meanSD.RDS"))

IDs <- read.csv(here('data_output',
                     "04_plants",
                     "01_MSOM",
                     "other_data",
                     'site_year_IDs.csv'), row.names=1)

obs_nps <- readRDS(here('data_output',
                        "04_plants",
                        "01_MSOM",
                        "other_data",
                        'nps_observed_jaccard.RDS'))

# Get response data in order ----------------------------------------------

stability2 <- as.data.frame(stability) %>%
  separate(plot_trans_quad,
           into = c('Plot', 'Transect','Quadrat'),
           sep = "_") %>%
  mutate(Quadrat = as.numeric(Quadrat)) %>%
  left_join(IDs, by = c("Plot", "Transect", "Quadrat", "EventYear"))




# ppt --------------------------------------------------------------------

ppt <- climate %>%
  dplyr::select(-c(vpd_monsoon:vpd_winter)) %>%
  pivot_longer(cols = ppt_monsoon:ppt_winter,
               names_to = "season",
               values_to = "PPT") %>%
  mutate(season = case_when(season == "ppt_spring" ~ 1,
                            season == "ppt_earlysummer" ~ 2,
                            season == "ppt_monsoon"~ 3,
                            TRUE ~ 4))

# vpd --------------------------------------------------------------------


vpd <- climate %>%
  dplyr::select(-c(ppt_monsoon:ppt_winter)) %>%
  pivot_longer(cols = vpd_monsoon:vpd_winter,
               names_to = "season",
               values_to = "VPD") %>%
  mutate(season = case_when(season == "vpd_spring" ~ 1,
                            season == "vpd_earlysummer" ~ 2,
                            season == "vpd_monsoon" ~ 3,
                            TRUE ~ 4))


# Get lags ----------------------------------------------------------------

# ppt_lags <- ppt %>%
#   group_by(Plot) %>%
#   arrange(EventYear, season) %>%
#   #this creates a column for every lag this season to 5 years ago
#   do(data.frame(., setNames(shift(.$PPT, 1:20), c('PPT_l1', 'PPT_l2', 'PPT_l3',
#                                                   'PPT_l4', 'PPT_l5',
#                                                   'PPT_l6', 'PPT_l7',
#                                                   'PPT_l8', 'PPT_l9',
#                                                   'PPT_l10', 'PPT_l11', 
#                                                   'PPT_l12', 'PPT_l13',
#                                                   'PPT_l14','PPT_l15',
#                                                   'PPT_l16','PPT_l17',
#                                                   'PPT_l18','PPT_l19',
#                                                   'PPT_l20')))) %>%
#   ungroup() %>%
#   #start in monsoon, which is when sampling ended
#   filter(season == 3) %>%
#   dplyr::select(-season)

ppt_lags <- ppt %>%
  group_by(Plot) %>%
  arrange(EventYear, season) %>%
  #this creates a column for every lag this season to 5 years ago
  do(data.frame(., setNames(shift(.$PPT, 1:10), c('PPT_l1', 'PPT_l2', 'PPT_l3',
                                                  'PPT_l4', 'PPT_l5',
                                                  'PPT_l6', 'PPT_l7',
                                                  'PPT_l8', 'PPT_l9',
                                                  'PPT_l10')))) %>%
  ungroup() %>%
  #start in monsoon, which is when sampling ended
  filter(season == 3) %>%
  dplyr::select(-season)


# #get npp lags:
# vpd_lags <- vpd %>%
#   group_by(Plot) %>%
#   arrange(EventYear, season)  %>%
#   #this creates a column for every lag this season to 5 years ago
#   do(data.frame(., setNames(shift(.$VPD, 1:20), c('VPD_l1', 'VPD_l2', 'VPD_l3',
#                                                   'VPD_l4', 'VPD_l5',
#                                                   'VPD_l6', 'VPD_l7',
#                                                   'VPD_l8', 'VPD_l9',
#                                                   'VPD_l10', 'VPD_l11', 
#                                                   'VPD_l12', 'VPD_l13',
#                                                   'VPD_l14','VPD_l15',
#                                                   'VPD_l16','VPD_l17',
#                                                   'VPD_l18','VPD_l19',
#                                                   'VPD_l20')))) %>%
#   ungroup() %>%
#   #start in monsoon, which is when sampling ended
#   filter(season == 3) %>%
#   dplyr::select(-season)

vpd_lags <- vpd %>%
  group_by(Plot) %>%
  arrange(EventYear, season)  %>%
  #this creates a column for every lag this season to 5 years ago
  do(data.frame(., setNames(shift(.$VPD, 1:20), c('VPD_l1', 'VPD_l2', 'VPD_l3',
                                                  'VPD_l4', 'VPD_l5',
                                                  'VPD_l6', 'VPD_l7',
                                                  'VPD_l8', 'VPD_l9',
                                                  'VPD_l10')))) %>%
  ungroup() %>%
  #start in monsoon, which is when sampling ended
  filter(season == 3) %>%
  dplyr::select(-season)



# Combine datasets --------------------------------------------------------

all_data <- stability2 %>%
  left_join(ppt_lags, by = c("Plot", "EventYear")) %>%
  left_join(vpd_lags, by = c("Plot", "EventYear"))


# Combine with observed data ----------------------------------------------
obs_nps2 <- obs_nps %>%
  pivot_wider(names_from = "type",
              values_from = "turnover") %>%
  separate(plot_trans_quad,
           into = c("Plot", "Transect", "Quadrat"),
            sep = "_") %>%
  mutate(Quadrat = as.numeric(Quadrat))

all_data2 <- all_data %>%
  left_join(obs_nps2, by = c("Plot", "Transect", "Quadrat", "EventYear"))

# Check for correlation ---------------------------------------------------

library(ggcorrplot)

# dat1 <- all_data %>%
#   dplyr::select(PPT:PPT_l20, VPD:VPD_l20)

dat1 <- all_data %>%
  dplyr::select(PPT:PPT_l10, VPD:VPD_l10)

ggcorrplot(cor(dat1, use = "complete.obs"), 
           type = "upper", lab= TRUE)


# Export ------------------------------------------------------------------

write.csv(all_data2,
          here('data_output',
               '04_plants',
               '02_betareg',
               'other_data',
               "plant_betareg_tidydata.csv"))


# prep data for jags ------------------------------------------------------


# Prep modeled data for ajgs ----------------------------------------------

# Filter years with data on climate/npp -----------------------------------

all_data2 <- all_data %>%
  filter(!is.na(mean)) %>%
  #there should be 70 unique quadrat IDs, I believe
  unite(c(Plot, Transect, Quadrat),
        col = "quadrat_num",
        remove = F,
        sep = "_") %>%
  #I think this is wrong:
  #mutate(Quad.ID = as.numeric(as.factor(quadnum))) %>%
  #make transect_num
  unite(c(Plot, Transect),
        col = "transect_num",
        remove = F,
        sep = "_") %>%
  mutate(Transect.ID = as.numeric(as.factor(transect_num))) %>%
  mutate(Plot.ID = as.numeric(as.factor(Plot))) %>%
  mutate(Quad.ID = as.numeric(as.factor(quadrat_num)))

# Prep data objects for model ---------------------------------------------


# Loop indexing -----------------------------------------------------------

n.data <- nrow(all_data2)

#I think there should be 70 unique quadrats
n.quads <- length(unique(all_data2$quadrat_num))

n.transects <- length(unique(all_data2$transect_num))

n.plots <- length(unique(all_data2$Plot))

n.lag <- all_data2 %>%
  dplyr::select(PPT:PPT_l7) %>%
  ncol()


# Response data -----------------------------------------------------------

# diss <- as.vector(all_data2$mean)
# var.estimate <- as.vector(all_data2$sd^2)

diss <- as.vector(all_data2$mean)
var.estimate <- as.vector(all_data2$sd^2)


# Random effects ----------------------------------------------------------

Plot.ID  <- all_data2 %>%
  distinct(Transect.ID, Plot.ID) %>%
  dplyr::select(Plot.ID) %>%
  as_vector()

Transect.ID <- as.vector(all_data2$Transect.ID)

Quad.ID <- as.vector(all_data2$Quad.ID)

# Covariates --------------------------------------------------------------

PPT <- all_data2  %>%
  dplyr::select(quadnum, EventYear, PPT:PPT_l7) %>%
  pivot_longer(PPT:PPT_l7,
               names_to = 'lag',
               values_to = 'ppt') %>%
  mutate(ppt = scale(ppt)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "ppt") %>%
  dplyr::select(PPT:PPT_l7) %>%
  as.matrix()

sum(is.na(PPT))/(sum(is.na(PPT)) + sum(!is.na(PPT)))
#~0.6% missing data (new - 5/6/24)
#~8% missing data (old)

VPD <- all_data2 %>%
  dplyr::select(quadnum, EventYear, VPD:VPD_l7) %>%
  pivot_longer(VPD:VPD_l7,
               names_to = 'lag',
               values_to = 'vpd') %>%
  mutate(vpd = scale(vpd)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "vpd") %>%
  dplyr::select(VPD:VPD_l7) %>%
  as.matrix()

sum(is.na(VPD))/(sum(is.na(VPD)) + sum(!is.na(VPD)))
#~0.6% missing data (new - 5/6/24)
#~8% missing data (old)


# Combine data into a data list -------------------------------------------


data <- list(n.data = n.data,
             n.lag = n.lag,
             n.quads = n.quads,
             n.transects = n.transects,
             n.plots = n.plots,
             diss = diss,
             var.estimate = var.estimate,
             Plot.ID = Plot.ID,
             Transect.ID = Transect.ID,
             Quad.ID = Quad.ID,
             PPT = PPT, 
             VPD = VPD)

saveRDS(data, here('data_output',
                   '03_grasshoppers',
                   '02_betareg',
                   'betareg_input',
                   "plant_betareg_input_data_list_impdetect.RDS"))


# Prep empirical data for jags --------------------------------------------

# Filter years with data on climate/npp -----------------------------------

all_data2 <- all_data %>%
  filter(!is.na(mean)) %>%
  #there should be 70 unique quadrat IDs, I believe
  unite(c(Plot, Transect, Quadrat),
        col = "quadrat_num",
        remove = F,
        sep = "_") %>%
  #I think this is wrong:
  #mutate(Quad.ID = as.numeric(as.factor(quadnum))) %>%
  #make transect_num
  unite(c(Plot, Transect),
        col = "transect_num",
        remove = F,
        sep = "_") %>%
  mutate(Transect.ID = as.numeric(as.factor(transect_num))) %>%
  mutate(Plot.ID = as.numeric(as.factor(Plot))) %>%
  mutate(Quad.ID = as.numeric(as.factor(quadrat_num)))

# Prep data objects for model ---------------------------------------------

all_data2 <- all_data2 %>%
  filter(!is.na(observed_all))

# Loop indexing -----------------------------------------------------------

n.data <- nrow(all_data2)

#I think there should be 70 unique quadrats
n.quads <- length(unique(all_data2$quadrat_num))

n.transects <- length(unique(all_data2$transect_num))

n.plots <- length(unique(all_data2$Plot))

#going back 2 years
n.lag <- all_data2 %>%
  dplyr::select(PPT:PPT_l7) %>%
  ncol()

# n.lag <- all_data2 %>%
#   dplyr::select(PPT:PPT_l20) %>%
#   ncol()


# Response data -----------------------------------------------------------

# diss <- as.vector(all_data2$mean)
# var.estimate <- as.vector(all_data2$sd^2)

diss <- as.vector((all_data2$observed_all*(n.data-1) + 0.5)/n.data)

# Random effects ----------------------------------------------------------

Plot.ID  <- all_data2 %>%
  distinct(Transect.ID, Plot.ID) %>%
  dplyr::select(Plot.ID) %>%
  as_vector()

Transect.ID <- as.vector(all_data2$Transect.ID)

# Transect.ID  <- all_data2 %>%
#   distinct(Quad.ID, Transect.ID) %>%
#   dplyr::select(Transect.ID) %>%
#   as_vector()

Quad.ID <- as.vector(all_data2$Quad.ID)

# Covariates --------------------------------------------------------------

PPT <- all_data2  %>%
  dplyr::select(quadnum, EventYear, PPT:PPT_l7) %>%
  pivot_longer(PPT:PPT_l7,
               names_to = 'lag',
               values_to = 'ppt') %>%
  mutate(ppt = scale(ppt)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "ppt") %>%
  dplyr::select(PPT:PPT_l7) %>%
  as.matrix()

sum(is.na(PPT))/(sum(is.na(PPT)) + sum(!is.na(PPT)))
#~8% missing data

VPD <- all_data2 %>%
  dplyr::select(quadnum, EventYear, VPD:VPD_l7) %>%
  pivot_longer(VPD:VPD_l7,
               names_to = 'lag',
               values_to = 'vpd') %>%
  mutate(vpd = scale(vpd)) %>%
  pivot_wider(names_from = 'lag',
              values_from = "vpd") %>%
  dplyr::select(VPD:VPD_l7) %>%
  as.matrix()

sum(is.na(VPD))/(sum(is.na(VPD)) + sum(!is.na(VPD)))
#~8% missing data


# Combine data into a data list -------------------------------------------


data <- list(n.data = n.data,
             n.lag = n.lag,
             n.quads = n.quads,
             n.transects = n.transects,
             n.plots = n.plots,
             diss = diss,
             #var.estimate = var.estimate,
             Plot.ID = Plot.ID,
             Transect.ID = Transect.ID,
             Quad.ID = Quad.ID,
             PPT = PPT, 
             VPD = VPD)

saveRDS(data, here('data_output',
                   '03_grasshoppers',
                   '02_betareg',
                   'betareg_input',
                   "plant_betareg_input_data_list_empirical.RDS"))



