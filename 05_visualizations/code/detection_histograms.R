#Distribution of detection probabilities
#Ana Miller-ter Kuile
#October 27, 2023

#this script looks at the distribution of detection probabilities for each
#MSAM/MSOM for each dataset


# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  'emmeans', 'glmmTMB',
                  'patchwork')


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

theme_set(theme_bw())


# Load data ---------------------------------------------------------------

#need: 
#summary of species-level detection probabilities

fish_detect <- readRDS(here('01_sbc_fish',
                            'monsoon',
                            'fish_MSAM',
                            'outputs',
                            'fish_p0_summary.RDS'))

hopper_detect <- readRDS(here('03_sev_grasshoppers',
                              'monsoon',
                              'MSAM',
                              'outputs',
                              'grasshopper_p_summary.RDS'))

bird_detect <- readRDS(here('02_konza_birds',
                            'monsoon',
                            'MSAM',
                            'outputs',
                            'bird_p0_summary.RDS'))

plant_detect <- readRDS(here('04_nps_plants',
                             'monsoon',
                             'nps_MSAM',
                             'outputs_yrsite',
                             'nps_p0_summary.RDS'))



fish_ob <- read.csv(here('01_sbc_fish',
                         'data_outputs',
                         'MSAM',
                         'all_fish_data.csv'))

bird_ob <- read.csv(here('02_konza_birds',
                         'data_outputs',
                         'MSAM',
                         'knz_tidy_data_for_model.csv'))

hop_ob <- read.csv(here('03_sev_grasshoppers',
                        'data_outputs',
                        'MSAM',
                        'sev_tidy_data_for_model.csv'))

plant_ob <- read.csv(here('04_nps_plants',
                          'data_outputs',
                          'MSAM',
                          'pfnp_tidy_data_for_model.csv'))

# Prep for plotting -------------------------------------------------------


# Modeled detection probs -------------------------------------------------


fish_det2 <- as.data.frame(fish_detect$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm != "deviance")%>%
  mutate(dataset = "fish")

hop_det2 <- as.data.frame(hopper_detect$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm != "deviance") %>%
  mutate(dataset = "hoppers")

bird_det2 <- as.data.frame(bird_detect$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm != "deviance") %>%
  mutate(dataset = "birds")

plant_det2 <- as.data.frame(plant_detect$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm != "deviance") %>%
  mutate(dataset = "plants")

detect_df <- fish_det2 %>%
  rbind(hop_det2, bird_det2, plant_det2)



# Observed values ---------------------------------------------------------


# Summarise to total abundances/detections --------------------------------

colnames(fish_ob)

fish_ob2 <- fish_ob %>%
  group_by(specID) %>%
  summarise(total = sum(COUNT, na.rm =T),
            mean = mean(COUNT, na.rm = T)) %>%
  ungroup() %>%
  mutate(dataset = "fish") %>%
  dplyr::select(-specID)

colnames(bird_ob)

bird_ob2 <- bird_ob %>%
  group_by(SpecID) %>%
  summarise(total = sum(NOBS, na.rm =T),
            mean = mean(NOBS, na.rm = T)) %>%
  ungroup() %>%
  mutate(dataset = "birds") %>%
  dplyr::select(-SpecID)

colnames(hop_ob)  

hop_ob2 <- hop_ob %>%
  group_by(speciesID) %>%
  summarise(total = sum(CNT, na.rm =T),
            mean = mean(CNT, na.rm = T)) %>%
  ungroup() %>%
  mutate(dataset = "hoppers") %>%
  dplyr::select(-speciesID)

colnames(plant_ob)  

plant_ob2 <- plant_ob %>%
  group_by(SpecID) %>%
  summarise(total = sum(presence, na.rm =T),
            mean = mean(presence, na.rm = T)) %>%
  ungroup() %>%
  mutate(dataset = "plants") %>%
  dplyr::select(-SpecID)


#combine
obs <- fish_ob2 %>%
  bind_rows(bird_ob2, hop_ob2, plant_ob2) %>%
  group_by(dataset) %>%
  mutate(tot = scale(total),
         mn = scale(mean)) %>%
  ungroup()

labs <- c("SBC fish", "SEV grasshoppers", "KNZ birds", "PFNP Plants")
names(labs) <- c("fish", "hoppers", 'birds', 'plants')

obs %>%
  mutate(dataset = factor(dataset, levels = c("fish", 'birds',"hoppers", 'plants'))) %>%
ggplot(aes(x = mn)) +
  geom_histogram() +
  facet_grid(dataset~., scales = "free",
             labeller = labeller(dataset = labs))  +
  theme(strip.background = element_rect(fill = "white"))

(obsplot <- obs %>%
  mutate(dataset = factor(dataset, levels = c("fish", 'birds',"hoppers", 'plants'))) %>%
  ggplot(aes(x = dataset, y = mn)) +
  geom_boxplot() +
  labs(x = "Dataset", 
       y = "Species-level \n scaled abundance/prevalence") +
  scale_x_discrete(labels = labs) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_y_sqrt() )
  

# Create histograms -------------------------------------------------------

labs <- c("SBC fish", "SEV grasshoppers", "KNZ birds", "PFNP Plants")
names(labs) <- c("fish", "hoppers", 'birds', 'plants')

detect_df %>%
  mutate(dataset = factor(dataset, levels = c("hoppers", "fish", 'birds', 'plants'))) %>%
ggplot(aes(x = `50%`)) +
  geom_histogram() +
  facet_grid(dataset~.,
             labeller = labeller(dataset = labs)) +
  labs(x = "Species-level detection probability",
       y = "Count") +
  theme(strip.background = element_rect(fill = "white"))

(detplot <- detect_df %>%
  mutate(dataset = factor(dataset, levels = c("fish", 'birds',"hoppers", 'plants'))) %>%
  ggplot(aes(x = dataset, y = `50%`)) +
  geom_boxplot() +
  labs(x = "Dataset", 
       y = "Species-level \n detection probability") +
  scale_x_discrete(labels = labs) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

obsplot / detplot + plot_annotation(tag_levels = "A")

ggsave(plot = last_plot(),
       filename = here("pictures",
                       "detection_models",
                       "species_detection_probabilities.jpg"),
       height = 5,
       width = 5,
       units = "in")

mod <- lm(`50%` ~ dataset,
          data = detect_df)

summary(mod)

em <- emmeans(mod, pairwise ~ dataset)

em

detect_df %>%
  group_by(dataset) %>%
  summarise(mean = mean(`50%`))
