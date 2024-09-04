#Detection partial effects plots
#October 27, 2023

#this script creates partial plots of the detection covariates in the MSAMs

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

#get scale_df function
source(here('code',
            "00_functions",
            "tidy_functions.R"))

# Load data ---------------------------------------------------------------

#need:
#summaries of parameter effect
fish_sum <- readRDS(here('model_summaries',
                         '01_fish',
                         'other_parameters',
                         'fish_detection_summary.RDS'))

bird_sum <- readRDS(here('model_summaries',
                         '02_birds',
                         'other_parameters',
                         'bird_detection_summary.RDS'))

plant_sum <- readRDS(here('model_summaries',
                          '04_plants',
                          'other_parameters',
                          'plant_detection_summary.RDS'))

# Effect plots ------------------------------------------------------------


# Fish --------------------------------------------------------------------


fish_effects <- as.data.frame(fish_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm %in% c("a1.Vis", "a2.Size")) %>% 
  mutate(parm = case_match(parm,
    "a1.Vis" ~ "Dive visibility",
    "a2.Size" ~ "Fish size"
  ))

fishdetect <- ggplot(fish_effects, aes(x = `50%`, y = parm)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3, alpha = 0.4) +
  geom_errorbar(aes(xmin = `2.5%`, xmax = `97.5%`), size = 0.4, width = 0, alpha = 0.6) +
  geom_point(size = 1.5, fill = "#FFFFFF", shape = 21, stroke = 0.5) +
  scale_x_continuous(limits = c(-3.25, 3.25), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  labs(x = "Covariate effect",
       y = "SBC fish",
       title = "(a) SBC fish") +
  # coord_flip() +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        axis.title.y = element_blank(),
        # axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title.position = "plot",
        plot.title = element_text(size = 8),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank()) +
  geom_hline(yintercept = 0.4)

fishdetect

# Birds -------------------------------------------------------------------

bird_effects <- as.data.frame(bird_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(parm %in% c("a1.Effort", "a2.Size")) %>% 
  mutate(parm = case_match(parm,
    "a1.Effort" ~ "Survey effort",
    "a2.Size" ~ "Bird size"
  ))

birddetect <- ggplot(bird_effects, aes(x = `50%`, y = parm)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3, alpha = 0.4) +
  geom_errorbar(aes(xmin = `2.5%`, xmax = `97.5%`), size = 0.4, width = 0, alpha = 0.6) +
  geom_point(size = 1.5, fill = "#FFFFFF", shape = 21, stroke = 0.5) +
  scale_x_continuous(limits = c(-1.6, 1.6), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  labs(x = "Covariate effect",
       y = "",
       title = "(b) KNZ birds") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        axis.title.y = element_blank(),
        # axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title.position = "plot",
        plot.title = element_text(size = 8),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_hline(yintercept = 0.4)

birddetect


# Plants ------------------------------------------------------------------

plant_effects <- as.data.frame(plant_sum$quantiles) %>%
  rownames_to_column(var = "parm") %>%
  filter(str_detect(parm, "a1.Cover|a2.LifeGroup")) %>% 
  mutate(new_parm = case_match(
    parm,
    "a1.Cover" ~ "Average\nspecies\ncover", 
    "a2.LifeGroup[2]" ~ "Perennial forb",
    "a2.LifeGroup[3]" ~ "Perennial grass",
    "a2.LifeGroup[4]" ~ "Perennial shrub",
    "a2.LifeGroup[5]" ~ "Annual grass",
    "a2.LifeGroup[6]" ~ "Biennial forb",
    "a2.LifeGroup[7]" ~ "Perennial cacti",
    "a2.LifeGroup[8]" ~ "Perennial succulent"
  ),
  new_parm = fct_inorder(new_parm)) 

plant_effects1 <- plant_effects %>%
  filter(parm == "a1.Cover")

plantdetect1 <- ggplot(plant_effects1, aes(x = `50%`, y = new_parm)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3, alpha = 0.4) +
  geom_errorbar(aes(xmin = `2.5%`, xmax = `97.5%`), size = 0.4, width = 0, alpha = 0.6) +
  geom_point(size = 1.5, fill = "#FFFFFF", shape = 21, stroke = 0.5) +
  scale_x_continuous(limits = c(-1.75, 1.75), breaks = c(-1.5, -1, -0.5, 0, 0.5, 1, 1.5)) +
  labs(x = "Covariate effect",
       y = "",
       title = "(c) PFNP plants: average species cover") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        axis.title.y = element_blank(),
        # axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title.position = "plot",
        plot.title = element_text(size = 8),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_hline(yintercept = 0.4)

plantdetect1

plant_effects2 <- plant_effects %>%
  filter(str_detect(parm, "a2")) %>%
  filter(parm != "a2.LifeGroup[1]")

plantdetect2 <- ggplot(plant_effects2, aes(x = `50%`, y = new_parm)) +
  geom_vline(xintercept = 0, linetype = 2, linewidth = 0.3, alpha = 0.4) +
  geom_errorbar(aes(xmin = `2.5%`, xmax = `97.5%`), size = 0.4, width = 0, alpha = 0.6) +
  geom_point(size = 1.5, fill = "#FFFFFF", shape = 21, stroke = 0.5) +
  scale_x_continuous(limits = c(-10, 40)) +
  labs(x = "Covariate effect",
       y = "",
       title = "(d) PFNP plants: Life group") +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 7),
        axis.title.y = element_blank(),
        # axis.title.y = element_text(angle = 0, vjust = 0.5),
        plot.title.position = "plot",
        plot.title = element_text(size = 8),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_hline(yintercept = 0.4)

plantdetect2

# String them together ----------------------------------------------------

detect_together <- (fishdetect)/(birddetect)/(plantdetect1)/(plantdetect2) +
  plot_layout(heights = c(10, 10, 10, 10))

detect_together

# ggsave(plot = detect_together,
#        filename = here("pictures",
#                        "detection_models",
#                        "detection_covariate_effects.jpg"),
#        height = 16,
#        width = 10,
#        units = "cm",
#        dpi = 300)

