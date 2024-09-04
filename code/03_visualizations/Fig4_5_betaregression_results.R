#SAM model results version 2
#April 2024

#this script generates results figures for SAM models
#with both modeled and raw datasets

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

theme_set(theme_classic())
theme_update(plot.title.position = "plot",
             panel.grid = element_blank(),
             axis.title = element_text(size = 6),
             axis.text = element_text(size = 5),
             plot.title = element_text(size = 7),
             legend.text = element_text(size = 4),
             legend.key.size = unit(0.25, "cm"),
             legend.key = element_blank(), 
             legend.title=element_blank(),
             legend.background = element_blank())

source(here('00_functions',
            'tidy_functions.R'))

source(here('00_functions',
            'plot_functions.R'))

modeled_col <- "#E88C23"
observed_col <- "#438AA8"
# Load data ---------------------------------------------------------------

#fish datasets:
fish_sam <- readRDS(here('model_summaries',
                         '01_fish',
                         'fish_betareg_model_summary_impdetect.RDS'))

fish_sam_raw <- readRDS(here('model_summaries',
                             '01_fish',
                             'fish_betareg_model_summary_empirical.RDS'))

#bird datasets:
bird_sam <- readRDS(here('model_summaries',
                         '02_birds',
                         'bird_betareg_model_summary_impdetect.RDS'))

bird_sam_raw <- readRDS(here('model_summaries',
                             '02_birds',
                             'bird_betareg_model_summary_empirical.RDS'))

#grasshopper datasets:
sev_sam <- readRDS(here('model_summaries',
                        '03_grasshoppers',
                        'grasshopper_betareg_model_summary_impdetect.RDS'))

sev_sam_raw <- readRDS(here('model_summaries',
                            '03_grasshoppers',
                            'grasshopper_betareg_model_summary_empirical.RDS'))

#cummulative temp and ppt from both models
sev_sam_temp <- readRDS(here('model_summaries',
                             '03_grasshoppers',
                             'other_parameters',
                             'grasshopper_cummtemp.RDS'))

sev_sam_ppt <- readRDS(here('model_summaries',
                            '03_grasshoppers',
                            'other_parameters',
                            'grasshopper_cummppt.RDS'))

sev_sam_temp_raw <- readRDS(here('model_summaries',
                                 '03_grasshoppers',
                                 'other_parameters',
                                 'grasshopper_cummtemp_emp.RDS'))

sev_sam_ppt_raw <- readRDS(here('model_summaries',
                                '03_grasshoppers',
                                'other_parameters',
                                'grasshopper_cummppt_emp.RDS'))

sev_sam_npp <- readRDS(here('model_summaries',
                            '03_grasshoppers',
                            'other_parameters',
                            'grasshopper_cummnpp.RDS'))

sev_sam_npp_raw <- readRDS(here('model_summaries',
                                '03_grasshoppers',
                                'other_parameters',
                                'grasshopper_cummnpp_emp.RDS'))


#plant datasets:
plant_sam <- readRDS(here('model_summaries',
                          '04_plants',
                          'plant_betareg_model_summary_impdetect.RDS'))

plant_sam_raw <- readRDS(here('model_summaries',
                              '04_plants',
                              'plant_betareg_model_summary_empirical.RDS'))


# Main intercept plot -----------------------------------------------------

#look at intercept in these datasets as the 
#"expected change" under average covariates
#are the intercepts really different? 

b0_fun <- function(model){
  betas <- as.data.frame(model$quantiles) %>%
    rownames_to_column(var = "parm") %>%
    filter(parm == "b0") %>%
    rename("LCI" = `2.5%`,
           "median" = `50%`,
           'UCI' = `97.5%`) %>%
    dplyr::select(LCI, median, UCI) %>%
    mutate(LCI = plogis(LCI),
           median = plogis(median),
           UCI = plogis(UCI))
  
  return(betas)
}

model_list <- list(fish_sam, fish_sam_raw,
                   bird_sam, bird_sam_raw,
                   sev_sam, sev_sam_raw,
                   plant_sam, plant_sam_raw)

names <- c("SBC fish_modeled", "SBC fish_observed",
           "KNZ birds_modeled", "KNZ birds_observed",
           'SEV grasshoppers_modeled', 
           "SEV grasshoppers_observed",
           'PFNP plants_modeled', "PFNP plants_observed")

names(model_list) <- names

b0_list <- lapply(model_list, b0_fun)

b0_df <- bind_rows(b0_list, .id = "id") %>%
  separate(id, 
           into = c('dataset', 'type'),
           sep = "_") %>%
  mutate(dataset = factor(dataset,
                          levels = c("SBC fish", 
                                     "KNZ birds", 
                                     "SEV grasshoppers", 
                                     "PFNP plants")))

intercept_plot <- ggplot(b0_df) +
  geom_pointrange(aes(x = dataset,
                      y = median,
                      ymin = LCI,
                      ymax = UCI,
                      color = type),
                  position=position_dodge(width=0.5),
                  size = 0.4) +
  scale_color_manual(values = c(modeled_col, observed_col),
                     labels = c("Modeled", "Observed \n (all surveys)")) +
  labs(x = "Dataset", y = "Baseline dissimilarity (intercept) \n (median and 95% BCI)")
# Effect plots ------------------------------------------------------------

#option for a main figure of effects plots per model

#TO-DO:
#make a boxplot of uncertainty (modeled
#communities have lower uncertainties)

sam_plot_fun <- function(model_model, model_raw){
  
  beta_m <- as.data.frame(model_model$quantiles) %>%
    rownames_to_column(var = "parm") %>%
    filter(str_detect(parm, "b")) %>%
    filter(!str_detect(parm, "b0")) %>%
    filter(!str_detect(parm, 'sig.web')) %>%
    mutate(type = "modeled")
  
  beta_r <- as.data.frame(model_raw$quantiles) %>%
    rownames_to_column(var = "parm") %>%
    filter(str_detect(parm, "b")) %>%
    filter(!str_detect(parm, "b0")) %>%
    filter(!str_detect(parm, 'sig.web')) %>%
    mutate(type = "observed")
  
  all_beta <- beta_m %>% rbind(beta_r) %>%
    mutate(sig = case_when(`2.5%` < 0 & `97.5%` > 0 ~ 'nonsig',
                            TRUE ~ "sig")) %>%
    mutate(alpha = case_when(sig == "sig" ~ 1,
                             sig == "nonsig" ~ 0.2))
  
  plot <- ggplot(all_beta, aes(x = `50%`, y = parm, color = type)) +
    geom_vline(xintercept = 0, linetype = 2, alpha = 0.4) +
    geom_pointrange(aes(x = `50%`,
                        y = parm, 
                        xmin = `2.5%`,
                        xmax = `97.5%`,
                        color = type, 
                        shape = sig), 
                    show.legend = F,
                    position=position_dodge(width=0.5),
                    size = 0.4) +
    scale_color_manual(values = c(modeled_col, observed_col),
                       labels = c("Modeled", "Observed \n (all surveys)")) +
    scale_shape_manual(values = c('nonsig' = 1, 'sig' = 19)) +
    labs(x = "Covariate effect \n (Median and 95% BCI)", y = "") 
  
  return(plot)
  
}

# fish --------------------------------------------------------------------

(fish_plot <- sam_plot_fun(model_model = fish_sam, model_raw = fish_sam_raw)  +
  scale_y_discrete(labels = c("Kelp biomass", "Temperature")) + 
  labs(title = "SBC fish"))

# birds -------------------------------------------------------------------

bird_plot <- sam_plot_fun(model_model = bird_sam, model_raw = bird_sam_raw) +
  scale_y_discrete(labels = c("Temperature", "Precipitation")) + 
  labs(title = "KNZ birds")

# grasshoppers ------------------------------------------------------------

hop_plot <- sam_plot_fun(model_model = sev_sam, model_raw = sev_sam_raw) +
  scale_y_discrete(labels = c("Temperature","Precipitation", "Plant biomass")) + 
  labs(title = "SEV grasshoppers")

# Plant -------------------------------------------------------------------

plant_plot <- sam_plot_fun(model_model = plant_sam, model_raw = plant_sam_raw) +
  scale_y_discrete(labels = c("VPD","Precipitation")) + 
  labs(title = "PFNP plants") 

# combine -----------------------------------------------------------------

(effects_plots_all <- intercept_plot + (fish_plot + bird_plot) / (hop_plot + plant_plot) + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = "(",
                  tag_suffix = ")"))

# Weights plots -----------------------------------------------------------

# weight function ---------------------------------------------------------

#Do cumulative weights like figure in OG legacy paper
# grey region = raw data
# points are the modeled data results

#need to do this in a different way
#extract 1000 coda samples of the weights and then
#get a cumulative weight for each iteration and THEN
#do the cumulative weights median and credible interval calculation

#modeled data model
#raw data model
#cummulative weight variable ID
#dataset ID
#variable name
weights_fun <- function(model_m, model_r, weightID, 
                        varID, datasetID){
  
  df <- as.data.frame(model_m$quantiles) %>%
    rownames_to_column(var = "parm") %>%
    filter(str_detect(parm, weightID)) %>%
    separate(parm, into = c("parm", 'lag'),
             sep = "\\[") %>%
    mutate(lag = substr(lag, 1, (nchar(lag)-1))) %>%
    mutate(lag = as.numeric(lag)) %>%
    mutate(variable = varID) %>%
    mutate(dataset = datasetID) %>%
    rename(LCI = `2.5%`,
           median = `50%`,
           UCI = `97.5%`) %>%
    dplyr::select(lag, variable, dataset,
                  LCI, median, UCI)
  
  df2 <- as.data.frame(model_r$quantiles) %>%
    rownames_to_column(var = "parm") %>%
    filter(str_detect(parm, weightID)) %>%
    separate(parm, into = c("parm", 'lag'),
             sep = "\\[") %>%
    mutate(lag = substr(lag, 1, (nchar(lag)-1))) %>%
    mutate(lag = as.numeric(lag)) %>%
    mutate(variable = varID) %>%
    mutate(dataset = datasetID) %>%
    rename(LCI_obs = `2.5%`,
           median_obs = `50%`,
           UCI_obs = `97.5%`) %>%
    dplyr::select(lag, variable, dataset,
                  LCI_obs, median_obs, UCI_obs)
  
  df_all <- df %>%
    left_join(df2, by = c("lag", "variable", 'dataset'))
  
  plot <- ggplot(df_all) +
    geom_ribbon(aes(x = lag, ymin = LCI_obs,
                    ymax = UCI_obs),
                fill = observed_col, alpha = 0.5) +
    geom_line(aes(x = lag, y = median_obs), 
              color = observed_col, alpha = 0.5) +
    geom_pointrange(aes(x = lag, y = median,
                        ymin = LCI, 
                        ymax = UCI),
                    color = modeled_col) +    
    geom_line(aes(x = lag, y = median), 
              color = modeled_col, alpha = 0.5,
              linewidth = 0.6) +
    labs(x = "Seasons into the past", 
         y = "Cumulative seasonal weights \n posterior median and 95% BCI")
  
  return(plot)

}

(fishwts_temp <- weights_fun(model_m = fish_sam,
                             model_r = fish_sam_raw,
                             weightID = "cumm.tempwt",
                             #nlag = 6,
                             var = "Temperature",
                             datasetID = "SBC fish") +
    labs(title = "SBC fish - temperature") +
    scale_x_continuous(limits = c(1,11), 
                       breaks = seq(1, 11, by = 1),
                       labels = c(0:10)))

sevwts_ppt <- sev_sam_ppt %>%
  left_join(sev_sam_ppt_raw, by = c("lag", "variable",
                                    'dataset'))

(sevwts_pptp <- ggplot(sevwts_ppt) +
  geom_ribbon(aes(x = lag, ymin = LCI_obs,
                  ymax = UCI_obs),
              fill = observed_col, alpha = 0.5) +
  geom_line(aes(x = lag, y = median_obs), 
            color = observed_col, alpha = 0.5) +
  geom_pointrange(aes(x = lag, y = median,
                      ymin = LCI, 
                      ymax = UCI),
                  color = modeled_col) +    
  geom_line(aes(x = lag, y = median), 
            color = modeled_col, alpha = 0.5,
            linewidth = 0.6) +
  ylim(0, 1.01) +
  labs(x = "Seasons into the past", 
       y = "Cumulative seasonal weights \n posterior median and 95% BCI") +
    labs(title = "SEV grasshoppers - precipitation") +
    scale_x_continuous(limits = c(1,6), 
                       breaks = seq(1, 6, by = 1),
                       labels = c(0:5)) +
    theme(axis.title.y = element_blank()))

sevwts_temp <- sev_sam_temp %>%
  left_join(sev_sam_temp_raw, by = c("lag", "variable",
                                    'dataset'))

(sevwts_tempp <- ggplot(sevwts_temp) +
    geom_ribbon(aes(x = lag, ymin = LCI_obs,
                    ymax = UCI_obs),
                fill = observed_col, alpha = 0.5) +
    geom_line(aes(x = lag, y = median_obs), 
              color = observed_col, alpha = 0.5) +
    geom_pointrange(aes(x = lag, y = median,
                        ymin = LCI, 
                        ymax = UCI),
                    color = modeled_col) +    
    geom_line(aes(x = lag, y = median), 
              color = modeled_col, alpha = 0.5,
              linewidth = 0.6) +
    labs(x = "Seasons into the past", 
         y = "Cumulative seasonal weights \n posterior median and 95% BCI") +
    labs(title = "SEV grasshoppers - temperature") +
    scale_x_continuous(limits = c(1,6), 
                       breaks = seq(1, 6, by = 1),
                       labels = c(0:5))) +
  theme(axis.title.y = element_blank())

fishwts_temp +  sevwts_pptp + sevwts_tempp +
  plot_annotation(tag_levels = 'a',
                  tag_prefix = "(",
                  tag_suffix = ")")

