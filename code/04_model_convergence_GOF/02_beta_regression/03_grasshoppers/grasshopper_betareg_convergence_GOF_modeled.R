#Convergence check for fish MSAM
# Ana Miller-ter Kuile
#Sepptember 7, 2023

#this script assesses convergence in the fish msam

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse",
                  'data.table') 


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}

source(here("00_functions",
            'plot_functions.R'))

# Load Data ---------------------------------------------------------------

rhat <- readRDS(here('03_sev_grasshoppers',
                     'monsoon',
                     'SAM',
                     'outputs',
                     'sev_SAM_model_Rhat_notrans.RDS'))

rhat2 <- readRDS(here('03_sev_grasshoppers',
                     'monsoon',
                     'SAM',
                     'outputs',
                     'sev_SAM_model_Rhat2.RDS'))

rhat3 <- readRDS(here('03_sev_grasshoppers',
                        'monsoon',
                        'SAM',
                        'outputs',
                        'sev_SAM_model_Rhat3_notrans.RDS'))


rhat4 <- readRDS(here('03_sev_grasshoppers',
                      'monsoon',
                      'SAM',
                      'outputs',
                      'sev_SAM_model_Rhat4.RDS'))

# Graph RHat per parameter ------------------------------------------------

parms <- c('b0.web', 'b0.transect',
           'b', 'b0', 'wA', 'wB', "wC",
           'sig.web', 'sig.transect', 'var.process',
           'deviance')

rhat_graph_fun(rhat3, parms = parms, rhat = 1.1)

rhat_graph_fun(rhat3, parms = parms, rhat = 1.1 ) +
  labs(title = "SEV LTER grasshopper SAM Rhat: \n modeled data")

ggsave(plot = last_plot(),
       filename = here("pictures",
                       "supplementary",
                       'SAM',
                       "SEV_SAM_Rhat_graph.jpg"),
       height = 4,
       width = 6,
       units = "in")


# GOF ---------------------------------------------------------------------

# Load data ---------------------------------------------------------------


observed <- readRDS(here('03_sev_grasshoppers',
                         "data_outputs",
                         'SAM',
                         "model_inputs",
                         "sev_bray_SAM_input_data_notrans.RDS"))

modeled <- readRDS(here('03_sev_grasshoppers',
                        "monsoon",
                        'SAM',
                        "outputs",
                        "hopper_SAM_GOF_summary.RDS"))


# Pull out y and yrep -----------------------------------------------------

bray <- observed$bray

bray.rep <- as.data.frame(modeled$statistics) %>%
  rownames_to_column(var = 'parm') %>%
  filter(str_detect(parm, "beta.rep")) %>%
  rename(bray.rep.Mean = Mean,
         bray.rep.SD = SD) %>%
  cbind(bray)

m1 <- lm(bray.rep.Mean ~ bray,
         data = bray.rep)

summary(m1)

lb1 <- paste("R^2 == 0.43")

ggplot(bray.rep, aes(x = bray, y = bray.rep.Mean)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_errorbar(aes(ymin = bray.rep.Mean - bray.rep.SD,
                    ymax = bray.rep.Mean + bray.rep.SD)) +
  annotate(geom = 'text', x = 0.68, y = 0.3, label = lb1, parse = T) +
  labs(x = 'observed', y = 'modeled', title = "SEV LTER grasshopper SAM GOF")


ggsave(plot = last_plot(),
       filename = here("pictures",
                       "supplementary",
                       'SAM',
                       "SEV_SAM_GOF_graph.jpg"),
       height = 4,
       width = 6,
       units = "in")



