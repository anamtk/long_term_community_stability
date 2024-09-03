#Convergence check for nps MSAM
#Shelby Lamm
#October 30, 2023

#this script assesses convergence in the nps msam

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

source(here('00_functions',
            'plot_functions.R'))

# Load Data ---------------------------------------------------------------

rhat <- readRDS(here('04_nps_plants',
                     'monsoon',
                     "nps_MSAM",
                     'outputs_yrsite',
                     'nps_MSAM_model_RE_Rhat.RDS'))


# Graph RHat per parameter ------------------------------------------------

parm <- c("b0.star", "eps.site.star", "eps.year.star",
          'sig.eps.site', 'sig.eps.year', 'deviance',
          'mu.b0species', 'sig.b0species',
          'mu.a0', 'sig.a0','a1.Cover', 'a2.LifeGroup')

rhat_graph_fun(rhat, parms = parm, rhat = 1.2)+ 
  labs(title = "PFNP plant MSOM Rhat")

ggsave(plot = last_plot(),
       filename = here("pictures",
                       "detection_models",
                       "PFNP_Rhat_graph.jpg"),
       height = 7,
       width = 8,
       units = "in")


# GOF ---------------------------------------------------------------------



# Load data ---------------------------------------------------------------

observed <- read.csv(here('04_nps_plants',
                          'data_outputs',
                          'MSAM',
                          'pfnp_tidy_data_for_model.csv'))

modeled <- readRDS(here('04_nps_plants',
                        'monsoon',
                        'nps_MSAM',
                        'outputs_yrsite',
                        'plant_MSOM_GOF_summary.RDS'))



# Prep observed -----------------------------------------------------------

obs2 <- observed %>%
  dplyr::select(EventYear, Plot, Transect, Quadrat, quadnum,
                yrID, REP, SpecID, presence)

# Pull out yrep -----------------------------------------------------------

#y.rep is indexed species, years, quadrats, reps

yrep <- as.data.frame(modeled$statistics) %>%
  rownames_to_column(var = 'parm') %>%
  filter(str_detect(parm, "y.rep")) %>%
  separate(parm,
           into = c("SpecID", 'yrID', "quadnum", "REP"),
           sep = ",") %>%
  mutate(SpecID = str_sub(SpecID, 7, nchar(SpecID))) %>%
  mutate(REP = str_sub(REP, 1, (nchar(REP)-1))) %>%
  mutate(SpecID = as.numeric(SpecID),
         quadnum = as.numeric(quadnum),
         yrID = as.numeric(yrID),
         REP = as.numeric(REP)) %>%
  left_join(obs2, by = c("SpecID", "quadnum", "yrID", "REP"))


# Plot --------------------------------------------------------------------

m1 <- lm(Mean ~ presence,
         data = yrep)

summary(m1)


ggplot(yrep, aes(x = presence, y = Mean)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point()

lb1 <- paste("R^2 == 0.89")

yrep %>%
  mutate(presence = as.factor(presence)) %>%
  ggplot(aes(x = presence, y = Mean)) +
  geom_boxplot() +
  annotate(geom = 'text', x = 2.25, y = 0.25, label = lb1, parse = T) +
  labs(x = "observed", y = "predicted", title = "PFNP plant MSOM GOF")

ggsave(plot = last_plot(),
       filename = here("pictures",
                       "detection_models",
                       "PFNP_GOF_graph.jpg"),
       height = 4,
       width = 6,
       units = "in")

