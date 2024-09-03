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

source(here('00_functions',
            'plot_functions.R'))

source(here('00_functions',
            'tidy_functions.R'))

# Load Data ---------------------------------------------------------------

rhat <- readRDS(here('03_sev_grasshoppers',
                     'monsoon',
                     "MSAM",
                     'outputs',
                     'sev_MSAMnocovRE_model_Rhat.RDS'))

rhat2 <- readRDS(here('03_sev_grasshoppers',
                      'monsoon',
                      "MSAM",
                      'outputs',
                      'sev_MSAM_modelRE_Rhat2.RDS'))

# Graph RHat per parameter ------------------------------------------------

parm <- c("b0.star", 'eps.site.star', 'eps.year.star', 'p.mean',
          'sig.lp', 'mu.b0species','sig.b0species',
          'sig.eps.year', 'sig.eps.site')
rhat_graph_fun(list = rhat, parms = parm, rhat = 1.2)  + #this is the one I'm using!!
  labs(title = "SEV LTER grasshopper MSAM Rhat")

ggsave(plot = last_plot(),
       filename = here("pictures",
                       "detection_models",
                       "SEV_Rhat_graph.jpg"),
       height = 7,
       width = 8,
       units = "in")


#rhat_graph_fun(rhat2)


# Get rhat summaries ------------------------------------------------------



rhats <- rhat_df_fun(rhat)

#remove ones we don't need to track
rhats2 <- rhats %>%
  filter(!id %in% c("eps.year", "eps.site"))

a <- rhats2 %>%
  filter(Rhat > 1.2) %>%
  tally()

b <- rhats2 %>%
  filter(Rhat <= 1.2) %>%
  tally()

1-(a/(a+b))

rhats_root <- rhats2 %>%
  filter(!id == "b0.star")

c <- rhats_root %>%
  filter(Rhat > 1.2) %>%
  tally()

d <- rhats_root %>%
  filter(Rhat <= 1.2) %>%
  tally()

c/(c+d)


# GOF ---------------------------------------------------------------------

# Load data ---------------------------------------------------------------

observed <- read.csv(here('03_sev_grasshoppers',
                          'data_outputs',
                          'MSAM',
                          'sev_tidy_data_for_model.csv'))

modeled <- readRDS(here('03_sev_grasshoppers',
                        'monsoon',
                        'MSAM',
                        'outputs',
                        'sev_MSAM_GOF_summary.RDS'))


# Pull out yrep -----------------------------------------------------------

#y.rep is indexed species, transects, years, replicates

yrep <- as.data.frame(modeled$statistics) %>%
  rownames_to_column(var = 'parm') %>%
  filter(str_detect(parm, "y.rep")) %>%
  separate(parm,
           into = c("speciesID", "siteID", "yrID", "rep"),
           sep = ",") %>%
  mutate(speciesID = str_sub(speciesID, 7, nchar(speciesID))) %>%
  mutate(rep = str_sub(rep, 1, (nchar(rep)-1))) %>%
  mutate(speciesID = as.numeric(speciesID),
         siteID = as.numeric(siteID),
         yrID = as.numeric(yrID),
         rep = as.numeric(rep)) %>%
  left_join(observed, by = c("speciesID", "siteID", "yrID", "rep"))


# Plot --------------------------------------------------------------------

m1 <- lm(Mean ~ CNT,
         data = yrep)

summary(m1)

lb1 <- paste("R^2 == 0.45")

ggplot(yrep, aes(x = CNT, y = Mean)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  annotate(geom = 'text', x = 20, y = 2.5, label = lb1, parse = T) +
  labs(x = "observed", y = "predicted", title = "SEV LTER grasshopper MSAM GOF")

ggsave(plot = last_plot(),
       filename = here("pictures",
                       "detection_models",
                       "SEV_GOF_graph.jpg"),
       height = 4,
       width = 6,
       units = "in")



