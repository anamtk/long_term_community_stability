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

# Load Data ---------------------------------------------------------------

rhat <- readRDS(here('02_konza_birds',
                     'monsoon',
                     "MSAM",
                     'outputs',
                     'bird_MSAM_model_RE_Rhat.RDS'))

rhat2 <- readRDS(here('02_konza_birds',
                     'monsoon',
                     "MSAM",
                     'outputs',
                     'bird_MSAM_modelRE_Rhat2.RDS'))


# Graph RHat per parameter ------------------------------------------------

parm <- c("a1.Effort", "a2.Size", 'b0.star', 'deviance',
          'eps.site.star', 'eps.year.star', 'mu.b0species',
          'sig.b0species', 'sig.eps.site', 'sig.eps.year',
          'mu.a0', 'sig.a0')

rhat_graph_fun(rhat, parms = parm, rhat = 1.2) + #this is the one I'm using!!
  labs(title = "KNZ LTER bird MSAM Rhat")

ggsave(plot = last_plot(),
       filename = here("pictures",
                       "detection_models",
                       "KNZ_Rhat_graph.jpg"),
       height = 7,
       width = 8,
       units = "in")

#rhat_graph_fun(rhat2)


# Rhat summaries ----------------------------------------------------------

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

observed <- read.csv(here('02_konza_birds',
                          'data_outputs',
                          'MSAM',
                          'knz_tidy_data_for_model.csv'))

modeled <- readRDS(here('02_konza_birds',
                        'monsoon',
                        'MSAM',
                        'outputs',
                        'bird_MSAM_GOF_summary.RDS'))


# Pull out yrep -----------------------------------------------------------

#y.rep is indexed species, transects, years, replicates

yrep <- as.data.frame(modeled$statistics) %>%
  rownames_to_column(var = 'parm') %>%
  filter(str_detect(parm, "y.rep")) %>%
  separate(parm,
           into = c("SpecID", "TransID", "yrID", "REP"),
           sep = ",") %>%
  mutate(SpecID = str_sub(SpecID, 7, nchar(SpecID))) %>%
  mutate(REP = str_sub(REP, 1, (nchar(REP)-1))) %>%
  mutate(SpecID = as.numeric(SpecID),
         TransID = as.numeric(TransID),
         yrID = as.numeric(yrID),
         REP = as.numeric(REP)) %>%
  left_join(observed, by = c("SpecID", "TransID", "yrID", "REP"))


# Plot --------------------------------------------------------------------

m1 <- lm(Mean ~ NOBS,
         data = yrep)

summary(m1)

lb1 <- paste("R^2 == 0.48")

ggplot(yrep, aes(x = NOBS, y = Mean)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point()+
  annotate(geom = 'text', x = 125, y = 20, label = lb1, parse = T) +
  labs(x = "observed", y = "predicted", title = "KNZ LTER bird MSAM GOF")

ggsave(plot = last_plot(),
       filename = here("pictures",
                       "detection_models",
                       "KNZ_GOF_graph.jpg"),
       height = 4,
       width = 6,
       units = "in")

