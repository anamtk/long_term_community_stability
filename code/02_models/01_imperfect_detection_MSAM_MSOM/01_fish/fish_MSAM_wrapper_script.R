# Running the MSAM for fish
# July 25, 2023

#this script provides code to run a model,
#check model convergence,
#update the model with informed initials for root nodes,
#re-run the model,
#update the model to track variables that inform goodness-
## of-fit metrics and plot them

# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  'rjags',
                  'mcmcplots',
                  "coda") #mcmc output


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load Data ---------------------------------------------------------------

data <- readRDS(here('data_output',
                     '01_fish',
                     '01_MSAM',
                     'MSAM_inputs',
                     'fish_msam_input_data_list.RDS'))

# Parameters to save ------------------------------------------------------

params <- c(
            #COMMUNITY parameters
            'b0.star',
            'eps.site.star',
            'eps.year.star',
            'a1.Vis',
            'a2.Size',
            'mu.a0',
            'sig.a0',
            'mu.b0species',
            'sig.b0species',
            'sig.eps.site',
            'sig.eps.year',
            'eps.site',
            'eps.year'
            )



inits <- list(list(N = data$ymax),
              list(N = data$ymax),
              list(N = data$ymax))


# JAGS model --------------------------------------------------------------

model <- here('data_output',
              '01_fish',
              '01_MSAM',
              'MSAM_inputs',
              'fish_MSAM_model.RDS')

(st.time <- Sys.time())
mod <- jagsUI::jags(data = data,
                    inits = NULL,
                    #inits = inits,
                    model.file = model,
                    parameters.to.save = params,
                    parallel = TRUE,
                    n.chains = 3,
                    #n.burnin = 2000,
                    n.iter = 1,
                    DIC = TRUE)

end.time <- Sys.time()

end.time - st.time

#I ran these on a computing cluster since they take a 
#long time to run on a personal computer. Additionally, they have to
#be re-run with initials several times for root nodes to converge

# Check convergence with Rhat ---------------------------------------------

parms <- c("a1.Vis", "a2.Size",  "eps.site.star",
           'eps.year.star', 'mu.a0', 'mu.b0species',
           'sig.a0', 'sig.b0species', 'sig.eps.site', 'sig.eps.year',
           "deviance", 'b0.star')

rhat_graph_fun(list = rhat6, parms = parms, rhat = 1.2) + #this is the one I'm using!!
  labs(title = "SBC LTER fish MSAM Rhat")


# Look at traceplots ------------------------------------------------------

mcmcplot(mod$samples)


# If didn't converge, set initials ----------------------------------------

# Get initials from previous model ----------------------------------------

#get the MCMC chains
samples <- mod$samples

#function to make each chain a dataframe
df_fun <- function(chain){
  df <- as.data.frame(chain) %>%
    rownames_to_column(var = "iteration")
  return(df)
}

#use that function on all list elements
samp_dfs <- lapply(samples, df_fun)

#make into one dataframe
samp_df <- bind_rows(samp_dfs, .id = "chain")

#get values for all parameters from the last iteration of the
#chain with the lowest deviance
samp_df2 <- samp_df %>%
  group_by(chain) %>%
  #get mean deviance by chain
  mutate(mean_dev = mean(deviance, na.rm = T)) %>%
  ungroup() %>%
  #get only the chain with the minimum average deviance
  filter(mean_dev == min(mean_dev)) %>%
  #pull out the final iteration from that chain
  filter(iteration == max(iteration)) %>%
  dplyr::select(-chain, -iteration,
                -deviance, -mean_dev) 

#for fish model root nodes:
eps.site<- samp_df2 %>%
  dplyr::select(contains('eps.site')) %>%
  dplyr::select(-contains('eps.site.star')) %>%
  dplyr::select(-contains('sig')) %>%
  pivot_longer(cols = everything(),
               names_to = "parm",
               values_to = "value") %>%
  separate(parm, 
           into = c("site", "species"),
           sep = ",") %>%
  mutate(site = str_sub(site, 10, nchar(site))) %>%
  mutate(species = str_sub(species, 1, (nchar(species)-1))) %>%
  pivot_wider(names_from = species,
              values_from = value) %>%
  dplyr::select(-site) %>%
  as.matrix()

eps.year<- samp_df2 %>%
  dplyr::select(contains('eps.year')) %>%
  dplyr::select(-contains('eps.year.star')) %>%
  dplyr::select(-contains('sig')) %>%
  pivot_longer(cols = everything(),
               names_to = "parm",
               values_to = "value") %>%
  separate(parm, 
           into = c("year", "species"),
           sep = ",") %>%
  mutate(year = str_sub(year, 10, nchar(year))) %>%
  mutate(species = str_sub(species, 1, (nchar(species)-1))) %>%
  pivot_wider(names_from = species,
              values_from = value) %>%
  dplyr::select(-year) %>%
  as.matrix()

mu.a0 <- as.vector(samp_df2$mu.a0)
sig.a0<- as.vector(samp_df2$sig.a0)
a1.Vis <- as.vector(samp_df2$a1.Vis)
a2.Size <- as.vector(samp_df2$a2.Size)
mu.b0species <- as.vector(samp_df2$mu.b0species)
sig.b0species <- as.vector(samp_df2$sig.b0species)
sig.eps.site <- as.vector(samp_df2$sig.eps.site)
sig.eps.year <- as.vector(samp_df2$sig.eps.year)


# Re-run ------------------------------------------------------------------

# INits -------------------------------------------------------------------

#we found ymax to set initials, since otherwise the model will hate us
inits <- list(list(N = data$ymax,
                   eps.site = eps.site,
                   eps.year = eps.year,
                   mu.a0 = mu.a0,
                   sig.a0 = sig.a0,
                   a1.Vis = a1.Vis,
                   a2.Size = a2.Size,
                   mu.b0species = mu.b0species,
                   sig.b0species = sig.b0species,
                   sig.eps.site  = sig.eps.site,
                   sig.eps.year = sig.eps.year),
              list(N = data$ymax,
                   eps.site = eps.site + 0.5,
                   eps.year = eps.year + 0.5,
                   mu.a0 = mu.a0 + 0.25,
                   sig.a0 = sig.a0 + 0.25,
                   a1.Vis = a1.Vis + 0.25,
                   a2.Size = a2.Size + 0.25,
                   mu.b0species = mu.b0species + 0.25,
                   sig.b0species = sig.b0species + 0.25,
                   sig.eps.site  = sig.eps.site + 0.25,
                   sig.eps.year = sig.eps.year + 0.25),
              list(N = data$ymax,
                   eps.site = eps.site - 0.5,
                   eps.year = eps.year - 0.5,
                   mu.a0 = mu.a0 - 0.25,
                   sig.a0 = sig.a0 + 0.4,
                   a1.Vis = a1.Vis - 0.25,
                   a2.Size = a2.Size - 0.25,
                   mu.b0species = mu.b0species - 0.25,
                   sig.b0species = sig.b0species + 0.4,
                   sig.eps.site  = sig.eps.site + 0.4,
                   sig.eps.year = sig.eps.year + 0.4))

# JAGS model --------------------------------------------------------------

mod2 <- jagsUI::jags(data = data_list,
                     inits = inits,
                     #inits = NULL,
                     model.file = model,
                     parameters.to.save = params,
                     parallel = TRUE,
                     n.chains = 3,
                     n.iter = 4000,
                     DIC = TRUE)

#repeat as many times as necessary for convergence


# Evaluate GOF of final model ---------------------------------------------

# Get summary of model -------------------------------------------------

params <- c("y.rep")

mod2 <- update(mod2, 
               parameters.to.save = params,
               n.iter = 335)


modeled <- summary(mod2$samples)

#load observed "y" data
observed <- read.csv(here('data_output',
                          '01_fish',
                          '01_MSAM',
                          'other_data',
                          'all_fish_data.csv'))

#y.rep is indexed species, transects, years, replicates

yrep <- as.data.frame(modeled$statistics) %>%
  rownames_to_column(var = 'parm') %>%
  filter(str_detect(parm, "y.rep")) %>%
  separate(parm,
           into = c("specID", "siteID", "yrID", "REP"),
           sep = ",") %>%
  mutate(specID = str_sub(specID, 7, nchar(specID))) %>%
  mutate(REP = str_sub(REP, 1, (nchar(REP)-1))) %>%
  mutate(specID = as.numeric(specID),
         siteID = as.numeric(siteID),
         yrID = as.numeric(yrID),
         REP = as.numeric(REP)) %>%
  left_join(observed, by = c("specID", "siteID", "yrID", "REP"))


# Plot --------------------------------------------------------------------

m1 <- lm(Mean ~ COUNT,
         data = yrep)

summary(m1)

lb1 <- paste("R^2 == 0.85")

ggplot(yrep, aes(x = COUNT, y = Mean)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  annotate(geom = 'text', x = 500, y = 200, label = lb1, parse = T) +
  labs(x = "observed", y = "predicted", title = "Fish MSAM GOF")


