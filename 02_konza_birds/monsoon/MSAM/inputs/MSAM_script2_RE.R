#Getting initials for next model run
#Ana Miller-ter Kuile
#September 15, 2023

#this script pulls out the last values from the chain with the lowest deviance
#to use as initials in a follow-up model run



# Load packages ---------------------------------------------------------------
(start.time <- Sys.time())


# Load packages,
package.list <- c("jagsUI", "coda",
                  'dplyr', 'stringr',
                  'magrittr', 'tidyr',
                  'mcmcplots','ggplot2',
                  'tibble') 


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load model --------------------------------------------------------------

mod <- readRDS(file ="/scratch/atm234/konza_birds/outputs/bird_MSAM_RE_model.RDS")

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
a1.Effort <- as.vector(samp_df2$a1.Effort)
a2.Size <- as.vector(samp_df2$a2.Size)
mu.b0species <- as.vector(samp_df2$mu.b0species)
sig.b0species <- as.vector(samp_df2$sig.b0species)
sig.eps.site <- as.vector(samp_df2$sig.eps.site)
sig.eps.year <- as.vector(samp_df2$sig.eps.year)

# Load Data ---------------------------------------------------------------

#load the formatted data for the JAGS model
data <- readRDS("/scratch/atm234/konza_birds/inputs/bird_msam_dynmultisite.RDS")

# Compile data ------------------------------------------------------------
data_list <- list(n.species = data$n.species,
                  n.transects = data$n.transects,
                  n.start = data$n.start,
                  n.end = data$n.end,
                  n.rep = data$n.rep,
                  n.years = data$n.years,
                  effort = data$effort,
                  size = data$size,
                  y = data$y,
                  n.sites = data$n.sites,
                  Year.ID = data$Year.ID,
                  Site.ID = data$Site.ID,
                  #omega prior:
                  #R = data$R,
                  #initials
                  ymax = data$ymax)#,
#omega.init = data$omega.init)

# Parameters to save ------------------------------------------------------

params <- c(
  #COMMUNITY parameters
  'b0.star',
  'eps.site.star',
  'eps.year.star',
  'eps.site',
  'eps.year',
  'a1.Effort',
  'a2.Size',
  'mu.a0',
  'sig.a0',
  'mu.b0species',
  'sig.b0species',
  'sig.eps.site',
  'sig.eps.year'
)


# INits -------------------------------------------------------------------

#we found ymax to set initials, since otherwise the model will hate us
inits <- list(list(N = data$ymax,
                   eps.site = eps.site,
                   eps.year = eps.year,
                   mu.a0 = mu.a0,
                   sig.a0 = sig.a0,
                   a1.Effort = a1.Effort,
                   a2.Size = a2.Size,
                   mu.b0species = mu.b0species,
                   sig.b0species = sig.b0species,
                   sig.eps.site  = sig.eps.site,
                   sig.eps.year = sig.eps.year),
              list(N = data$ymax,
                   eps.site = eps.site + 0.05,
                   eps.year = eps.year + 0.05,
                   mu.a0 = mu.a0 + 0.025,
                   sig.a0 = sig.a0 + 0.025,
                   a1.Effort = a1.Effort + 0.025,
                   a2.Size = a2.Size + 0.025,
                   mu.b0species = mu.b0species + 0.025,
                   sig.b0species = sig.b0species + 0.025,
                   sig.eps.site  = sig.eps.site + 0.025,
                   sig.eps.year = sig.eps.year + 0.025),
              list(N = data$ymax,
                   eps.site = eps.site - 0.05,
                   eps.year = eps.year - 0.05,
                   mu.a0 = mu.a0 - 0.025,
                   sig.a0 = sig.a0 + 0.04,
                   a1.Effort = a1.Effort - 0.025,
                   a2.Size = a2.Size - 0.025,
                   mu.b0species = mu.b0species - 0.025,
                   sig.b0species = sig.b0species + 0.04,
                   sig.eps.site  = sig.eps.site + 0.04,
                   sig.eps.year = sig.eps.year + 0.04))

# JAGS model --------------------------------------------------------------

mod2 <- jagsUI::jags(data = data_list,
                     inits = inits,
                     #inits = NULL,
                     model.file = '/scratch/atm234/konza_birds/inputs/kb_MSAM_simple.R',
                     parameters.to.save = params,
                     parallel = TRUE,
                     n.chains = 3,
                     n.thin = 10,
                     n.burnin = 5000,
                     n.iter = 25000,
                     DIC = TRUE)

#save as an R data object
saveRDS(mod2, 
        file ="/scratch/atm234/konza_birds/outputs/bird_MSAM_RE_model2.RDS")

(end.time <- Sys.time())


(tot.time <- end.time - start.time)
# Check convergence -------------------------------------------------------

params2 <- c(
  #COMMUNITY parameters
  'b0.star',
  'eps.site.star',
  'eps.year.star',
  'a1.Effort',
  'a2.Size',
  'mu.a0',
  'sig.a0',
  'mu.b0species',
  'sig.b0species',
  'sig.eps.site',
  'sig.eps.year',
  'deviance'
)

mcmcplot(mod2$samples,
         parms = params2,
         dir = "/scratch/atm234/konza_birds/outputs/mcmcplots/MSAM_RE2")

# Get RHat per parameter ------------------------------------------------

Rhat <- mod2$Rhat

saveRDS(Rhat, "/scratch/atm234/konza_birds/outputs/bird_MSAM_modelRE_Rhat2.RDS")


# Get Raftery diag --------------------------------------------------------


raf <- raftery.diag(mod2$samples)

names <- rownames(raf[[1]]$resmatrix)
ch1 <- raf[[1]]$resmatrix[,2]
ch2 <- raf[[2]]$resmatrix[,2]
ch3 <- raf[[3]]$resmatrix[,2]

raf_all <- as.data.frame(cbind(names, 
                               ch1, ch2, ch3)) %>%
  mutate(ch1 = as.numeric(ch1),
         ch2 = as.numeric(ch2),
         ch3 = as.numeric(ch3)) %>%
  pivot_longer(ch1:ch3,
               names_to = "chain",
               values_to = 'iterations') 

raf_all %>%
  summarise(iterations_90 = quantile(iterations, 
                                     probs = 0.9, 
                                     na.rm = T)/3,
            iterations_95 = quantile(iterations,
                                     probs = 0.95,
                                     na.rm = T)/3,
            max = max(iterations, 
                      na.rm = T)/3)

bu1 <- raf[[1]]$resmatrix[,1]
bu2 <- raf[[2]]$resmatrix[,1]
bu3 <- raf[[3]]$resmatrix[,1]

burn <- as.data.frame(cbind(names, bu1, bu2, bu3)) %>%
  mutate(bu1 = as.numeric(bu1),
         bu2 = as.numeric(bu2),
         bu3 = as.numeric(bu3)) %>%
  filter(!str_detect(names, "z")) %>%
  pivot_longer(bu1:bu3,
               names_to = "chain",
               values_to = 'iterations') 

burn %>%
  summarise(max(iterations, na.rm = T))