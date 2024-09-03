
# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse", 
                  "jagsUI",
                  'rjags',
                  'mcmcplots',
                  "coda",
                  'patchwork') #mcmc output


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

data_list <- readRDS(here("data_output",
                          '01_fish',
                          '02_betareg',
                          'betareg_inputs',
                          "fish_betareg_input_data_list_impdetect.RDS"))


# Parameters to save ------------------------------------------------------

params <- c('b0.transect',
            'b',
            'b0',
            'wA',
            'wB',
            'sig.transect',
            'sig.site',
            'var.process',
            'cumm.kelpwt',
            'cumm.tempwt')



# JAGS model --------------------------------------------------------------

model <- here('01_sbc_fish',
              "code", 
              "03_sb_analyses",
              '02_SAM_modeling',
              'modeled',
              "jags",
              "fish_SAM.R")

Sys.time()
mod <- jagsUI::jags(data = data_list,
                    inits = NULL,
                    model.file = model,
                    parameters.to.save = params,
                    parallel = TRUE,
                    n.chains = 3,
                    n.burnin =1500,
                    n.iter = 5000,
                    DIC = TRUE)

Sys.time()

# Check convergence -------------------------------------------------------
# 
mcmcplot(mod$samples)
# 
gelman.diag(mod$samples, multivariate = F)

rhat <- mod$Rhat

parm <- c('b0.transect', 'b', 'wA', 'wB', 'sig.transect', 'sig.site',
          'var.process', 'deviance')
rhat_graph_fun(rhat, parms = parm, rhat = 1.1) +
  labs(title = "Fish Rhat")

# GOF ---------------------------------------------------------------------

parms2 <- c("bray.rep", "resid")

mod2 <- update(mod,
               parameters.to.save = parms2,
               n.iter = 500)

modeled <- summary(mod2$samples)

observed <- readRDS(here("data_output",
                         '01_fish',
                         '02_betareg',
                         'other_data',
                         "fish_betareg_tidydata.csv"))


bray <- observed$bray

bray.rep <- as.data.frame(modeled$statistics) %>%
  rownames_to_column(var = 'parm') %>%
  filter(str_detect(parm, "bray.rep")) %>%
  rename(bray.rep.Mean = Mean,
         bray.rep.SD = SD) %>%
  cbind(bray)

m1 <- lm(bray.rep.Mean ~ bray,
         data = bray.rep)

summary(m1)

lb1 <- paste("R^2 == 0.19")

ggplot(bray.rep, aes(x = bray, y = bray.rep.Mean)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_errorbar(aes(ymin = bray.rep.Mean - bray.rep.SD,
                    ymax = bray.rep.Mean + bray.rep.SD)) +
  annotate(geom = 'text', x = 0.8, y = 0.3, label = lb1, parse = T) +
  labs(x = 'observed', y = 'modeled', title = "SBC LTER fish SAM GOF")

