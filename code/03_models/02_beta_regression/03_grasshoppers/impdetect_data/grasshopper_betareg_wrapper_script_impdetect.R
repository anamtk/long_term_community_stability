
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

data_list <- readRDS(here('data_output',
                          '03_grasshoppers',
                          '02_betareg',
                          'betareg_inputs',
                          'grasshopper_betareg_input_data_list_impdetect.RDS'))


# Parameters to save ------------------------------------------------------

params <- c('b0.web',
            'b0.transect',
            'b',
            'b0',
            'wA',
            'wB',
            'wC',
            'deltaA',
            'deltaB',
            'deltaC',
            'sig.web',
            'sig.transect',
            'var.process',
            'cumm.tempwt',
            'cumm.pptwt',
            'cumm.nppwt')



# JAGS model --------------------------------------------------------------

model <- here('code',
              '03_models',
              '02_beta_regression',
              '03_grasshoppers',
              'impdetect_data',
              'grasshopper_betareg_model_impdetect.R')

Sys.time()
mod <- jagsUI::jags(data = data_list,
                    inits = NULL,
                    model.file = model,
                    parameters.to.save = params,
                    parallel = TRUE,
                    n.chains = 3,
                    n.burnin = 1500,
                    n.iter = 2835,
                    DIC = TRUE)

Sys.time()

# # Check convergence -------------------------------------------------------
# 
mcmcplot(mod$samples)
# 
gelman.diag(mod$samples, multivariate = F)
# 
#
source(here('code',
'00_functions',
'plot_functions.R'))


rhat <- mod$Rhat

parms <- c('b0.web', 'b0.transect',
           'b', 'b0', 'wA', 'wB', "wC",
           'sig.web', 'sig.transect', 'var.process',
           'deviance')

rhat_graph_fun(rhat, parms = parms, rhat = 1.1) +
  labs(title = "SEV LTER grasshopper SAM Rhat: \n raw data")


# GOF ---------------------------------------------------------------------

parms2 <- c("beta.rep",
            'resid')

mod2 <- update(mod, 
               parameters_to_save = parms2,
               n.iter = 1335)

modeled <- summary(mod2)

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

ggplot(bray.rep, aes(x = bray, y = bray.rep.Mean)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_errorbar(aes(ymin = bray.rep.Mean - bray.rep.SD,
                    ymax = bray.rep.Mean + bray.rep.SD)) +
  labs(x = 'observed', y = 'modeled', title = "SEV LTER grasshopper SAM GOF")


