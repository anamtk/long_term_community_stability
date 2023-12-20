
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

data_list <- readRDS(here('01_sbc_fish',
                          "data_outputs",
                          'SAM',
                          "model_inputs",
                          "bray_SAM_input_data_long.RDS"))


# Parameters to save ------------------------------------------------------

params <- c('b0.transect',
            'b',
            'wA',
            'wB',
            'sig.transect',
            'sig.site',
            'var.process')



# JAGS model --------------------------------------------------------------

model <- here('01_sbc_fish',
              "code", 
              "03_sb_analyses",
              '02_SAM_modeling',
              "jags",
              "fish_SAM.R")

Sys.time()
mod <- jagsUI::jags(data = data_list,
                    inits = NULL,
                    model.file = model,
                    parameters.to.save = params,
                    parallel = TRUE,
                    n.chains = 3,
                    n.iter = 1335,
                    DIC = TRUE)

Sys.time()

# Check convergence -------------------------------------------------------
# 
#mcmcplot(mod$samples)
# 
# gelman.diag(mod$samples, multivariate = F)
# 