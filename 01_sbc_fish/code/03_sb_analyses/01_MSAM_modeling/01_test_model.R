# Running the MSAM for fish
# Ana Miller-ter Kuile
# July 25, 2023

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

data <- readRDS(here('01_sbc_fish',
                     "data_outputs",
                     'MSAM',
                     "model_inputs",
                     "fish_msam_dynmultisiteRE.RDS"))

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

model <- here("01_sbc_fish",
              "code", 
              "03_sb_analyses",
              '01_MSAM_modeling',
              "models",
              "MSAM_simple_siteyearRE.R")

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
# 
#mcmcplot(mod$samples)
# 
#gelman.diag(mod$samples)
# 
