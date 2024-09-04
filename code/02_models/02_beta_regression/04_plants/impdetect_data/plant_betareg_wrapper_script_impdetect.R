
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
                          '04_plants',
                          '02_betareg',
                          'betareg_inputs',
                          "plant_betareg_input_data_list_impdetect.RDS"))

# Parameters to save ------------------------------------------------------

params <- c(#'b0.quad',
            'b0.transect',
            #'b0.plot',
            'b',
            'b0',
            'wA',
            'wB',
            #'sig.quad',
            'sig.transect',
            #'sig.plot',
            'var.process')



# JAGS model --------------------------------------------------------------

model <- here('code',
              '03_models',
              '02_beta_regression',
              '04_plants',
              'impdetect_data',
              'plant_betareg_model_impdetect.R')

Sys.time()
mod <- jagsUI::jags(data = data_list,
                    inits = NULL,
                    model.file = model,
                    parameters.to.save = params,
                    parallel = TRUE,
                    n.chains = 3,
                    n.burnin = 1000,
                    n.iter =  41000,
                    n.thin = 7,
                    DIC = TRUE)

Sys.time()

# # Check convergence -------------------------------------------------------
# 
mcmcplot(mod$samples)
# 
gelman.diag(mod$samples, multivariate = F)
# 
#
sum <- summary(mod$samples)

saveRDS(sum, here('model_summaries',
                  '04_plants',
                  'plant_betareg_model_summary_impdetect.RDS'))


# GOF ---------------------------------------------------------------------

params2 <- c("diss.rep", "resid")

mod2 <- update(mod,
               parameters.to.save = params2,
               n.iter = 1000)

modeled <- summary(mod2$samples)

observed <- readRDS(here("data_output",
                         "04_plants",
                         '02_betareg',
                         'betareg_inputs',
                         'plant_betareg_input_data_list_impdetect.RDS'))

# Pull out y and yrep -----------------------------------------------------

diss <- observed$diss

diss.rep <- as.data.frame(modeled$statistics) %>%
  rownames_to_column(var = 'parm') %>%
  filter(str_detect(parm, "diss.rep")) %>%
  rename(diss.rep.Mean = Mean,
         diss.rep.SD = SD) %>%
  cbind(diss)

m1 <- lm(diss.rep.Mean ~ diss,
         data = diss.rep)

summary(m1)


ggplot(diss.rep, aes(x = diss, y = diss.rep.Mean)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_errorbar(aes(ymin = diss.rep.Mean - diss.rep.SD,
                    ymax = diss.rep.Mean + diss.rep.SD)) +
  labs(x = 'observed', y = 'modeled', title = "PFNP plant SAM GOF")



