
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

source(here("code",
            "00_functions",
            'plot_functions.R'))

# Load data ---------------------------------------------------------------

data_list <- readRDS(here("data_output",
                          '02_betareg',
                          "betareg_inputs",
                          "bird_betareg_input_data_list_impdetect.RDS"))


# Parameters to save ------------------------------------------------------

params <- c('b',
            'b0',
            'wA',
            'wB',
            #'wC',
            'var.process')



# JAGS model --------------------------------------------------------------

model <- here('code',
              '03_models',
              '02_beta_regression',
              '02_birds',
              'impdetect_data',
              'bird_betareg_model_impdetect.R')

Sys.time()
mod <- jagsUI::jags(data = data_list,
                    inits = NULL,
                    model.file = model,
                    parameters.to.save = params,
                    parallel = TRUE,
                    n.chains = 3,
                    n.iter = 10000,
                    DIC = TRUE)

Sys.time()

# Check convergence -------------------------------------------------------

mcmcplot(mod$samples)

gelman.diag(mod$samples, multivariate = F)

rhats <- mod$Rhat

parm <- c("b", "b0", "wA", "wB", "var.process", "deviance")

rhat_graph_fun(list =rhats, parms = parm, rhat = 1.1) +
  labs(title = "KNZ LTER bird SAM Rhat")

# Output summary ----------------------------------------------------------

sum <- summary(mod$samples)

# Update for GOF ----------------------------------------------------------
params2 <- c('bray.rep',
            'resid')

modGOF <- update(mod,
                 parameters.to.save = params2,
                 n.iter = 1335)

sumGOF <- summary(modGOF$samples)

observed <- readRDS(here('data_output',
                         '02_birds',
                         '02_betareg',
                         'betareg_inputs',
                         'bird_betareg_input_data_list_impdetect.RDS'))

bray <- observed$bray

bray.rep <- as.data.frame(sumGOF$statistics) %>%
  rownames_to_column(var = 'parm') %>%
  filter(str_detect(parm, "bray.rep")) %>%
  rename(bray.rep.Mean = Mean,
         bray.rep.SD = SD) %>%
  cbind(bray)

m1 <- lm(bray.rep.Mean ~ bray,
         data = bray.rep)

summary(m1)

lb1 <- ("R^2 == 0.38")
ggplot(bray.rep, aes(x = bray, y = bray.rep.Mean)) +
  geom_abline(slope = 1, intercept = 0) +
  geom_point() +
  geom_errorbar(aes(ymin = bray.rep.Mean - bray.rep.SD,
                    ymax = bray.rep.Mean + bray.rep.SD)) +
  annotate(geom = "text", x = 0.46, y = 0.68, label = lb1, parse = T) +
  labs(x = "observed", y = "modeled", title = "KNZ LTER bird SAM GOF")




