# Ana Miller-ter Kuile
# January 25, 2023
# fish MSAM data prep

# this script preps SBC fish data for correction model

# Load packages -----------------------------------------------------------


package.list <- c("here", "tidyverse",
                  'ggcorrplot',
                  'MASS')

## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}


# Load data ---------------------------------------------------------------

#Data can be found here:
#https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-sbc&identifier=17&revision=37

#fish survey data
fish <- read.csv(here('01_sbc_fish',
                      "data_raw",
                      "Monthly_Fish_All_Years_20221018.csv"))

#data on bdoy size distributions for species
bs <- read.csv(here('01_sbc_fish',
                    "data_raw",
                    "Annual_fish_comb_20220809.csv"))



# Explore fish ------------------------------------------------------------

colnames(fish)

colnames(bs)

#ABUR, AQUE, MOHK

#important variables
# YEAR, MONTH, SITE, TRANSECT, VIS, SP_CODE,
# SIZE, COUNT, AREA

# are all areas the same
unique(fish$AREA) #YES

unique(fish$SP_CODE)

# Clean dataset -----------------------------------------------------------

#get months of resurvey to match yearly all-site survey months
# get just our practice site
# fill visibility across a month for each site
fish1 <- fish %>%
  #get months from yearly surveys only
  filter(MONTH %in% c(7, 8, 9, 10)) %>%
  #make NA values for visibility
  mutate(VIS = case_when(VIS == -99999 ~ NA_real_,
                         TRUE ~ VIS)) %>%
  group_by(YEAR, SITE,MONTH, DATE, TRANSECT) %>%
  #fill missing VIS values for all species on a transect-year-month
  fill(VIS, .direction = "updown") %>%
  ungroup() %>%
  #set rid of NA counts
  mutate(COUNT = case_when(COUNT == -99999 ~ NA_integer_,
                           TRUE ~ COUNT)) %>%
  #ungroup that dataset
  ungroup() %>%
  unite(SITE_TRANS,
        c("SITE", "TRANSECT"),
        sep = "_",
        remove = F) %>%
  #remove two transect only surveyed for a few years
  filter(!SITE_TRANS %in% c("ABUR_3", "ABUR_2") ) %>%
  group_by(YEAR, SITE, SITE_TRANS, MONTH,DATE, TRANSECT, VIS, SP_CODE) %>%
  #get total count per species for all year-months
  summarise(COUNT = sum(COUNT, na.rm = T)) %>%
  ungroup()

#add other transects from those sites that only
#have one survey, by taking the annual data
#and then removing the ones taht are already in the 
#monthly dataset
bs2 <- bs %>%
  filter(SITE %in% c("ABUR", "AQUE", "MOHK")) %>%
  unite(SITE_TRANS,
        c("SITE", "TRANSECT"),
        sep = "_",
        remove = F) %>%
  filter(!SITE_TRANS %in% c('ABUR_1', "ABUR_2", 
                            "AQUE_1", "MOHK_1")) %>%
  dplyr::select(YEAR, SITE_TRANS, SITE, MONTH, DATE,
                TRANSECT, VIS, SP_CODE, COUNT) %>%
  group_by(YEAR, SITE_TRANS, SITE, MONTH, DATE, 
           TRANSECT, VIS, SP_CODE) %>%
  summarise(COUNT = sum(COUNT, na.rm = T)) %>%
  ungroup()

#min(bs2$VIS, na.rm = T)

bs3 <- bs %>%
  filter(!SITE %in% c("ABUR", "AQUE", "MOHK")) %>%
  unite(SITE_TRANS,
        c("SITE", "TRANSECT"),
        sep = "_",
        remove = F) %>%
  dplyr::select(YEAR, SITE_TRANS, SITE, MONTH, DATE,
                TRANSECT, VIS, SP_CODE, COUNT) %>%
  group_by(YEAR, SITE_TRANS, SITE, MONTH, DATE,
           TRANSECT, VIS, SP_CODE) %>%
  summarise(COUNT = sum(COUNT, na.rm = T)) %>%
  ungroup()

#combine these datasets
fish2 <- fish1 %>%
  rbind(bs2) %>%
  rbind(bs3) %>%
  #get rid of NA counts and set to 0
  mutate(COUNT = case_when(COUNT == -99999 ~ 0,
                            TRUE ~ COUNT)) %>%
  #filter out "unidentified" species codes
  filter(!SP_CODE %in% c("SYNG", "COTT", "BOTH",
                         "EMBI", "SCSP")) %>%
  #factor species code so we can fill in all species
  #for all surveys
  mutate(SP_CODE = as.factor(SP_CODE)) %>%
  #group by unique surveyes
  group_by(SITE_TRANS, YEAR,DATE, MONTH, SITE, TRANSECT) %>%
  #complete species list for each survey
  complete(SP_CODE) %>%
  #fill all the data that was missing in this
  fill(SITE, .direction = "updown") %>%
  fill(TRANSECT, .direction = "updown") %>%
  fill(VIS, .direction = "updown") %>%
  ungroup() %>%
  #set NA counts to 0
  mutate(COUNT = case_when(is.na(COUNT) ~ 0,
                           TRUE ~ COUNT)) %>%
  #change species code back to character
  mutate(SP_CODE = as.character(SP_CODE))# %>%
  #filter out species that are always 0,
  #see if this works to fix modeling...
  # group_by(SP_CODE) %>%
  # mutate(tot = sum(COUNT, na.rm = T)) %>%
  # filter(tot > 0) %>%
  # ungroup()


spcount <- fish2 %>%
  distinct(SITE_TRANS, YEAR, MONTH, SP_CODE) %>%
  group_by(SITE_TRANS, MONTH, YEAR) %>%
  tally()


fish2 %>%
  distinct(SITE_TRANS, YEAR, VIS) %>%
  filter(is.na(VIS)) %>%
  tally()
#14

fish2 %>%
  distinct(SITE_TRANS, YEAR, VIS) %>%
  filter(!is.na(VIS)) %>%
  tally()

#14/470 have missing vis (7%)

#there are two years with missing months from the survey
#that we want to populate with NA values for the model
# we also want to give repeat months 1:4 IDs so we can
# model them later in JAGS (jags likes numbers)
groups <- fish2 %>%
  #get the distinct combos of year and month
  distinct(SITE_TRANS, YEAR, MONTH, DATE) %>%
  #group by year
  group_by(SITE_TRANS, YEAR) %>%
  #arrange in order by month each year
  arrange(MONTH, DATE) %>%
  #give each month within a year a 1:4 count
  mutate(REP = 1:n()) %>%
  #ungroup
  ungroup() 

#combine those month IDs and missing months with the 
#fish observation dataset
fish3 <- fish2 %>%
  left_join(groups, by = c('SITE_TRANS', 'YEAR', 'MONTH', 'DATE')) %>%
  #make numreic variables for year and species
  mutate(specID = as.numeric(as.factor(SP_CODE)),
         yrID = as.numeric(as.factor(YEAR))) #%>%
  #get rid of columns we don't need for the 
  #single site 
  #dplyr::select(-SITE, -TRANSECT) 


#now combine that back wit hteh fisth dataset
fish4 <- fish3 %>%
  #remove any rows where speciesID is not defined
  filter(!is.na(specID)) %>%
  #get a site id that is numerical
  mutate(siteID = as.numeric(as.factor(SITE_TRANS))) %>%
  mutate(VIS = case_when(VIS == -99999 ~ NA_real_,
                         TRUE ~ VIS)) %>%
  #scale visibility covaraite
  mutate(VIS2 = VIS,
         VIS = scale(VIS))  

fish4 %>%
  filter(COUNT ==0) %>%
  tally()

fish4 %>%
  filter(COUNT >0) %>%
  tally()

62449/(6347+62449)

# Get covariates in order -------------------------------------------------

####Visibility covariate - site, year, visit month

#Now we need to make an array of the observed
#vis data with rows for sites, columns for years,
# and matrix elements for each replicate
nsites <- max(fish4$siteID)#get the dimension of rows 45
nyrs <- max(fish4$yrID) #get the dimension of columns 23
nreps <- max(fish4$REP) #get the dimension of matrices 4

#now, generate IDs for the for loop where 
# we will populate the matrix
yr <- fish4$yrID #get a yearID for each iteration of the loop
site <- fish4$siteID #get a site ID for each iteration of the loop
rep <- fish4$REP #get a replicate for each iteration of the loop

#make a blank array with dims of sites x years x reps
vis <- array(NA, dim = c(nsites, nyrs, nreps))

#fill taht array based on the values in those columns
# for occupancy
for(i in 1:dim(fish4)[1]){ #dim[1] = n.rows
  #using info from the dataframe on the site of row i,
  # the year of row i and the replicate of row i,
  # populate that space in the array with the column in
  # the dataframe that corresponds to the scaled vis data
  # for that sitexyearxreplicate combo
  vis[site[i], yr[i], rep[i]] <- as.numeric(fish4[i,8])
}

### BODY SIZE covariate ###
#get unique species codes to match to the body size 
# dataset
species <- unique(fish4$SP_CODE)

#Select size columns in yearly surveys - by species
sizesa <- bs %>%
  #remove all the NA size values
  filter(SIZE != -99999) %>%
  #select only the variables of interest
  dplyr::select(SIZE, SP_CODE, COUNT)

#again, with the monthly data, do the same thing
#select the size data
sizes <- fish %>%
  #remove the missing values for size
  filter(SIZE != -99999) %>%
  #select only variables of interest
  dplyr::select(SIZE, SP_CODE, COUNT) %>%
  #add in the yearly surveys (They seem like
  # they're on different days)
  bind_rows(sizesa) %>%
  #group by species ID
  group_by(SP_CODE) %>%
  #get a weighted average based on the distributions
  # of sizes of individuals observed in taht size
  # class
  summarise(AVG_SIZE = weighted.mean(SIZE, COUNT)) %>%
  #select only the species in the species codes in the dataset
  filter(SP_CODE %in% species) %>%
  #scle the variable so the model plays nice
  mutate(AVG_SIZE = scale(AVG_SIZE)) %>%
  dplyr::select(AVG_SIZE) %>%
  #make this variable a vector
  as_vector()

size_df <- fish %>%
  #remove the missing values for size
  filter(SIZE != -99999) %>%
  #select only variables of interest
  dplyr::select(SIZE, SP_CODE, COUNT) %>%
  #add in the yearly surveys (They seem like
  # they're on different days)
  bind_rows(sizesa) %>%
  #group by species ID
  group_by(SP_CODE) %>%
  #get a weighted average based on the distributions
  # of sizes of individuals observed in taht size
  # class
  summarise(AVG_SIZE = weighted.mean(SIZE, COUNT)) %>%
  #select only the species in the species codes in the dataset
  filter(SP_CODE %in% species) 

write.csv(size_df, here('01_sbc_fish',
                        'data_outputs',
                        'MSAM',
                        'all_fish_size_data.csv'))
#look at that variable's distribution
hist(sizes)


# Prep observed data ------------------------------------------------------

#Now we need to make an array of the observed
#data with rows for species, columns for sites,
# third dimension for years, and fourth dimension for reps in 
#each year
nspecs <- max(fish4$specID) #get the dimension of rows
nsites <- max(fish4$siteID) #get dimension for columns
nyrs <- max(fish4$yrID) #get the dimension of 3rd dimension
nreps <- max(fish4$REP) #get the dimension of  4th dimension

#now, generate IDs for the for loop where 
# we will populate the matrix
yr <- fish4$yrID #get a yearID for each iteration of the loop
site <- fish4$siteID #site ID for each iteration fo the loop
spec <- fish4$specID #get a species ID for each iteration of the loop
rep <- fish4$REP #get a replicate for each iteration of the loop

#make a blank array with dims of species x years x reps
y <- array(NA, dim = c(nspecs, nsites, nyrs, nreps))

#fill taht array based on the values in those columns
# for occupancy
for(i in 1:dim(fish4)[1]){ #dim[1] = n.rows
  #using info from the dataframe on the species of row i,
  #the site of row i, 
  # the year of row i and the replicate of row i,
  # populate that space in the array with the column in
  # the dataframe that corresponds to the 1-0 occupancy
  # for that speciesxyearxreplicate combo
  y[spec[i], site[i], yr[i], rep[i]] <- as.numeric(fish4[i,9])
}

#generate the z-matrix of speciesxsitexyear which assumes
# no false positives
#z matrix is speciesxsitexyear summed over all reps
Ndf <- fish4 %>%
  group_by(siteID, yrID, specID) %>%
  #get total abundance for each sitexspeciesxyear
  summarise(tot = sum(COUNT, na.rm = T)) 


nspecs <- max(Ndf$specID) #get the dimension of rows
nsites <- max(Ndf$siteID) #get dimension for columns
nyrs <- max(Ndf$yrID) #get the dimension of 3rd dimension

#now, generate IDs for the for loop where
# we will populate the matrix
yr <- Ndf$yrID #get a yearID for each iteration of the loop
site <-Ndf$siteID #site ID for each iteration fo the loop
spec <- Ndf$specID #get a species ID for each iteration of the loop

#make a blank array with dims of species x years
ymax <- array(NA, dim = c(nspecs, nsites, nyrs))

#fill taht array based on the values in those columns
# for occupancy
for(i in 1:dim(Ndf)[1]){ #dim[1] = n.rows
  #using info from the dataframe on the species of row i,
  #the site of row i,
  # the year of row i and the replicate of row i,
  # populate that space in the array with the column in
  # the dataframe that corresponds to the 1-0 occupancy
  # for that speciesxyearxreplicate combo
  ymax[spec[i], site[i], yr[i]] <- as.numeric(Ndf[i,4])
}

#set all zeros (which could be true or false) to NA
ymax[ymax == 0] <- NA

#z <- ymax

#z[z > 0] <- 1

#ymax2 <- ymax

#ymax2[is.na(ymax2)] <- 1

# Get covariates and list elements ----------------------------------------

#values for for-loops in the model
n.species <- nrow(y)
n.years <- length(unique(fish4$yrID))
n.transects <- length(unique(fish4$SITE_TRANS))
n.sites <- length(unique(fish4$SITE))

n.start <- fish4 %>%
  distinct(siteID, yrID) %>%
  group_by(siteID) %>%
  filter(yrID == min(yrID)) %>%
  arrange(siteID) %>%
  ungroup() %>%
  dplyr::select(yrID) %>%
  as_vector()

n.end <- fish4 %>%
  distinct(siteID, yrID) %>%
  group_by(siteID) %>%
  filter(yrID == max(yrID)) %>%
  arrange(siteID) %>%
  ungroup() %>%
  dplyr::select(yrID) %>%
  as_vector()

#site x year matrix
n.rep <- fish4 %>%
  distinct(siteID, yrID, REP) %>%
  group_by(siteID, yrID) %>%
  tally(name = "REP") %>%
  pivot_wider(names_from = yrID,
              values_from = REP) %>%
  column_to_rownames(var = "siteID") %>%
  dplyr::select("1", "2", "3", "4", '5',
                "6", '7', '8', '9', '10',
                '11','12','13','14','15',
                '16','17','18','19','20',
                '21', '22', '23') %>%
  as.matrix()

# random effects ----------------------------------------------------------

Site.ID <- fish4 %>%
  distinct(SITE, siteID) %>%
  mutate(SITE = as.numeric(as.factor(SITE))) %>%
  dplyr::select(SITE) %>%
  as_vector()

Year.ID <- 1:23
# Make data list to export ------------------------------------------------


#make the data list
data <- list(y = y,
             vis = vis,
             size = sizes,
             n.species = n.species,
             n.years = n.years,
             n.start = n.start,
             n.end = n.end,
             n.transects = n.transects,
             n.rep = n.rep,
             n.sites = n.sites,
             Site.ID = Site.ID,
             Year.ID = Year.ID,
             #for initials
             ymax = ymax)

#export that for using with the model
saveRDS(data, here('01_sbc_fish',
                   "data_outputs",
                   'MSAM',
                   "model_inputs",
                   "fish_msam_dynmultisiteRE.RDS"))



# Export metadata for post summaries --------------------------------------

write.csv(fish4, here('01_sbc_fish',
                      'data_outputs',
                      'MSAM',
                      'all_fish_data.csv'))

fish5 <- fish4 %>%
  ungroup() %>%
  distinct(SITE_TRANS, YEAR, siteID, yrID)

write.csv(fish5, here('01_sbc_fish',
                      "data_outputs",
                      "metadata",
                      "site_year_IDs.csv"),
          row.names = F)

fish6 <- fish4 %>%
  ungroup() %>%
  distinct(SP_CODE, specID)

write.csv(fish6, here('01_sbc_fish',
                      "data_outputs",
                      "metadata",
                      "species_IDs.csv"),
          row.names = F)

# Raw community matrix ----------------------------------------------------

#Just for visualization purposes to compare "raw" vs "corrected" bray
#we will want to get a community matrix for one site across years

#we'll do site one, and do whta folks used to do and just take the 
#maximum number of individuals observed per species per year across
#repeat surveys

matrix <- fish4 %>%
  filter(SITE_TRANS == "ABUR_1") %>%
  group_by(YEAR, specID) %>%
  summarise(COUNT = max(COUNT, na.rm = T)) %>%
  pivot_wider(names_from = YEAR,
              values_from = COUNT) %>%
  column_to_rownames(var = 'specID')

a <- matrix(NA, nrow = nrow(matrix),
            ncol = ncol(matrix))

b <- matrix(NA, nrow = nrow(matrix),
            ncol = ncol(matrix))

c <- matrix(NA, nrow = nrow(matrix),
            ncol = ncol(matrix))

for(r in 1:nrow(matrix)){
  for(t in 2:ncol(matrix)){
  a[r, t] <- min(c(matrix[r,t-1], matrix[r,t]))
  b[r,t] <- matrix[r,t-1] - a[r,t]
  c[r,t] <- matrix[r,t] - a[r,t]
  }
}

A <- colSums(a)
B <- colSums(b)
C <- colSums(c)

bray <- (B + C)/(2*A+B+C)
years <- 2002:2022

raw_bray <- as.data.frame(cbind(raw_bray_all = bray,
                                year = years))

matrix2 <- fish4 %>%
  filter(SITE_TRANS == "ABUR_1") %>%
  filter(REP == 1) %>%
  distinct(YEAR, COUNT, specID) %>%
  pivot_wider(names_from = YEAR,
              values_from = COUNT) %>%
  column_to_rownames(var = 'specID')

a2 <- matrix(NA, nrow = nrow(matrix2),
            ncol = ncol(matrix2))

b2 <- matrix(NA, nrow = nrow(matrix2),
            ncol = ncol(matrix2))

c2 <- matrix(NA, nrow = nrow(matrix2),
            ncol = ncol(matrix2))

for(r in 1:nrow(matrix2)){
  for(t in 2:ncol(matrix2)){
    a2[r, t] <- min(c(matrix2[r,t-1], matrix2[r,t]))
    b2[r,t] <- matrix2[r,t-1] - a2[r,t]
    c2[r,t] <- matrix2[r,t] - a2[r,t]
  }
}

A2 <- colSums(a2)
B2 <- colSums(b2)
C2 <- colSums(c2)

bray2 <- (B2 + C2)/(2*A2+B2+C2)

raw_bray2 <- as.data.frame(cbind(raw_bray_one = bray2,
                                year = years))

raw_bray_all <- raw_bray %>%
  left_join(raw_bray2, by = "year")

saveRDS(raw_bray_all, here("05_visualizations",
                       "viz_data",
                       "sbc_ABUR1_raw_bray.RDS"))


# Uncorrected bray for all sites-years ------------------------------------

#pulling out and calculating bray-curtis for all 
#communities so we can compare to the "corrected" version
#after the model

diss_fun <- function(site){
  
  matrix <- fish4 %>%
    filter(siteID == site) %>%
    group_by(YEAR, specID) %>%
    summarise(COUNT = max(COUNT, na.rm = T)) %>%
    pivot_wider(names_from = YEAR,
                values_from = COUNT) %>%
    column_to_rownames(var = 'specID')
  
  a <- matrix(NA, nrow = nrow(matrix),
              ncol = ncol(matrix))
  
  b <- matrix(NA, nrow = nrow(matrix),
              ncol = ncol(matrix))
  
  c <- matrix(NA, nrow = nrow(matrix),
              ncol = ncol(matrix))
  
  for(r in 1:nrow(matrix)){
    for(t in 2:ncol(matrix)){
      a[r, t] <- min(c(matrix[r,t-1], matrix[r,t]))
      b[r,t] <- matrix[r,t-1] - a[r,t]
      c[r,t] <- matrix[r,t] - a[r,t]
    }
  }
  
  A <- colSums(a)
  B <- colSums(b)
  C <- colSums(c)
  
  bray <- (B + C)/(2*A+B+C)
  
  bray_df <- fish4 %>%
    filter(siteID == site) %>%
    distinct(yrID, siteID) %>%
    cbind(bray) %>%
    mutate(type = "observed_all")
  
  matrix2 <- fish4 %>%
    filter(siteID == site) %>%
    filter(REP == 1) %>%
    distinct(YEAR, COUNT, specID) %>%
    pivot_wider(names_from = YEAR,
                values_from = COUNT) %>%
    column_to_rownames(var = 'specID')
  
  a2 <- matrix(NA, nrow = nrow(matrix2),
              ncol = ncol(matrix2))
  
  b2 <- matrix(NA, nrow = nrow(matrix2),
              ncol = ncol(matrix2))
  
  c2 <- matrix(NA, nrow = nrow(matrix2),
              ncol = ncol(matrix2))
  
  for(r in 1:nrow(matrix2)){
    for(t in 2:ncol(matrix2)){
      a2[r, t] <- min(c(matrix2[r,t-1], matrix2[r,t]))
      b2[r,t] <- matrix2[r,t-1] - a2[r,t]
      c2[r,t] <- matrix2[r,t] - a2[r,t]
    }
  }
  
  A2 <- colSums(a2)
  B2 <- colSums(b2)
  C2 <- colSums(c2)
  
  bray2 <- (B2 + C2)/(2*A2+B2+C2)

  bray_df2 <- fish4 %>%
    filter(siteID == site) %>%
    distinct(yrID, siteID) %>%
    cbind(bray2) %>%
    rename(bray = bray2) %>%
    mutate(type = "observed_one")
  
  bray_df_all <- bray_df %>%
    rbind(bray_df2)
  
  return(bray_df_all)
}

sites <- unique(fish4$siteID)
results <- lapply(sites, FUN = diss_fun)

results_df <- do.call(rbind, results)

saveRDS(results_df, here('05_visualizations',
               'viz_data',
               'sbc_observed_bray.RDS'))


# Summary stats -----------------------------------------------------------

colnames(fish4)

fish4 %>%
  separate(col = SITE_TRANS, 
           into = c("site", "transect"),
           sep= "_") %>%
  distinct(site) 

bs %>%
  distinct(SCIENTIFIC_NAME, COMMON_NAME)

ad <- bs %>%
  distinct(SP_CODE, SCIENTIFIC_NAME, COMMON_NAME)

t <- fish4 %>%
  distinct(SP_CODE) %>%
  full_join(ad, by = "SP_CODE")
