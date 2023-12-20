# Load packages ---------------------------------------------------------------

# Load packages, here and tidyverse for coding ease, 
package.list <- c("here", "tidyverse") 


## Installing them if they aren't already on the computer
new.packages <- package.list[!(package.list %in% 
                                 installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## And loading them
for(i in package.list){library(i, character.only = T)}



# Extract Chlrophyl for each point ----------------------------------------


extract_chl <- function(files){
  
  # import the list of dataframes in the vector "files", 
  #now a list of dataframes, into R
  #then make them all rasters
  data_list <-  lapply(files, function(x) raster(x))
  
  # make a shortened name of the files
  files_short <- files %>%
    str_replace(".*/", "") %>% # remove the file path
    str_replace("AQUA_MODIS.", "") %>%
    str_replace(".L3m.MO.CHL.chlor_a.4km.nc", "") # remove the file extension
  
  # make the list of files have the name of their file
  names(data_list) <- files_short
  
  data_list2 <- lapply(data_list, function(x) crop(x, ROI))
  
  data_list3 <- lapply(data_list2, function(x) raster::extract(x, extract.pts, sp = T))
 
  return(data_list3)
}


# Make Chl_a values dataframes --------------------------------------------

chla_df <- function(vector){
  
  names <- c("chla", "site")
  df <- as.data.frame(vector) %>%
    dplyr::mutate(site = c("ABUR", "AHND", "AQUE", "BULL", "CARP", "GOLB",
                           "IVEE", "MOHK", "NAPL", "SCDI", "SCTW")) 
  
  colnames(df) <- names
  return(df)
  
}


# Scaled-unscaled dataframe for predictive graphing -----------------------


#get a dataframe to predict onto that includes
# the original scale of the variable 
# the scaled variable, and the "level"
# of the scaled variable. You can
# specify different "lengths", but 20 is a good
# length to choose from 

#x =the original variable
#length = the lenght of the scale (how many values to predict onto)
# name = the name of the variable that you want to attach to the final df
scale_df <- function(x, length, name){
  
  #get the sequenceo f the variable
  var1 <- seq(min(x, na.rm = T),
              max(x, na.rm = T),
              #set the length you want
              length.out = length)
  
  #scale that variable
  varS <- (var1 - mean(x, na.rm = T))/sd(x, na.rm = T)
  
  #make the OG and scaled a DF with appropriate 
  # names and a "level" for graphing later
  final_df <- var1 %>%
    cbind(varS) %>%
    as.data.frame() %>%
    dplyr::rename(!!name := ".") %>%
    dplyr::mutate(level = row_number())
  
  #get the final DF back from the function
  return(final_df)
}



# Rhat DF -----------------------------------------------------------------


rhat_df_fun <- function(list){
  
  #this creates a dtaaframe out of the Rhat values from the model
  df <- data.frame(id = names(list),
                   Rhat = unlist(lapply(list, paste, collapse = ","))) %>%
    #splits Rhat by , when that list element had more than one value
    mutate(Rhat = str_split(Rhat, ",")) %>%
    #unnests - so makes each Rhat a new row in the df
    unnest(c(Rhat)) %>%
    #make sure Rhat is a numeric
    mutate(Rhat = as.numeric(Rhat)) 
  
  return(df)
  
}
