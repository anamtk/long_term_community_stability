# Code from: Miller-ter Kuile et al. *Long-term community stability: accounting for imperfect detection to understand the contributions of current and past environmental conditions to community states*

## General description 

This is the code for a paper modeling long-term community stability in which we employed a two-stage modeling process to account for imperfect detection and then explored how environmental variables at multiple time scales influence community stability. We performed this modeling process on four long-term community datasets available from published sources:


| Dataset                                          | Years     | Number of sites | Number of species | Source | 
| ------------------------ | --------- | ---------- | ------------- | -------------- |
| Santa Barbara Channel LTER fish                  | 2000-2022 | 43             | 63                | [Reed and Miller 2022](https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-sbc&identifier=17&revision=37) |
| Konza Prairie LTER passerine birds               | 1981-2009 | 11             | 78                | [Boyle 2023](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-knz.26.12) |
| Sevilleta LTER grasshoppers                      | 1992-2019 | 60             | 46                | [Lightfoot 2021](https://portal.edirepository.org/nis/mapbrowse?packageid=knb-lter-sev.106.214969) |
| Petrified Forest National Park understory plants | 2007-2022 | 70             | 84                | [Swan and Ploughe 2023](https://irma.nps.gov/DataStore/Reference/Profile/2300890) |

## Folder structure

Each dataset has a similar file and folder organization structure that corresponds to the two-part modeling process. 

1. **MSAM/MSOM folders**: Any folder with the label *MSAM* or *MSOM* corresponds to the first part of the two-part modeling process in which we used multi-species (community) occupancy and abundance models to account for imperfect detection in species abundances and presences.

2. **SAM folders**: Any folder with the label *SAM* corresponds to the second part of the two-part modeling process in which we used a regression model with a stochastic antecedent modeling structure ([Ogle et al. 2015](https://onlinelibrary.wiley.com/doi/abs/10.1111/ele.12399)) to model community change based on environmental covariates.

## 1 Code folders structure

All code is written in R and JAGS languages. Code has been divided into "data prep" and "analyses" folders for each dataset. These folders are then subdivided into code scripts for each model (MSAM/MSOM and SAM). 

### 1.1 Data prep folders

#### 1.1.1 MSAM data prep folders

These folders contain code scripts to incorporate raw community data alongside any covariates to detection for each dataset exported both as a tidy dataframe:

- **[dataset] -> data_outputs -> MSAM/MSOM -> [all_data].csv**

and as a list of objects for the JAGS models 

- **[dataset] -> data_outputs -> MSAM/MSOM -> model_inputs -> [dataset_list].RDS**

#### 1.1.2 SAM data prep folders

These folders contain code scripts to incorporate exported community change metrics from the MSAM/MSAM for each dataset alongside environmental covariates exports these datasets as a tidy dataframe:

- **[dataset] -> data_outputs -> SAM -> data_prep -> [dataset_stability].csv**

### 1.2 Analysis folders

#### 1.2.1 MSAM analysis folders

These folders contain a set of scripts that:

1. Test each model prior to running on the computing cluster (`test_model.R` or `run_model.R` scripts)

2. Evaluate convergence (`convergence.R` scripts) and export figures in Supporting Information (SI Figures 1-4)

3. Evaluate goodness-of-fit (`GOF.R` scripts) and export figures in Supporting Information (SI Figures 9-12)

These folders also contains a "jags" or "models" folder that has the JAGS model for each MSAM/MSOM.

#### 1.2.2 SAM analysis folders

These folders contain a set of scripts that: 

1. Create a data list to import into JAGS (`list_prep.R` scripts). These data lists are saved in: 
    - **[dataset] -> data_outputs -> SAM -> model_inputs -> [data_list].RDS**

2. Test or run the SAM model (depending on dataset size, some of these were run on a computing cluster; `run_model.R` or `test_model.R` scripts)

3. Check convergence for each SAM model (`convergence.R` scripts; though for some datasets, convergence was evaluated in the model running process) and export figures in Supporting Information (SI Figures 5-8)

4. Evaluate goodness-of-fit of each model (`GOF.R` scripts) and export figures in Supporting Information (SI Figures 16-19)

These folders also contains a "jags" or "models" folder that has the JAGS model for each SAM regression model.

## 2 Data outputs folders structure

Data outputs are divided into intermediate objects for the MSAM/MSOM and SAM models. These include these intermediate data files:

### 2.1 MSAM/MSOM

  - tidy dataframe of all community data and detection covariates
  - inputs for MSAM/MSOM models in the form of lists
  

### 2.2 SAM 

  - tidy dataframe of all community change and environmental covariates
  - inputs for the SAM models in the form of lists
  - (Sometimes) output summaries from the SAM model if it was run on a local computer
  
### 2.3 metadata

  - data on site, year, and species IDs to link back to change metrics calculated from the MSAM/MSOM
  
## 3 Data raw folders structure

These data are previously published (see sources in above table) and too large to commit to GitHub. In all relevant scripts, we provide a link to a data object (DOI or website) where data can be downloaded and accessed. Raw data is split up between raw community data (in the main data_raw folder) and a subfolder labeled "environmental" which contains all the datasets that contain environmental covariates for each dataset.

## 4 Monsoon folders structure

Monsoon is the computing cluster at Northern Arizona University where all models were run that could not be run on a local computer. These folders are split up between MSAM/MSOM and SAM subfolders that contain subfolders "inputs" and "outputs"

### 4.1 inputs folders

These folders contain all the input scripts and data to run JAGS models on monsoon and extract community change metrics, convergence, goodness-of-fit, and model summaries

### 4.2 outputs folders 

These folders contain all the output data objects from Monsoon, including model summaries, convergence checks, and goodness-of-fit objects

## 5 05_visualizations folder

This folder compiles the results from all datasets to create the figures in the manuscript. 

### 5.1 code folder

This folder contains all the code to generate the main and supporting figures for the manuscript, including: 

1. `detection_histograms.R`: Code to create supplementary figures exploring detection and rarity in each dataset (SI Figure 13).
2. `detection_partialplots.R`: Code to create effects plots for detection covariates in the MSAM/MSOM models in Figure 2.
3. `raw_corrected_boxplots.R`: Code to create the violin plots in Figure 3 and also perform the post-hoc analysis of dataset type (observed vs. modeled) and predicted change (SI Figure 14).
4. `raw_corrected_dissimilarity.R`: Code to create time series of example site community change in Figure 4.
5. `sam_model_results.R`: Code to create effects plots and partial plots for the environmental regression (SAM) models (Figure 5, SI Figure 15)

### 5.2 viz_data folder

This folder contains data exported from elsewhere (monsoon and data_output folders, depending on the dataset) used to generate figures in the figures R scripts

## 6 pictures folder

This folder contains exported figures from R for both the main text and for the Supporting Information. It also contains a conceptual figure generated in design software to highlight the modeling process (Figure 1)

### 6.1 detection_models

This folder contains all the main text figures generated from the MSAM/MSOM model 

### 6.2 sam_models

This folder contains all the main text figures generated from the environmental regression (SAM) model

### 6.3 supplementary

This folder contains all the supplementary figures for both parts of the modeling process and any post-hoc analyses. This folder is divided between figures generated by the MSAM/MSOM and from the SAM modeling steps

