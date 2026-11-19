# Exploring Subnational Patterns in Male and Female Fertility across Distinct Demographic Regimes

This repository contains code and materials to replicate the results and figures of the upcoming chapter "Exploring Subnational Patterns in Male and Female Fertility across Distinct Demographic Regimes"

### Replication Package

This repository includes code to replicate all figures of the chapter. 


#### Computing 

All analyses and computations were carried out on 2023 MacBook Pro with an Apple M5 Pro chip, 24 GB memory. 
All analyses were originally conducted using R version 4.5.3 (2026-03-11) and the package versions recorded in the attached 
session info at the bottom of the README. Re-running the pipeline with updated R or package versions, 
or a different seed, may yield minor numerical differences. These do not affect the paper’s results or conclusions.


#### Data 

The repository contains all data required to reproduce the analysis, except for the [DHS survey datasets](https://dhsprogram.com/), which can be obtained through the DHS Program website upon registration and approval of a data access request.

#### Code 


To run the code, you need to download the data folder.

Researchers can run the following files individually in order: 
  
- `upload_packages.R` - This file installs and uploads all the required libraries.
- `helpers.R` - This file contains all the R functions needed to conduct the data analyses.
- `nimble_models.R` - This file contains the Bayesian models written in *NIMBLE*. 
- `pop_data_input.R` - This file uploads the data on population counts by age and sex required for the Bayesian model. 
- `age_fert_schedule_est.R` - This file uploads data on the age-specific fertility rates (ASFR), national total fertility rates (TFR) and performs a singular value decomposition on SVD.
- `mortality_estimates.R` - This file calculates the person-years estimates needed as part of the input for the model. 
- `prepare_data_model_input.R` - This file creates the input data for the Bayesian models.
- `model_fit.R` -  This file fits the Bayesian models using the Markov Chain Monte Carlo (MCMC) algorithm.
- `first_union_estimates_dhs.R` -  This file calculates the sex differences in the age at first union for Bangladesh and Pakistan in 2017 using DHS surveys.
- `chapter_figures.R` - This file generates all the figures included in the chapter.


#### Authors


- [Riccardo Omenti](https://romenti.github.io/)
- [Monica Alexander](https://monicaalexander.com/)
- [Nicola Barban](https://nicolabarban.com/)