# Exploring Subnational Patterns in Male and Female Fertility across Distinct Demographic Regimes

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15047102.svg)](https://zenodo.org/records/24377315)
[![Generic badge](https://img.shields.io/badge/R-4.5.3-orange.svg)](https://cran.r-project.org/bin/macosx/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)


This repository contains code and materials to replicate the results and figures of the upcoming chapter "Exploring Subnational Patterns in Male and Female Fertility across Distinct Demographic Regimes"

### Abstract

Demographers have long studied fertility trends and their socioeconomic determinants from both macro- and micro-level perspectives. Yet two important dimensions remain comparatively underexplored: male fertility and subnational variation in reproductive behavior. In this Chapter, we examine the evolution of male and female fertility at the subnational level between 2000 and 2020 in four countries with distinct fertility regimes: Bangladesh, France, Pakistan, and Spain. Combining a wide range of demographic data sources within in a Bayesian spatiotemporal framework for indirect estimation, we estimate annual subnational total fertility rates (TFRs) for men and women. We then assess the evolution of disparities in subnational fertility using well-established measures of dispersion and examine how male-female fertility differences relate to reproductive sex ratios and spousal age gaps.  The results reveal marked differences across countries. France and Spain exhibit relatively limited subnational variation and small differences between male and female fertility. Conversely, Bangladesh and Pakistan display substantially higher spatial heterogeneity, with male fertility being more dispersed and greater across areas than female fertility. Nonetheless, fertility disparities narrow over the study period in both countries, pointing towards subnational convergence. Differences between male and female are associated with subnational imbalances in the number of men and women at peak reproductive ages and, in Bangladesh and Pakistan, with male-female gaps in the age at first union. These findings reveal that national fertility indicators may conceal substantial geographic and sex-specific heterogeneity. 

<img width="499" height="560" alt="image" src="https://github.com/user-attachments/assets/d42a0a6e-815d-46b9-ace3-7892a1dfb424" />


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


To run the code, you need to download the data folder from [Zotero](https://zenodo.org/records/24377315)

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
