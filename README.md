# Early-Life Environmental Conditions Shape Demographic Responses to Climate Change in Male Antarctic Fur Seals

## Overview

This repository contains the R code used to analyse male recruitment patterns in Antarctic fur seals, including the effects of early-life environmental conditions and climate variability (SAM) on male counts and recruitment.

The input data for this analysis were produced by a recapture analysis maintained in a [separate repository](https://github.com/apaijmans/AFS_recaptures/). All input data are available via Zenodo: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.19680323.svg)](https://doi.org/10.5281/zenodo.19680323) 

The Zenodo repository contains the following input files:
- `all_msat_genotypes_uniqueID.xlsx` — microsatellite genotypes with duplicate individuals flagged
- `male_pups_pheno_data.xlsx` — fitness data for male pups
- `sMLH_msats_male_pups.xlsx` — standardised multilocus heterozygosity (sMLH) values calculated from the microsatellite data

## Requirements

All scripts can be run on a standard desktop machine in R.

## Usage

Download the complete repository and run the scripts in the `Rcode` folder in order (1a → 1b → 2 → 3a → 3b → 4).

## Scripts

| Script | Description |
|---|---|
| `1a_wrangle_obs_data.R` | Imports observational location data for adult males (observed-only and tissue-sampled) to calculate annual male counts. Also adds the SAM index. Outputs `n_observed_males.xlsx`. |
| `1b_wrangle_data.R` | Three-part wrangling script. (1) Uses genetic recapture data to calculate annual male counts (annual number of genetically sampled unique males, `total_n_males.xlsx`). (2) Calculates individual-level variables (first/last recapture year, age at recapture, skipping behaviour) and outputs `all_males_wrangled.xlsx`. (3) Combines natal recaptures with non-recaptured male pups, adds fitness data, sMLH and SAM values, and produces Fig. 1d. Outputs `males_sMLH_fitness.xlsx`. |
| `2_explore_data_n_males.R` | Generates panels for Fig. 1: annual male counts (Fig. 1c) and number of recaptures (Fig. 1b). Final figure aesthetics are handled in `4_figs.R`. |
| `3a_stats_n_males.R` | Runs GLMs testing the effects of year and SAM on male counts (modelled separately due to collinearity). Produces Tables 1a and 1b. |
| `3b_stats_recruitment.R` | Runs GLMs testing the effects of various predictors on male recruitment, including a temporal model (year) and a full model (SAM and other predictors). Also tests for a cohort effect on recruitment age before and after 2009. Produces Tables 2a and 2b and all panels for Fig. 2. Final figure aesthetics are handled in `4_figs.R`. |
| `4_figs.R` | Applies final aesthetics to figure panels and combines them to produce Figs. 1 and 2. |
```
