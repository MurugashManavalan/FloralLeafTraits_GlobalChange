# FloralLeafTraits_GlobalChange

## Introduction
This repository contains the datasets and R code used in the study:  
**“Elevated CO₂, warming, and drought differentially impact reproductive and vegetative economic traits in two grassland species.”**  

It provides both raw and processed data, as well as scripts for univariate and multivariate analyses of leaf, floral, and seed traits.

---

## Overview of Files

### Data
- `Data/Lotus-raw_data.csv` and `Data/Crepis-raw_data.csv`  
  - Preliminary datasets for univariate analysis of individual species. Trait values from leaves and flowers of the same individual were retained as separate data points. Linear models and ANOVA analyses were conducted using these datasets.  

- `Data/LotusCrepis-RDA.csv`  
  - Processed dataset for multivariate analysis, where trait values were averaged per individual.  

- `Data/LotusCrepis-RAC.csv`  
  - Processed dataset for assessing covariation of plant traits in response to global change treatments.  

### R Scripts
- `R/trait_analysis.R` – Main R script containing the analysis code.  
- `R/functions.R` – Helper script with package loading and custom functions.  

---

## Dependencies
- **R version:** ≥ 4.0  
- **Packages:**  
```r
`readr`, `lme4`, `lmerTest`, `ggplot2`, `vegan`, `dplyr`, `corrplot`, 
`igraph`, `qgraph`, `Hmisc`, `ggrepel`, `tidyr`, `plyr`, `ggeffects`, 
`car`, `reshape2`, `factoextra`, `purrr`, `here`, `devtools`, `brms`