# metro-ai-readiness

This repository contains the code for the analysis of AI readiness.

## Folder Structure

1. `1_clean.R` contains code to pull data from different sources including GoogleSheet, clean the data and create normalized index.
2. `2_cluster.R` contains code to create k-means cluster. Data output writes directly to GoogleSheet using `googlesheets4` library.
2. `0_get_census.R` contains script to download data from Census using `tidycensus` package. 
3. find `data_raw` folder in Google Drive

