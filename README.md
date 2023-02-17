# ADI-ADRD

This repository contains scripts associated with the paper: Mobley TM, Shaw C, Hayes-Larson E, Fong J, Gilsanz P, Gee GC, Brookmeyer R, Whitmer RA, Casey JA, Mayeda ER. Neighborhood disadvantage and dementia incidence in a cohort of Asian American and non-Latino White older adults in Northern California. Alzheimers Dement. 2023 Jan;19(1):296-306. Epub 2022 Apr 6. https://pubmed.ncbi.nlm.nih.gov/35388625/

The scripts are:

1_dataset_construction.R: this script cleans variables and creates an analytic data set
2_multiple_imputations.R: this script imputes data in the analytic sample
3_table1.R: this script produces descriptive statistics in the stacked imputed data set
4_county15aa_coxph_analysis.R: this script runs nested non-Latino white and Asian American-stratified cox proportional hazards models with ADI quintiles and saves model output in an excel table
5_county15aa_asianeth_coxph_analysis.R: this script runs nested Asian ethnicity-stratified cox proportional hazards models with ADI quintiles and saves model output in an excel table
6_county15aa_coxph_sa_analysis.R: this script runs race/ethnicity-stratified cox proportional hazards models with (1) age as the timescale and (2) a binary version of the ADI and saves model output in an excel table
7_county15aa_coxph_sa_nativity_analysis.R: this script runs fully-adjusted + nativity-adjusted race/ethnicity-stratified cox proportional hazards models 
8_results.R: this script produces figures for the manucsript and supplementary materials
