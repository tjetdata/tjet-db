# TJET data pipeline

This repository contains the code for processing TJET raw data and assembling datasets, including: 

1. downloading all data from the Airtable development bases (TJET MegaBase and TJET Prosecutions) 
    - pipeline/downloads.R

2. processing these data for the website and for analyses
    - pipeline/processing.R

3. accessing and transforming UCDP conflict data
    - conflicts/UCDP_lookups.R

4. accessing regime datasets and coding TJET transitions variables
    - transitions/transitions.R

5. assembling analyses datasets
    - pipeline/analysis_prep.R
  
6. translating parts of the TJET database for the website
    - pipeline/translation.R

7. writing data to the production database for the TJET website
    - pipeline/sql.R
