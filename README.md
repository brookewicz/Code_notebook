# Code_notebook
- Code notebook for GW lab, started Jan 2025
- for misc codes - organization, codes for specific chapters or applicable to all chapters 

## Scripts
- **BleachedSamples.ipynb:** 8/11/2025 working on updating sample types in SCTLD_samples from healthy to diseased 

- **ColonyChecks.ipynb:** 8/10/2025 script to add a file to SCTLD_Samples repo to track which colonies' health statuses have been confirmed 

- **organize_photos.sh:** 8/7/2025 bash script to reorganize google drive colony photos by species 

- **sample_fixes.ipynb:** fixing misc issues in SCTLD_samples spreadsheet (have NOT exported this yet..still working 8/11/2025)

- **SampleSummaries.ipynb:** 8/10/2025 summary tables of tagged colonies - bleaching, all sctld, & colonies that I sequenced 3/25 
    - list of 3/25 sequenced samples is in seq/lists/genohublist_sctld2024.txt; see CBC_metagenomics/all_sctld for analysis

- **cbc_maps/CBC Transect Maps_color_SD.R:** R script that generates colony maps for CBC. (made by Sofia Diaz de Villegas, Fuess lab)

## Outputs
- **cbc_maps/:** .tif files contain tagged colony map for each CBC transect
- **tables/:** tagged colony summary tables (from SampleSummaries.ipynb)
    - sctld_colonysummary.csv - colonies (tagged in 2019 & 2022) sorted by species and health status (Resistant (did not get sctld), SCTLD (SCTLD signs), SCTLD_Recovery (SCTLD signs then recovered), SCTLD_Mortality (died from SCTLD))
    - bks_sctld_colonysummary.csv - colonies sequenced in 3/25 sorted by species and health status
    - tagged_summarytable.csv - total tagged colonies sorted by species & transect
    - immune_summarytable.csv - immune tagged colonies (RAPID subset) sorted by species & transect
    - clb_taggedsummary.csv - tagged colonies for 2023 bleaching tracking (excludes colonies that died by 9/2023) sorted by species, transect, and bleaching status
    - clb_immunesummary.csv - immune tagged colonies for 2023 bleaching tracking sorted by species, transect, and bleaching status

