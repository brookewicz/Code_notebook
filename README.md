# Code_notebook
- Code notebook for GW lab, started Jan 2025
- for misc codes - organization, qa/qc, codes for specific chapters or applicable to all chapters 

## data_summaries
### Scripts
- **ColonySummaryTables.ipynb:** 8/10/2025 summary tables of tagged colonies - bleaching, all sctld, & colonies that I sequenced 3/25 
    - list of 3/25 sequenced samples is in *inputs/genohublist_sctld2024.txt*; see CBC_metagenomics/all_sctld for analysis
    - outputs in *colony_tables/*

- **SampleSummaryStats.ipynb:** 9/2024 summary tables of sample data - immune coral totals and started but did not finish making sample summary table for each year 

- **old_Demographics.ipynb:** 9/2024 old version of demographics plots and some stats (bleaching and sctld), see Leah Harper for current sctld demographics analysis, and https://github.com/brookewicz/belize_bleaching for current bleaching demographics analysis

### Outputs
- **colony_tables/:** tagged colony summary tables (from SampleSummaries.ipynb)

    - *sctld_colonysummary.csv* - colonies (tagged in 2019 & 2022) sorted by species and health status (Resistant (did not get sctld), SCTLD (SCTLD signs), SCTLD_Recovery (SCTLD signs then recovered), SCTLD_Mortality (died from SCTLD))

    - *bks_sctld_colonysummary.csv* - colonies sequenced in 3/25 sorted by species and health status

    - *tagged_colonysummary.csv* - total tagged colonies sorted by species & transect

    - *immune_colonysummary.csv* - immune tagged colonies (RAPID subset) sorted by species & transect

    - *clb_tagged_colonysummary.csv* - tagged colonies for 2023 bleaching tracking (excludes colonies that died by 9/2023) sorted by species, transect, and bleaching status

    - *clb_immune_colonysummary.csv* - immune tagged colonies for 2023 bleaching tracking sorted by species, transect, and bleaching status

## qa_qc
### Scripts
- **organize_photos.sh:** 8/7/2025 bash script to reorganize google drive colony photos by species 

#### ColonyData
- **ColonyChecks.ipynb:** 8/10/2025 script to add a file to SCTLD_Samples repo to track which colonies' health statuses have been confirmed 

- **DataOrganization.ipynb:** 9/2024 script that changed layout of CBC_ColonyData in https://github.com/sagw/SCTLD_samples to current format

- **Merge_colony.ipynb:** 9/2024 merged 2019 & 2022 CBC colony data that LH had with SCTLD_samples CBC_ColonyData.csv

#### SampleData
- **BleachedSamples.ipynb:** 8/11/2025 working on updating sample types in SCTLD_samples from healthy to diseased 

- **sample_fixes.ipynb:** 9/2024 started fixing misc issues (format issues, inconsistencies, started adding bleaching data into sample type) in SCTLD_samples spreadsheet but never exported or finished (last checked on.. 8/11/2025)


## cbc_maps
- **cbc_maps/:** .tif files contain tagged colony map for each CBC transect
- **cbc_maps/CBC Transect Maps_color_SD.R:** 6/1/2025 R script that generates colony maps for CBC. (made by Sofia Diaz de Villegas, Fuess lab)

## inputs
- any additional input files for scripts (most inputs come from https://github.com/sagw/SCTLD_samples)

- *genohublist_sctld2024.txt* - list of sample IDs sequenced in 3/2025 for sctld metagenomics analysis. used in ColonySummaryTables.ipynb: *bks_sctld_colonysummary.csv*

