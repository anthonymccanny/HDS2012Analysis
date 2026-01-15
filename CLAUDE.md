# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview
This repository contains R code for re-analyzing the Housing Discrimination Study (HDS) 2012 conducted by the US Department of Housing and Urban Development. The analysis focuses specifically on sales tests (not rental tests) examining discrimination patterns.

## Key Commands

### Running R Scripts
```bash
# Main data cleaning pipeline
Rscript data_cleaning.R

# Run the main analysis
Rscript analysis.R

# Address geocoding (requires api_keys.R setup)
Rscript address_geocoding.R

# Data structure investigation
Rscript data_structure_investigation.R

# Missing data analysis
Rscript missing_data_analysis.R

# Validate SEDA school matching
Rscript add_schools_and_validate.R
```

### Working with R Interactively
```R
# Set working directory (required for all scripts)
setwd("/Users/anthony/Library/CloudStorage/OneDrive-UniversityofToronto/Research/Replication Games/HDS2012Analysis")

# Source any script
source("data_cleaning.R")
```

## Architecture and Data Flow

### Data Pipeline
1. **Raw Data Import** (`data_cleaning.R`): Reads SAS files from `Data/HDS_raw_data/`, filters to sales tests only (control numbers with "-SA-", "-SB-", "-SH-" patterns)
2. **Data Cleaning**: Complex date parsing, handling duplicates, standardizing formats
3. **Merging**: Joins assignment, TAF, sales, tester, and rhgeo data by CONTROL and TESTERID
4. **Geocoding**: Adds lat/long coordinates and census block group IDs
5. **ACS Merge**: Links to American Community Survey demographic data at tract level
6. **SEDA School Matching**: Spatially matches properties to school test scores
7. **Analysis** (`analysis.R`): Uses `lfe` package for fixed effects regression analysis

### Key Data Files
- **Raw Data**: SAS files (.sas7bdat) in `Data/HDS_raw_data/`
- **Intermediate**: Cleaned/merged CSVs in `Data/`
- **Final Output**: `Data/cleaned_hds.csv` - Complete dataset with all merges (18,990 rows)
- **Documentation**: PDFs in `Documentation/` explaining data structure and codebooks

### Critical Functions
- `import_sas()`: Reads SAS files with UTF-8 encoding, converts empty strings to NA
- `parse_date_string()`: Handles 15+ date format variations with manual corrections
- `parse_time_string()`: Standardizes appointment times from various formats

## Required R Packages
```R
packages <- c("haven", "dplyr", "readr", "stringr", "purrr", "lfe", "tidyr", "sf")
```

## API Keys Setup
For geocoding functionality:
1. Copy `api_keys_template.R` to `api_keys.R`
2. Add actual API keys (Census API is free but rate-limited)
3. Never commit `api_keys.R` to version control

## Important Context
- Only analyze sales tests (identified by control patterns "-SA-", "-SB-", "-SH-")
- Date parsing is complex due to inconsistent formats - see Methods_Appendix_Data_Cleaning.md
- Tester data has been censored for privacy (tester_censored.sas7bdat)
- Fixed effects models use CONTROL as the grouping variable

---

## SEDA School Test Score Integration (Completed)

The data cleaning pipeline now includes school test score matching using SEDA v3.0, following Christensen & Timmins (2022) methodology.

### Key Files
- **seda_merge.R**: Spatial matching functions using SABS boundaries and SEDA v3.0 data
- **add_schools_and_validate.R**: Validation script comparing against C&T replication data

### Methodology
1. Spatially match geocoded properties to SABS 2013-14 school attendance boundaries
2. Link to SEDA v3.0 test scores using NCES school identifiers (ncessch)
3. Average test scores:
   - Elementary: grades 4, 4.5, 5, 5.5
   - Middle: grades 6.5, 7, 7.5

### Data Sources
- **SEDA v3.0**: Stanford Education Data Archive (2019)
  - File: `Data/Non_HDS/SEDA_v3/seda_school_pool_cs_v30.csv`
- **SABS 2013-14**: School Attendance Boundary Survey
  - Elementary: `Data/Non_HDS/SABS/SABS_1314_SchoolLevels/SABS_1314_Primary.shp`
  - Middle: `Data/Non_HDS/SABS/SABS_1314_SchoolLevels/SABS_1314_Middle.shp`

### Validation Results
Correlations with C&T (2022) replication data:
- Middle school scores: r = 0.80-0.82 (good match)
- Elementary school scores: r = 0.56-0.61 (moderate match)

Coverage: 60.1% elementary, 61.1% middle (exceeds C&T's ~43-44%)

### Output Variables
- `mn_avg_ol_elem`: Elementary school test score (cohort-standardized mean)
- `mn_avg_ol_middle`: Middle school test score (cohort-standardized mean)

These are added to `Data/cleaned_hds.csv` by the data cleaning pipeline.
