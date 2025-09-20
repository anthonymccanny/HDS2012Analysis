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
4. **Analysis** (`analysis.R`): Uses `lfe` package for fixed effects regression analysis

### Key Data Files
- **Raw Data**: SAS files (.sas7bdat) in `Data/HDS_raw_data/`
- **Intermediate**: Cleaned/merged CSVs in `Data/`
- **Documentation**: PDFs in `Documentation/` explaining data structure and codebooks

### Critical Functions
- `import_sas()`: Reads SAS files with UTF-8 encoding, converts empty strings to NA
- `parse_date_string()`: Handles 15+ date format variations with manual corrections
- `parse_time_string()`: Standardizes appointment times from various formats

## Required R Packages
```R
packages <- c("haven", "dplyr", "readr", "stringr", "purrr", "lfe", "tidyr")
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