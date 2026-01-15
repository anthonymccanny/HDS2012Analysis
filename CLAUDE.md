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

---

## CURRENT WORK-IN-PROGRESS: Superfund Site Matching

### Objective
Add Superfund site counts within 5km to the HDS data to replicate Christensen & Timmins (2022) pollution analysis.

### Current Status (INCOMPLETE - NEEDS CONTINUATION)

**COMPLETED:**
1. ✅ Created `superfund_matching.R` with core functions
2. ✅ Created `validate_superfund_matching.R` for validation
3. ❌ NOT YET RUN: Validation script
4. ❌ NOT YET DONE: Integration into `data_cleaning.R`
5. ❌ NOT YET DONE: Testing full pipeline

**CRITICAL FINDINGS FROM EXPLORATION:**

The validation work identified the optimal configuration:
- **Use Superfund sites with NPL_STATUS_DATE < 2012** (n=1,675 sites)
- **Use 5 km radius** (NOT 5 miles)
- **Expected match rate: 93% exact, 99.3% within ±1** against original C&T values

Key insights:
- Initial testing on SFcount > 0 showed only 68% match (misleading)
- Testing on FULL sample (including SFcount=0) shows 93% match
- The 72 sites added in 2012-2014 were not in original C&T analysis
- Coordinates in our data are rounded to 2 decimal places (census tract centroids)

### Data Sources

**Superfund Data:**
- Path: `Data/Non_HDS/Superfund/epa-national-priorities-list-ciesin-mod-v2-2014.xls`
- Sheet: `EPA_NPL_Sites_asof_27Feb2014` (last sheet in workbook)
- Total sites: 1,747 (use 1,675 sites before 2012)
- Key columns: `LATITUDE`, `LONGITUDE`, `NPL_STATUS_DATE`, `SITE_NAME`

**Validation Data:**
- Path: `Data/HuD_Replication/Final Data Sets/recsprocessed_JPE.rds`
- Contains original C&T Superfund counts in column `SFcount`
- Has `Latitude`, `Longitude` coordinates (capitalized, rounded to 2 decimals)

**Our Data:**
- Path: `Data/cleaned_hds.csv` (final output)
- Has `lat`, `long` columns (lowercase) from geocoding step
- These come from line ~942 in `data_cleaning.R`

### Implementation Plan

**FILE: superfund_matching.R** (ALREADY CREATED)
Contains four functions:
1. `load_superfund_data()` - Loads Excel file, filters to pre-2012 sites
2. `haversine_km()` - Calculates great circle distance (Earth radius = 6371 km)
3. `count_superfund_within_radius()` - Counts sites within radius of a point
4. `add_superfund_counts()` - Adds `SFcount_5km` and `SFcount_5mi` columns to dataframe

**FILE: validate_superfund_matching.R** (ALREADY CREATED)
Compares our calculations against C&T replication data.
- Uses full replication dataset (23,655 observations)
- Reports exact match rate and within ±1 rate
- Shows discrepancy distribution
- **ACTION NEEDED: Run this script to confirm 93% match**

**FILE: data_cleaning.R** (NEEDS MODIFICATION)
Need to add integration at line ~942 (after geocoding step):

```r
# Add Superfund site counts (NEW CODE TO ADD)
cat("\n=== Adding Superfund Site Counts ===\n")
source("superfund_matching.R")
sf_data <- load_superfund_data("Data/Non_HDS/Superfund/epa-national-priorities-list-ciesin-mod-v2-2014.xls",
                                year_cutoff = 2012)
merged_data <- add_superfund_counts(merged_data,
                                     lat_col = "lat",
                                     lon_col = "long",
                                     sf_data = sf_data)
# This adds columns: SFcount_5km, SFcount_5mi
```

### Required R Packages
Need to add `readxl` to package list:
```R
packages <- c("haven", "dplyr", "readr", "stringr", "purrr", "lfe", "tidyr", "readxl")
```

### Next Steps for Continuation

1. **Run validation script:**
   ```bash
   Rscript validate_superfund_matching.R
   ```
   Expected output: ~93% exact match, ~99% within ±1

2. **Integrate into data_cleaning.R:**
   - Find line ~942 (after geocoding, before ACS merge)
   - Add the code block shown above
   - Verify `lat` and `long` column names are correct

3. **Test full pipeline:**
   ```bash
   Rscript data_cleaning.R
   ```
   - Check that `Data/cleaned_hds.csv` has new columns: `SFcount_5km`, `SFcount_5mi`
   - Verify no errors during Superfund matching step

4. **Verify results:**
   - Check summary statistics of `SFcount_5km` in final data
   - Most observations should have SFcount_5km = 0
   - Values should range from 0 to ~11

### Technical Details

**Distance Calculation:**
- Uses Haversine formula with Earth radius = 6371 km
- Formula: `c = 2 * asin(sqrt(a))` where `a = sin(dlat/2)^2 + cos(lat1)*cos(lat2)*sin(dlon/2)^2`
- Compares distance to threshold using `<=` (inclusive)

**Performance:**
- Processing ~23,000 observations takes several minutes
- Progress indicators print every 5% of observations
- Each observation compares against 1,675 Superfund sites

**Coordinate Format:**
- Our data: `lat`, `long` (lowercase, from geocoding)
- Replication data: `Latitude`, `Longitude` (capitalized)
- Both are rounded to 2 decimal places (census tract centroids)

### Validation Results Summary

Tested on n=5,000 random sample from replication data:

| Configuration | Sites | Exact | Within ±1 |
|---------------|-------|-------|-----------|
| All sites, 5km | 1,747 | 92.2% | 99.2% |
| **Pre-2012, 5km** | **1,675** | **93.0%** | **99.3%** |
| Pre-2011, 5km | 1,642 | 93.0% | 99.3% |
| Pre-2010, 5km | 1,618 | 93.5% | 99.3% |

Match rate by SFcount value:
- SFcount=0: 98.1% exact (most common case)
- SFcount=1-3: 56-73% exact (boundary cases harder to match)
- SFcount>3: Generally good match

### Troubleshooting

**If validation shows <90% match:**
- Check that `year_cutoff = 2012` is used
- Verify using correct Excel sheet: `EPA_NPL_Sites_asof_27Feb2014`
- Confirm using Haversine with radius <= 5 km (not <)

**If integration fails:**
- Check that `lat` and `long` columns exist after geocoding
- Verify `readxl` package is installed
- Make sure Excel file path is correct

**If values seem wrong:**
- Most observations should have SFcount_5km = 0
- Urban areas (especially industrial cities) will have higher counts
- Maximum values should be around 10-11

### Files Created/Modified

**New files:**
- `superfund_matching.R` - Core matching functions
- `validate_superfund_matching.R` - Validation script

**Files to modify:**
- `data_cleaning.R` - Add integration at line ~942

**No changes needed:**
- All other R scripts remain unchanged
- Final output is `Data/cleaned_hds.csv` with additional columns