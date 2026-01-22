# Non-HDS and HuD_Replication Data Assembly

This document describes how to reconstruct the external (Non-HDS) data used by this repository and the Christensen & Timmins replication inputs under `Data/HuD_Replication`. It is intended for users who cannot receive the full data bundle in a replication package.

If you are missing any datasets not publicly redistributable, follow the download instructions below and place the files in the specified paths. If any source URL or access requirement has changed, please contact the project maintainer.

---

## A. Directory structure (expected)

```
Data/
  Non_HDS/
    NNCS/
    PM2_5/
    RSEI/
    SABS/
    SEDA_v3/
    SEDA_v5/
    Superfund/
  HuD_Replication/
    Appendix/
    Figures/
    Final Data Sets/
    images/
    readme.txt
```

The Non-HDS subfolders should contain the following (illustrative, not exhaustive):

```
Data/Non_HDS/
  PM2_5/
    2012.h5
    README.md
  RSEI/
    aggmicro2022_2020.csv
    (any intermediate files can live here)
  SABS/
    SABS_1516_SchoolLevels/
      SABS_1516_Primary.shp + sidecars
      SABS_1516_Middle.shp + sidecars
    SABS_1516_Documentation.pdf
    (older SABS vintages optional)
  SEDA_v3/
    seda_school_pool_cs_v30.csv
    seda_school_pool_gcs_v30.csv
  SEDA_v5/
    (optional reference files)
  Superfund/
    epa-national-priorities-list-ciesin-mod-v2-2014.xls
```

---

## B. Non-HDS datasets

### B.1 ACS (American Community Survey, 2008–2012 5-year)

**How it is used**
- Pulled on the fly by `acs_merging.R` via `tidycensus` using tract GEOIDs from geocoded HDS data.
- Requires a Census API key.

**Step-by-step**
1. Go to the Census API key signup page and request a key: https://api.census.gov/data/key_signup.html
2. Copy `api_keys_template.R` to `api_keys.R` and fill in your key.
3. Run `data_cleaning.R` (or call `merge_acs()`) to fetch ACS data on demand.

**Files in repo**
- No static ACS files are stored; this merge is fully reproducible via API.

---

### B.2 Superfund sites (EPA National Priorities List)

**How it is used**
- `superfund_merging.R` loads the EPA NPL Excel file and counts sites within 5 km of each property.

**Required file (path)**
- `Data/Non_HDS/Superfund/epa-national-priorities-list-ciesin-mod-v2-2014.xls`

**Step-by-step**
1. Go to the dataset DOI page (this is the authoritative reference):
   - https://doi.org/10.7910/DVN/CMW1Z0
2. Click **Access Dataset** → **Download Zip**.
3. Inside the ZIP, locate the Excel file:
   - `epa-national-priorities-list-ciesin-mod-v2-2014.xls`
4. Place it at:
   - `Data/Non_HDS/Superfund/epa-national-priorities-list-ciesin-mod-v2-2014.xls`

**Sharing note:** this Superfund dataset is **publicly sharable** and can be included in the replication package.

---

### B.3 School attendance boundaries (SABS)

**How it is used**
- `school_score_merging.R` uses 2015–16 school attendance boundary shapefiles for primary/middle school matching.

**Required directories (paths)**
- `Data/Non_HDS/SABS/SABS_1516_SchoolLevels/`
  - `SABS_1516_Primary.shp` (+ associated shapefile sidecars)
  - `SABS_1516_Middle.shp` (+ associated shapefile sidecars)
- Documentation PDFs are optional:
  - `Data/Non_HDS/SABS/SABS_1516_Documentation.pdf`

**Step-by-step**
1. Download SABS 2015–16 school attendance boundaries from NCES:
   - Direct ZIP: https://nces.ed.gov/programs/edge/data/SABS_1516_SchoolLevels.zip
   - Or: https://nces.ed.gov/programs/edge/sabs (download “School Level Shapefile”)
2. Extract the ZIP so that these shapefiles exist:
   - `Data/Non_HDS/SABS/SABS_1516_SchoolLevels/SABS_1516_Primary.shp`
   - `Data/Non_HDS/SABS/SABS_1516_SchoolLevels/SABS_1516_Middle.shp`
3. Ensure all shapefile sidecar files (`.shx`, `.dbf`, `.prj`, etc.) are kept alongside each `.shp`.

**Sharing note:** SABS is listed on Data.gov (e.g., https://catalog.data.gov/dataset/school-attendance-boundary-survey-2013-2014-c6cbf) and is **publicly sharable**.

---

### B.4 School test scores (SEDA v3.0)

**How it is used**
- `school_score_merging.R` reads SEDA v3.0 school-level pooled scores and computes elementary/middle averages.

**Required files (paths)**
- `Data/Non_HDS/SEDA_v3/seda_school_pool_cs_v30.csv`
- `Data/Non_HDS/SEDA_v3/seda_school_pool_gcs_v30.csv` (not used in current matching but kept for reference)

**Step-by-step**
1. Go to the SEDA downloads page:
   - https://edopportunity.org/opportunity/data/downloads/
2. Download the **Excel** version of:
   - `seda_school_pool_cs_v30` (this is the file used by the pipeline)
3. Save/export it as CSV and place it at:
   - `Data/Non_HDS/SEDA_v3/seda_school_pool_cs_v30.csv`
4. (Optional) If you also download the GCS version, place it at:
   - `Data/Non_HDS/SEDA_v3/seda_school_pool_gcs_v30.csv`

**Sharing note:** SEDA does not appear to be explicitly designated as publicly redistributable here; do **not** include the raw files in the replication package unless the license terms explicitly allow redistribution.

---

### B.5 SEDA v5.0 (optional / not used by current pipeline)

**Status**
- The `Data/Non_HDS/SEDA_v5/` directory contains newer SEDA v5.0 files for potential future use. These are not currently required by `data_cleaning.R`.

---

### B.6 PM2.5, RSEI, NNCS (not yet used in pipeline)

**Status**
- These folders currently exist but are not yet integrated into the standardized merge pipeline.
- If/when these datasets are added, this document will be updated with download sources and placement instructions.

**PM2.5 (2012, modeled)**
- Source: Zenodo record `2616769` (see `Data/Non_HDS/PM2_5/README.md`).
- Expected file: `Data/Non_HDS/PM2_5/2012.h5`.
- Note: values are stored as 100× their true value (divide by 100 when interpreting).

**Step-by-step**
1. Go to: https://zenodo.org/records/2616769
2. Download the data archive for Meng et al. (2019) PM2.5 (1981–2016).
3. Extract the 2012 file from the archive:
   - File name in archive: `2012.h5`
4. Place it at:
   - `Data/Non_HDS/PM2_5/2012.h5`
 
**Sharing note:** this PM2.5 dataset is **publicly sharable** and can be included in the replication package.

**New PM2.5 Source**

Christensen and Timmins (2022) source their PM2.5 concentrations from:

Donkelaar, Aaron van, Randall V. Martin, Chi Li, and Richard T. Burnett. 2019. “Regional Estimates of Chemical Composition of Fine Particulate Matter Using a Combined Geoscience-Statistical Method with Information from Satellites, Models, and Monitors.” Environmental Science & Technology 53 (5): 2595–611. https://doi.org/10.1021/acs.est.8b06392.

The URL listed in this paper as the source of the full PM2.5 concentration datasets (http://fizz.phys.dal.ca/~atmos/martin/?page_id=140) is no longer available at the time of writing. However, one of the authors has made their satellite-derived PM2.5 concentration datasets available at https://sites.wustl.edu/acag/satellites/surface-pm2-5-archive/#V5.NA.04.02. We pull the V4.NA.02 version of the datasets, which are indicated by van Donkelaar et al. (2019) and at the archived version of http://fizz.phys.dal.ca/~atmos/martin/?page_id=140. We also pull datasets derived by the same authors with more up-to-date methodology V5.NA.04.02 to build the best estimates of pollution by neighbourhood. 

Sources for the files in each folder are here:

V4.NA.02:
GWRwSPEC_PM25_NA_201201_201212-RH35-NoNegs.asc and GWRwSPEC_PM25_NA_201201_201212-RH35-NoNegs.prj: https://wustl.app.box.com/s/wk3144jc6xfy6ujfvyv5m2yfk33nz2nn/file/802740439614
GWRwSPEC_PM25_NA_201201_201212-RH35.nc: https://wustl.app.box.com/s/wk3144jc6xfy6ujfvyv5m2yfk33nz2nn/file/802736943926
GWRwSPEC.HEI.ELEVandURB_PM25_NA_201201_201212-RH35.nc: https://wustl.app.box.com/s/3jxywsp2hnr4nojztznmbn1o9fb9ilvo/file/802655356099

Reference: van Donkelaar, A., R. V. Martin, et al. (2019). Regional Estimates of Chemical Composition of Fine Particulate Matter using a Combined Geoscience-Statistical Method with Information from Satellites, Models, and Monitors. Environmental Science & Technology, 2019, doi:10.1021/acs.est.8b06392.

V5.NA.04.02:
V5NA04.02.HybridPM25.xNorthAmerica.2012001-2012364.asc and V5NA04.02.HybridPM25.xNorthAmerica.2012001-2012364.prj: https://wustl.app.box.com/s/ssqbw4f7ys2cwmnrv34xnyv739450slb/file/1585219677266
V5NA04.02.HybridPM25EFull.xNorthAmerica.2012001-2012364.nc: https://wustl.app.box.com/s/tfyt4uyuzbt4hbnw7bhos16aep9b5u7g/file/1718698899868
Reference: van Donkelaar, A., R. V. Martin, B. Ford, C. Li, A. J. Pappin, S. Shen, and D. Zhang, North American Fine Particulate Matter Chemical Composition for 2000–2022 from Satellites, Models, and Monitors: The Changing Contribution of Wildfires., ACS ES&T Air, doi: 10.1021/acs.est.0c01764, 2024.




**RSEI (Risk-Screening Environmental Indicators, 2022)**
- Dataset: EPA RSEI aggregated grid cell data and 810m standard grid shapefiles.
- Source: EPA FTP (`https://gaftp.epa.gov/rsei/`).
- Expected files:
   - `Data/Non_HDS/RSEI/aggmicro2022_2020.csv`
   - `Data/Non_HDS/RSEI/poly_gc14_conus_810m_bottom.shp` (+ associated shapefile sidecars)
   - `Data/Non_HDS/RSEI/poly_gc14_conus_810m_top.shp` (+ associated shapefile sidecars)

**Step-by-step**
1. Download the aggregated grid cell data:
    - Go to: `https://gaftp.epa.gov/rsei/Current_Version/V2312_RY2022/Aggregated_Grid_Cell/`
    - Download: `aggmicro2022_2020.csv.gz`
    - Decompress: `gunzip aggmicro2022_2020.csv.gz`
    - Place at: `Data/Non_HDS/RSEI/aggmicro2022_2020.csv`
2. Download the 810m standard grid shapefiles:
    - Go to: `https://gaftp.epa.gov/rsei/Shapefiles/810m_Standard_Grid_Shapefiles/`
    - Download the following files:
       - `poly_gc14_conus_810m_bottom.shp` and sidecars (`.shx`, `.dbf`, `.prj`)
       - `poly_gc14_conus_810m_top.shp` and sidecars (`.shx`, `.dbf`, `.prj`)
    - Place them at: `Data/Non_HDS/RSEI/`
3. Ensure all shapefile sidecar files are kept alongside each `.shp`.

**Notes on handling `.gz` files**
- On Linux/macOS, decompress with `gunzip aggmicro2022_2020.csv.gz`.
- On Windows, use 7-Zip or a similar utility to extract the `.csv` from the `.gz` archive.

**Sharing note:** RSEI data is **publicly sharable** and can be included in the replication package.

<!-- **NNCS (Wave 2, 1999–2013)**
- Dataset: Krivo, Lauren J., Lyons, Christopher J., and Velez, Maria B. National Neighborhood Crime Study, Wave 2 (NNCS2), [United States], 1999–2013.
- Source: ICPSR (2023-01-25), DOI `10.3886/ICPSR38483.v2`.
- Download and expand directly into `Data/Non_HDS/NNCS/`.

**Step-by-step**
1. Go to the ICPSR record:
   - `https://doi.org/10.3886/ICPSR38483.v2`
2. Download the NNCS2 Wave 2 dataset (ICPSR 38483).
3. Extract the downloaded archive directly into:
   - `Data/Non_HDS/NNCS/`

**Source citation**
Krivo, Lauren J., Lyons, Christopher J., and Velez, Maria B. National Neighborhood Crime Study, Wave 2 (NNCS2), [United States], 1999-2013.
Inter-university Consortium for Political and Social Research [distributor], 2023-01-25.
https://doi.org/10.3886/ICPSR38483.v2 -->

---

## C. HuD_Replication (Christensen & Timmins replication package)

**How it is used**
- `matching_diagnosis.R` and various validation steps use the processed replication data in `Data/HuD_Replication/Final Data Sets/`.
- See `Data/HuD_Replication/readme.txt` for the full list of required files by table/figure.

**Required directory (as distributed by authors)**
- The entire replication package should be placed under `Data/HuD_Replication/` without modification.

**Key files used by this repository**
- `Data/HuD_Replication/Final Data Sets/recsprocessed_JPE.rds`
- `Data/HuD_Replication/Final Data Sets/HUDprocessed_JPE_testscores_042021.rds`
- `Data/HuD_Replication/Final Data Sets/HUDprocessed_JPE_census_042021.rds`
- `Data/HuD_Replication/Final Data Sets/HUDprocessed_JPE_names_042021.rds`
- `Data/HuD_Replication/Final Data Sets/HUDprocessed_tract.rds`
- `Data/HuD_Replication/Final Data Sets/adsprocessed_JPE.rds`
- `Data/HuD_Replication/Final Data Sets/tester_assignment.csv`

**How to reconstruct**
- If the replication package is included in the distribution for this project, place it at `Data/HuD_Replication/` exactly as received.
- If the replication package is not included, contact the project maintainer for access or substitution guidance.

**Note**
- The Christensen & Timmins replication package is not currently publicly available online. If/when it is distributable, this section should be updated with the official source.

---

## D. Validation checks (after data assembly)

Once all files are in place:

```bash
Rscript matching_diagnosis.R
Rscript data_cleaning.R
```

Successful runs will populate `Appendix_Tables/` and generate `Data/cleaned_hds.csv`.

---

## E. Access considerations

- RSEI and NNCS downloads may require agreement to data-use terms on their respective sites.
- Any required credentials should be obtained by the user following the providers' instructions.
