# ==============================================================================
# Data Censoring Script for HDS 2012 Tester Dataset
# ==============================================================================
# 
# Purpose: Remove confidential/sensitive variables from the raw HDS dataset
#          and create a censored version suitable for sharing or analysis
# 
# Input:   Data/HDS_raw_data/tester.sas7bdat (original SAS dataset)
# Output:  Data/HDS_raw_data/tester_censored.sas7bdat (censored SAS dataset)
# 
# Author:  Anthony McCanny
# Date:    2025-05-27
# ==============================================================================

library(haven)
library(dplyr)
library(tidyr)

# Load the SAS dataset
data <- read_sas("Data/HDS_raw_data/tester.sas7bdat")

# Define variables to keep based on the codebook
vars_to_keep <- c(
    "TesterID", "APRACE", "TRACESPY", "TASIANG", "TNATORIG", "THISPUBG",
    "TASIANS", "TTRIBE", "TSEX", "TDOB", "TCORGIN", "TCORGI2", "TTIMEUS",
    "TTMUSMO", "TTMUSYR", "TMALIVE", "TLIVMON", "TLIVYR", "TENGFL",
    "TENGAGE", "TPROFO", "TPROFOS", "TLANGHOM", "TCUREMP", "TPREVE",
    "TSTUDNT", "THIGHEDU", "TDEGREE", "TPEGAI", "THHEGAI", "TTIMECUR",
    "TCURTENR", "TCURTENS", "TDWLTYP", "TDWLTYPS", "TMORTFIN", "THOMEHNT",
    "THMHNTS", "TIFWRE", "TIFWRED", "TIFWREP", "TVIEWRE", "TVIEWRES",
    "TEXPERNC", "TNOTESTS", "TTESTTP1", "TTESTTP2", "TTESTTP3", "TTESTTP4",
    "TTESTTP5", "TTESTTP6", "TTESTTP7", "TTSTTYPS", "TRESERV", "THIPROF",
    "TDRIVER", "TOWNCAR", "TCOMPUTE", "TCOMPUTS", "TCOMPACC", "TCOMPACS"
)

# Keep only variables that exist in the dataset
vars_available <- intersect(vars_to_keep, names(data))
vars_to_remove <- setdiff(names(data), vars_available)
data_censored <- data %>% select(all_of(vars_available))

# Save the censored dataset with suffix
write_sas(data_censored, "Data/HDS_raw_data/tester_censored.sas7bdat")

# Print summary of what was removed
cat("Removed", length(vars_to_remove), "confidential variables:\n")
cat(paste(vars_to_remove, collapse = ", "), "\n")
cat("\nOriginal dataset had", ncol(data), "variables\n")
cat("Censored dataset has", ncol(data_censored), "variables\n")
