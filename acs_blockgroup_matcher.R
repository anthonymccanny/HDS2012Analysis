# ACS 2008-2012 5-Year Block Group Data Matcher
# Replicates Christensen & Timmins (2022) ACS data linking methodology
# Author: Anthony McCanny
# Date: 2025-09-21
# Purpose: Pull ACS block group data and merge with existing geocoded HDS data

# =================================================================================================== #
# SETUP: LOAD REQUIRED PACKAGES AND API KEYS
# =================================================================================================== #

packages <- c(
  "tidyverse",    # Data manipulation
  "tidycensus",   # Census API interface
  "readr"         # File I/O
)

# Install missing packages and load all
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Load API keys
source("api_keys.R")

# Set Census API key for tidycensus
census_api_key(CENSUS_API_KEY, install = TRUE)

# =================================================================================================== #
# ACS VARIABLE DEFINITIONS (CHRISTENSEN & TIMMINS 2022)
# =================================================================================================== #

# Define ACS variables matching Christensen & Timmins (2022) methodology
acs_variables <- c(
  # Poverty (Table B17001 - Poverty Status)
  poverty_total = "B17001_001",      # Total population for poverty determination
  poverty_below = "B17001_002",      # Income below poverty level

  # Education (Table B15003 - Educational Attainment)
  education_total = "B15003_001",    # Total population 25 years and over
  education_bachelors = "B15003_022", # Bachelor's degree
  education_masters = "B15003_023",   # Master's degree
  education_professional = "B15003_024", # Professional degree
  education_doctorate = "B15003_025", # Doctorate degree

  # Occupation (Table C24010 - Occupation by Sex)
  occupation_total = "C24010_001",   # Total civilian employed population 16+
  occupation_mgmt_male = "C24010_003",   # Management, business, science, arts - Male
  occupation_mgmt_female = "C24010_039", # Management, business, science, arts - Female

  # Household Type (Table B11001 - Household Type)
  total_households = "B11001_001",   # Total households
  family_households = "B11001_002",  # Family households
  single_parent_male = "B11001_006",   # Male householder, no spouse, with children
  single_parent_female = "B11001_016", # Female householder, no spouse, with children

  # Housing Tenure (Table B25003 - Tenure)
  tenure_total = "B25003_001",       # Total occupied housing units
  tenure_owned = "B25003_002",       # Owner occupied

  # Race and Hispanic Origin (Table B03002)
  race_total = "B03002_001",         # Total population
  race_white_alone = "B03002_003",   # White alone
  race_black_alone = "B03002_004",   # Black or African American alone
  race_asian_alone = "B03002_006",   # Asian alone
  race_hispanic = "B03002_012"       # Hispanic or Latino
)

# =================================================================================================== #
# MAIN FUNCTION: GET ACS DATA FOR SPECIFIC BLOCK GROUPS
# =================================================================================================== #

get_acs_data_for_geoids <- function(geoid_list, year = 2012) {
  """
  Pull ACS 2008-2012 5-year data for specific block group GEOIDs

  Args:
    geoid_list: Vector of 12-digit block group GEOIDs
    year: ACS year (2012 for 2008-2012 5-year estimates)

  Returns:
    Tibble with processed ACS variables for the specified block groups
  """

  # Extract unique states from GEOIDs
  unique_states <- unique(str_sub(geoid_list, 1, 2))
  cat(sprintf("Fetching ACS data for %d block groups across %d states...\n",
              length(unique(geoid_list)), length(unique_states)))

  all_data <- tibble()

  for (state in unique_states) {
    cat(sprintf("Processing state %s...\n", state))

    # Get all block group data for this state
    state_data <- get_acs(
      geography = "block group",
      variables = acs_variables,
      state = state,
      year = year,
      survey = "acs5",
      output = "wide"
    )

    all_data <- bind_rows(all_data, state_data)
    Sys.sleep(0.1)  # Rate limiting
  }

  # Filter to only the GEOIDs we need and process variables
  filtered_data <- all_data %>%
    mutate(geoid_12 = str_pad(GEOID, 12, "left", "0")) %>%
    filter(geoid_12 %in% geoid_list) %>%
    process_acs_variables()

  cat(sprintf("Retrieved data for %d of %d requested block groups\n",
              nrow(filtered_data), length(unique(geoid_list))))

  return(filtered_data)
}

# =================================================================================================== #
# PROCESS ACS VARIABLES TO MATCH CHRISTENSEN & TIMMINS
# =================================================================================================== #

process_acs_variables <- function(acs_raw) {
  """
  Calculate derived variables matching Christensen & Timmins (2022) methodology
  """

  acs_processed <- acs_raw %>%
    # Clean GEOID to ensure 12 digits
    mutate(
      geoid_12 = str_pad(GEOID, 12, "left", "0"),
      state_fips = str_sub(geoid_12, 1, 2),
      county_fips = str_sub(geoid_12, 3, 5),
      tract_fips = str_sub(geoid_12, 6, 11),
      blockgroup_fips = str_sub(geoid_12, 12, 12)
    ) %>%

    # Calculate Christensen & Timmins variables
    mutate(
      # 1. Poverty rate (share of households at/below poverty line)
      poverty_rate = ifelse(poverty_totalE > 0, poverty_belowE / poverty_totalE, NA),

      # 2. College graduate rate (bachelor's degree or higher)
      college_graduate_rate = ifelse(
        education_totalE > 0,
        (education_bachelorsE + education_mastersE +
         education_professionalE + education_doctorateE) / education_totalE,
        NA
      ),

      # 3. High-skilled occupation rate (management, business, science, arts)
      high_skilled_rate = ifelse(
        occupation_totalE > 0,
        (occupation_mgmt_maleE + occupation_mgmt_femaleE) / occupation_totalE,
        NA
      ),

      # 4. Single-parent household rate
      single_parent_rate = ifelse(
        total_householdsE > 0,
        (single_parent_maleE + single_parent_femaleE) / total_householdsE,
        NA
      ),

      # 5. Home ownership rate
      ownership_rate = ifelse(tenure_totalE > 0, tenure_ownedE / tenure_totalE, NA),

      # 6. Racial composition (as shares of total population)
      percent_white = ifelse(race_totalE > 0, race_white_aloneE / race_totalE, NA),
      percent_black = ifelse(race_totalE > 0, race_black_aloneE / race_totalE, NA),
      percent_asian = ifelse(race_totalE > 0, race_asian_aloneE / race_totalE, NA),
      percent_hispanic = ifelse(race_totalE > 0, race_hispanicE / race_totalE, NA)
    ) %>%

    # Select final columns matching Christensen & Timmins variable names
    select(
      geoid_12, NAME, state_fips, county_fips, tract_fips, blockgroup_fips,
      poverty_rate, college_graduate_rate, high_skilled_rate,
      single_parent_rate, ownership_rate,
      percent_white, percent_black, percent_asian, percent_hispanic,
      # Keep raw counts for reference
      poverty_totalE, education_totalE, occupation_totalE,
      total_householdsE, tenure_totalE, race_totalE
    )

  return(acs_processed)
}

# =================================================================================================== #
# MERGE FUNCTION FOR HDS DATA
# =================================================================================================== #

merge_hds_with_acs <- function(hds_data, geoid_col = "final_stfid") {
  """
  Merge HDS property data with ACS block group data

  Args:
    hds_data: Data frame with HDS property data including geocoded block group IDs
    geoid_col: Column name containing 12-digit block group GEOIDs

  Returns:
    Data frame with HDS data merged with ACS variables
  """

  # Extract unique GEOIDs from HDS data
  geoid_list <- hds_data %>%
    pull(!!sym(geoid_col)) %>%
    unique() %>%
    na.omit() %>%
    str_pad(12, "left", "0")  # Ensure 12-digit format

  cat(sprintf("Found %d unique block groups in HDS data\n", length(geoid_list)))

  # Get ACS data for these specific block groups
  acs_data <- get_acs_data_for_geoids(geoid_list)

  # Merge with HDS data
  merged_data <- hds_data %>%
    mutate(geoid_12 = str_pad(!!sym(geoid_col), 12, "left", "0")) %>%
    left_join(acs_data, by = "geoid_12")

  # Report merge success
  n_matched <- sum(!is.na(merged_data$poverty_rate))
  n_total <- nrow(merged_data)
  cat(sprintf("Successfully merged ACS data for %d of %d properties (%.1f%%)\n",
              n_matched, n_total, 100 * n_matched / n_total))

  return(merged_data)
}

# =================================================================================================== #
# EXAMPLE USAGE
# =================================================================================================== #

# Example workflow:
#
# # 1. Load your geocoded HDS data
# hds_data <- read_csv("Data/hds_geocoded_data.csv")
#
# # 2. Merge with ACS data
# hds_with_acs <- merge_hds_with_acs(hds_data, geoid_col = "final_stfid")
#
# # 3. Save results
# write_csv(hds_with_acs, "Data/hds_data_with_acs_variables.csv")
#
# # 4. Summary of ACS variables
# hds_with_acs %>%
#   select(poverty_rate, college_graduate_rate, high_skilled_rate,
#          single_parent_rate, ownership_rate, percent_white, percent_black,
#          percent_asian, percent_hispanic) %>%
#   summary()

cat("ACS Block Group Matcher loaded successfully.\n")
cat("Use merge_hds_with_acs() to merge your HDS data with ACS variables.\n")