# Example Usage of Address Geocoding for HDS Data
# This script demonstrates how to use the address geocoding functions

# Source the geocoding functions
source("address_geocoding.R")

# =================================================================================================== #
# EXAMPLE 1: VALIDATE YOUR EXISTING ADDRESSES
# =================================================================================================== #

cat("=== VALIDATING EXISTING ADDRESSES ===\n")

# Load your existing data to see what we're working with
if (file.exists("Data/sales_tester_rhgeo_merged.csv")) {
  hds_data <- read_csv("Data/sales_tester_rhgeo_merged.csv")
  
  # Validate address quality
  validated <- validate_addresses(
    data = hds_data,
    address_col = "HSITEAD", 
    city_col = "HCITY",
    zip_col = "HZIP"
  )
  
  # Show examples of different quality categories
  cat("\nExamples by address quality:\n")
  for (quality in unique(validated$address_quality)) {
    cat("\n", quality, "examples:\n")
    examples <- validated %>%
      filter(address_quality == quality) %>%
      select(CONTROL, TESTERID, HSITEAD, HCITY, HSTATE, HZIP, stfid) %>%
      head(3)
    print(examples)
  }
}

# =================================================================================================== #
# EXAMPLE 2: GEOCODE A SMALL SAMPLE FIRST (RECOMMENDED)
# =================================================================================================== #

cat("\n=== TESTING WITH SMALL SAMPLE ===\n")

if (exists("hds_data")) {
  # Take a small sample of rows missing STFID for testing
  test_sample <- hds_data %>%
    filter(is.na(stfid) | stfid == "") %>%
    head(10)  # Just 10 rows for testing
  
validated <- validate_addresses(
    data = test_sample,
    address_col = "HSITEAD", 
    city_col = "HCITY",
    zip_col = "HZIP"
  )
  
  # Show examples of different quality categories
  cat("\nExamples by address quality:\n")
  for (quality in unique(validated$address_quality)) {
    cat("\n", quality, "examples:\n")
    examples <- validated %>%
      filter(address_quality == quality) %>%
      select(CONTROL, TESTERID, HSITEAD, HCITY, HSTATE, HZIP, stfid) %>%
      head(3)
    print(examples)
  }

  # Show test sample with complete address information
  complete_address_rows <- test_sample %>%
    filter(!is.na(HSITEAD) & HSITEAD != "" | 
           !is.na(HCITY) & HCITY != "" | 
           !is.na(HSTATE) & HSTATE != "" | 
           !is.na(HZIP) & HZIP != "")
  
  cat("\nRows with non-missing HSITEAD, HCITY, HSTATE, OR HZIP (", nrow(complete_address_rows), "rows):\n")
  complete_address_display <- complete_address_rows %>%
    select(CONTROL, TESTERID, HSITEAD, HCITY, HSTATE, HZIP, stfid)
  print(complete_address_display)

  if (nrow(test_sample) > 0) {
    cat("Testing geocoding with", nrow(test_sample), "rows missing STFID\n")
    
    # Run geocoding on the sample
    test_results <- geocode_and_get_stfid(
      data = test_sample,
      address_col = "HSITEAD",
      city_col = "HCITY", 
      state_col = "HSTATE",
      zip_col = "HZIP",
      existing_stfid_col = "stfid"
    )
    
    # Show results
    cat("\nTest Results:\n")
    results_summary <- test_results %>%
      select(CONTROL, TESTERID, HSITEAD, HCITY, HZIP, 
             existing_stfid, derived_stfid, final_stfid, 
             geocoding_method, geocoding_quality) %>%
      mutate(
        address_short = str_trunc(HSITEAD, 30),
        success = !is.na(final_stfid)
      )
    
    print(results_summary)
    
    cat("Success rate:", mean(results_summary$success, na.rm = TRUE) * 100, "%\n")
  } else {
    cat("No rows found missing STFID for testing\n")
  }
}

# =================================================================================================== #
# EXAMPLE 3: PROCESS FULL DATASET (UNCOMMENT TO RUN)
# =================================================================================================== #

# WARNING: This will make many API calls and take time!
# Uncomment the lines below only when you're ready to process the full dataset

# cat("\n=== PROCESSING FULL DATASET ===\n")
# cat("WARNING: This will take considerable time and make many API calls\n")
# cat("Uncomment this section when ready to run on full dataset\n")

# full_results <- geocode_hds_data(
#   input_file = "Data/sales_tester_rhgeo_merged.csv",
#   output_file = "Data/sales_tester_rhgeo_geocoded.csv"
# )

# =================================================================================================== #
# EXAMPLE 4: MANUAL GEOCODING FOR SPECIFIC CASES
# =================================================================================================== #

cat("\n=== MANUAL GEOCODING EXAMPLES ===\n")

# Test with some example addresses
example_addresses <- data.frame(
  id = 1:5,
  address = c(
    "123 Main St",
    "456 Oak Avenue", 
    "789 Park Drive",
    "",  # Missing address
    "999 Nonexistent Blvd"
  ),
  city = c("New York", "Los Angeles", "Chicago", "Boston", "Nowhere"),
  state = c("NY", "CA", "IL", "MA", "XX"), 
  zip = c("10001", "90210", "60601", "02101", "00000"),
  existing_stfid = c(NA, NA, NA, NA, NA)
)

cat("Testing with example addresses:\n")
print(example_addresses)

# Geocode the examples
example_results <- geocode_and_get_stfid(
  data = example_addresses,
  address_col = "address",
  city_col = "city",
  state_col = "state", 
  zip_col = "zip",
  existing_stfid_col = "existing_stfid"
)

cat("\nExample Results:\n")
example_summary <- example_results %>%
  select(id, address, city, state, zip, final_stfid, geocoding_method, geocoding_quality)
print(example_summary)

# =================================================================================================== #
# TIPS FOR USAGE
# =================================================================================================== #

cat("\n=== USAGE TIPS ===\n")
cat("1. Always test with a small sample first\n")
cat("2. The geocoding process makes API calls - be respectful and don't abuse\n")
cat("3. Results with geocoding_quality = 'high' are more reliable than 'medium'\n")
cat("4. 'full_address' method is better than 'zip_centroid' method\n")
cat("5. Save your results frequently - geocoding takes time!\n")
cat("6. Check the geocoding_quality and geocoding_method columns to understand how each STFID was derived\n")
cat("\nTo run on your full dataset, uncomment the full dataset section above\n")
