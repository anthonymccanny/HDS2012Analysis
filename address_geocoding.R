# Address Geocoding and Census Tract ID Assignment
# Author: Anthony McCanny
# Date: 2025-07-01
# Purpose: Algorithmically assign US Census tract IDs (STFIDs) to addresses using multiple geocoding strategies

# =================================================================================================== #
# SETUP: LOAD REQUIRED PACKAGES
# =================================================================================================== #

packages <- c(
  "tidyverse",    # Data manipulation
  "tidygeocoder", # Geocoding addresses
  # "tigris",       # Census geography data
  # "sf",           # Spatial operations
  # "RecordLinkage", # Fuzzy string matching
  "stringdist",   # String distance calculations
  #"zipcodeR",     # ZIP code operations
  "httr",         # HTTP requests for Census API
  "jsonlite",     # JSON parsing
  "readr",        # File I/O
  "stringr"       # String operations
)

# Install missing packages and load all
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Set working directory
setwd("/Users/anthony/Library/CloudStorage/OneDrive-UniversityofToronto/Research/Replication Games/HDS2012Analysis")

# =================================================================================================== #
# CONFIGURATION AND HELPER FUNCTIONS
# =================================================================================================== #

# Configuration
options(tigris_use_cache = TRUE)  # Cache census geography data
GEOCODING_BATCH_SIZE <- 100       # Process addresses in batches
MAX_RETRIES <- 3                  # Maximum retry attempts for failed geocoding

# Function to clean and standardize addresses
clean_address <- function(address, city = NULL, state = NULL, zip = NULL) {
  if (is.na(address) || address == "") {
    address <- ""
  }
  
  # Basic cleaning
  address <- str_trim(toupper(address))
  address <- str_replace_all(address, "[^A-Z0-9\\s]", " ")
  address <- str_replace_all(address, "\\s+", " ")
  
  # Standardize common abbreviations
  address <- str_replace_all(address, "\\bSTREET\\b", "ST")
  address <- str_replace_all(address, "\\bAVENUE\\b", "AVE")
  address <- str_replace_all(address, "\\bROAD\\b", "RD")
  address <- str_replace_all(address, "\\bDRIVE\\b", "DR")
  address <- str_replace_all(address, "\\bLANE\\b", "LN")
  address <- str_replace_all(address, "\\bCOURT\\b", "CT")
  address <- str_replace_all(address, "\\bBOULEVARD\\b", "BLVD")
  address <- str_replace_all(address, "\\bPLACE\\b", "PL")
  address <- str_replace_all(address, "\\bCIRCLE\\b", "CIR")
  address <- str_replace_all(address, "\\bAPARTMENT\\b", "APT")
  address <- str_replace_all(address, "\\bSUITE\\b", "STE")
  
  # Build full address string
  full_address <- address
  if (!is.null(city) && !is.na(city) && city != "") {
    full_address <- paste(full_address, str_trim(toupper(city)), sep = ", ")
  }
  if (!is.null(state) && !is.na(state) && state != "") {
    full_address <- paste(full_address, str_trim(toupper(state)), sep = ", ")
  }
  if (!is.null(zip) && !is.na(zip) && zip != "") {
    full_address <- paste(full_address, str_trim(zip))
  }
  
  return(str_trim(full_address))
}

# Function to get census tract from coordinates using Census API
get_census_tract_from_coords <- function(lat, lon, vintage = "2010", debug = FALSE) {
  if (is.na(lat) || is.na(lon)) {
    return(list(state = NA, county = NA, tract = NA, stfid = NA))
  }

  base_url <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates"

  # Try multiple vintage formats since the API might have changed
  vintage_options <- c(
    paste0("Census", vintage, "_Current"),
    paste0("ACS", as.numeric(vintage) + 5, "_Current"),
    "Current_Current"
  )

  for (vintage_format in vintage_options) {
    params <- list(
      x = lon,
      y = lat,
      benchmark = "Public_AR_Current",
      vintage = vintage_format,
      format = "json"
    )

    tryCatch({
      response <- GET(base_url, query = params)

      if (status_code(response) == 200) {
        content <- content(response, "text", encoding = "UTF-8")
        data <- fromJSON(content)

        if (debug) {
          cat("\n--- Debug: API Response for vintage", vintage_format, "---\n")
          cat("Response status:", status_code(response), "\n")
          cat("Available geographies:", names(data$result$geographies), "\n")
        }

        # Try different key patterns for census tracts
        tract_keys <- c(
          "Census Tracts",
          "2010 Census Tracts",
          paste0("Census Tracts_", vintage),
          "Census Tract"
        )

        for (key in tract_keys) {
          if (!is.null(data$result$geographies[[key]])) {
            tracts_data <- data$result$geographies[[key]]

            if (debug) {
              cat("Found key:", key, "\n")
              cat("Data type:", class(tracts_data), "\n")
              cat("Data structure:", str(tracts_data), "\n")
            }

            # Handle different response structures
            tract_info <- NULL

            # If it's a list of tracts
            if (is.list(tracts_data) && length(tracts_data) > 0) {
              # If it's a named list with tract info
              if (!is.null(names(tracts_data))) {
                # Check if it has the expected fields directly
                if (all(c("STATE", "COUNTY", "TRACT") %in% names(tracts_data))) {
                  tract_info <- tracts_data
                } else if (length(tracts_data) > 0 && is.list(tracts_data[[1]])) {
                  # It might be a list of tract records
                  tract_info <- tracts_data[[1]]
                }
              } else if (length(tracts_data) > 0) {
                # Unnamed list - take first element
                tract_info <- tracts_data[[1]]
              }
            }

            # Extract fields if we found valid tract info
            if (!is.null(tract_info) && is.list(tract_info)) {
              if (debug) {
                cat("Tract info fields:", names(tract_info), "\n")
              }

              state <- tract_info$STATE
              county <- tract_info$COUNTY
              tract <- tract_info$TRACT

              # Also try alternative field names
              if (is.null(state)) state <- tract_info$state
              if (is.null(county)) county <- tract_info$county
              if (is.null(tract)) tract <- tract_info$tract

              # Create STFID (State + County + Tract)
              if (!is.null(state) && !is.null(county) && !is.null(tract)) {
                stfid <- paste0(state, county, tract)

                if (debug) {
                  cat("Successfully extracted STFID:", stfid, "\n")
                }

                return(list(
                  state = state,
                  county = county,
                  tract = tract,
                  stfid = stfid
                ))
              } else if (debug) {
                cat("Missing required fields. State:", state, "County:", county, "Tract:", tract, "\n")
              }
            }
          }
        }
      }
    }, error = function(e) {
      if (debug) {
        cat("Error with vintage", vintage_format, ":", e$message, "\n")
      }
    })
  }

  if (debug) {
    cat("Failed to get census tract for coordinates:", lat, lon, "\n")
  }

  return(list(state = NA, county = NA, tract = NA, stfid = NA))
}

# Function to geocode addresses using multiple services
geocode_address_multi_service <- function(address, method = "cascade") {
  if (is.na(address) || address == "" || str_trim(address) == "") {
    return(list(lat = NA, lon = NA, geocode_source = NA))
  }
  
  services <- c("census", "osm", "arcgis")
  
  for (service in services) {
    tryCatch({
      cat("Trying", service, "for:", str_trunc(address, 50), "\n")
      
      result <- geo(address, method = service, limit = 1, full_results = FALSE)
      
      if (nrow(result) > 0 && !is.na(result$lat[1]) && !is.na(result$long[1])) {
        return(list(
          lat = result$lat[1],
          lon = result$long[1], 
          geocode_source = service
        ))
      }
      
    }, error = function(e) {
      cat("  ", service, "failed:", e$message, "\n")
    })
    
    # Small delay between services
    Sys.sleep(0.5)
  }
  
  return(list(lat = NA, lon = NA, geocode_source = NA))
}

# Function for fuzzy matching city names
fuzzy_match_city <- function(input_city, reference_cities, threshold = 0.8) {
  if (is.na(input_city) || input_city == "") {
    return(NA)
  }
  
  input_clean <- str_trim(toupper(input_city))
  reference_clean <- str_trim(toupper(reference_cities))
  
  # Calculate string distances
  distances <- stringdist(input_clean, reference_clean, method = "jw")  # Jaro-Winkler
  min_dist <- min(distances, na.rm = TRUE)
  
  if (min_dist <= (1 - threshold)) {
    return(reference_cities[which.min(distances)])
  }
  
  return(NA)
}

# =================================================================================================== #
# MAIN GEOCODING FUNCTION
# =================================================================================================== #

geocode_and_get_stfid <- function(data, 
                                  address_col = "address", 
                                  city_col = "city", 
                                  state_col = "state", 
                                  zip_col = "zip",
                                  existing_stfid_col = "stfid") {
  
  cat("=== STARTING GEOCODING AND STFID ASSIGNMENT ===\n")
  cat("Input data has", nrow(data), "rows\n")
  
  # Create working copy
  df <- data %>%
    mutate(
      row_id = row_number(),
      original_address = if(address_col %in% names(.)) .[[address_col]] else NA,
      original_city = if(city_col %in% names(.)) .[[city_col]] else NA,
      original_state = if(state_col %in% names(.)) .[[state_col]] else NA,
      original_zip = if(zip_col %in% names(.)) .[[zip_col]] else NA,
      existing_stfid = if(existing_stfid_col %in% names(.)) .[[existing_stfid_col]] else NA
    )
  
  # Initialize result columns
  df <- df %>%
    mutate(
      geocoded_lat = NA_real_,
      geocoded_lon = NA_real_,
      geocode_source = NA_character_,
      derived_state = NA_character_,
      derived_county = NA_character_,
      derived_tract = NA_character_,
      derived_stfid = NA_character_,
      geocoding_method = NA_character_,
      geocoding_quality = NA_character_
    )
  
  # Identify rows that need geocoding (missing STFID)
  needs_geocoding <- df %>%
    filter(is.na(existing_stfid) | existing_stfid == "")
  
  cat("Rows needing geocoding:", nrow(needs_geocoding), "\n")
  
  if (nrow(needs_geocoding) == 0) {
    cat("No rows need geocoding. Returning original data.\n")
    return(df)
  }
  
  # Strategy 1: Full address geocoding
  cat("\n--- Strategy 1: Full Address Geocoding ---\n")
  
  for (i in 1:nrow(needs_geocoding)) {
    row_idx <- needs_geocoding$row_id[i]
    
    # Clean and construct full address
    clean_addr <- clean_address(
      needs_geocoding$original_address[i],
      needs_geocoding$original_city[i],
      needs_geocoding$original_state[i],
      needs_geocoding$original_zip[i]
    )
    
    if (clean_addr != "" && !is.na(clean_addr)) {
      cat("Processing row", i, "of", nrow(needs_geocoding), "- Row ID:", row_idx, "\n")
      
      # Geocode the address
      geocode_result <- geocode_address_multi_service(clean_addr)
      
      if (!is.na(geocode_result$lat)) {
        # Get census tract from coordinates (with debug enabled for first 10)
        census_result <- get_census_tract_from_coords(
          geocode_result$lat,
          geocode_result$lon,
          vintage = "2010",
          debug = (i <= 10)
        )
        
        # Update the main dataframe
        df[df$row_id == row_idx, c(
          "geocoded_lat", "geocoded_lon", "geocode_source",
          "derived_state", "derived_county", "derived_tract", "derived_stfid",
          "geocoding_method", "geocoding_quality"
        )] <- list(
          geocode_result$lat,
          geocode_result$lon,
          geocode_result$geocode_source,
          census_result$state,
          census_result$county,
          census_result$tract,
          census_result$stfid,
          "full_address",
          "high"
        )
        
        cat("  Successfully geocoded and assigned STFID:", census_result$stfid, "\n")
      } else {
        cat("  Failed to geocode address\n")
      }
    }
    
    # Small delay to be respectful to geocoding services
    if (i %% 10 == 0) {
      Sys.sleep(1)
    }
  }
  
  # Strategy 2: ZIP code centroid geocoding for failed cases
  cat("\n--- Strategy 2: ZIP Code Centroid Geocoding ---\n")
  
  still_missing <- df %>%
    filter(is.na(derived_stfid) & !is.na(original_zip) & original_zip != "")
  
  cat("Rows to try ZIP code geocoding:", nrow(still_missing), "\n")
  
  if (nrow(still_missing) > 0) {
    # Get unique ZIP codes to minimize API calls
    unique_zips <- unique(still_missing$original_zip)
    zip_geocodes <- data.frame(
      zip = unique_zips,
      lat = NA_real_,
      lon = NA_real_,
      source = NA_character_
    )
    
    for (i in 1:length(unique_zips)) {
      zip_code <- unique_zips[i]
      cat("Geocoding ZIP code", zip_code, "(", i, "of", length(unique_zips), ")\n")
      
      geocode_result <- geocode_address_multi_service(paste("ZIP", zip_code, "USA"))
      
      if (!is.na(geocode_result$lat)) {
        zip_geocodes[i, c("lat", "lon", "source")] <- list(
          geocode_result$lat,
          geocode_result$lon,
          geocode_result$geocode_source
        )
      }
      
      if (i %% 5 == 0) {
        Sys.sleep(1)
      }
    }
    
    # Apply ZIP code geocoding results
    for (i in 1:nrow(still_missing)) {
      row_idx <- still_missing$row_id[i]
      zip_code <- still_missing$original_zip[i]
      
      zip_info <- zip_geocodes[zip_geocodes$zip == zip_code, ]
      
      if (nrow(zip_info) > 0 && !is.na(zip_info$lat[1])) {
        census_result <- get_census_tract_from_coords(zip_info$lat[1], zip_info$lon[1], vintage = "2010")
        
        if (!is.na(census_result$stfid)) {
          df[df$row_id == row_idx, c(
            "geocoded_lat", "geocoded_lon", "geocode_source",
            "derived_state", "derived_county", "derived_tract", "derived_stfid",
            "geocoding_method", "geocoding_quality"
          )] <- list(
            zip_info$lat[1],
            zip_info$lon[1],
            zip_info$source[1],
            census_result$state,
            census_result$county,
            census_result$tract,
            census_result$stfid,
            "zip_centroid",
            "medium"
          )
          
          cat("  ZIP", zip_code, "assigned STFID:", census_result$stfid, "\n")
        }
      }
    }
  }
  
  # Create final STFID column (use existing if available, otherwise derived)
  df <- df %>%
    mutate(
      final_stfid = case_when(
        !is.na(existing_stfid) & existing_stfid != "" ~ existing_stfid,
        !is.na(derived_stfid) & derived_stfid != "" ~ derived_stfid,
        TRUE ~ NA_character_
      ),
      stfid_source = case_when(
        !is.na(existing_stfid) & existing_stfid != "" ~ "original",
        !is.na(derived_stfid) & derived_stfid != "" ~ "geocoded",
        TRUE ~ "missing"
      )
    )
  
  # Summary
  cat("\n=== GEOCODING SUMMARY ===\n")
  summary_stats <- df %>%
    summarise(
      total_rows = n(),
      had_original_stfid = sum(!is.na(existing_stfid) & existing_stfid != ""),
      successfully_geocoded = sum(!is.na(derived_stfid) & derived_stfid != ""),
      still_missing = sum(is.na(final_stfid) | final_stfid == ""),
      high_quality = sum(geocoding_quality == "high", na.rm = TRUE),
      medium_quality = sum(geocoding_quality == "medium", na.rm = TRUE)
    )
  
  cat("Total rows:", summary_stats$total_rows, "\n")
  cat("Had original STFID:", summary_stats$had_original_stfid, "\n")
  cat("Successfully geocoded:", summary_stats$successfully_geocoded, "\n")
  cat("Still missing STFID:", summary_stats$still_missing, "\n")
  cat("High quality geocodes:", summary_stats$high_quality, "\n")
  cat("Medium quality geocodes:", summary_stats$medium_quality, "\n")
  
  return(df)
}

# =================================================================================================== #
# WRAPPER FUNCTION FOR HDS DATA
# =================================================================================================== #

# Function specifically for your HDS data structure
geocode_hds_data <- function(input_file = "Data/sales_tester_rhgeo_merged.csv",
                             output_file = "Data/sales_tester_rhgeo_geocoded.csv") {
  
  cat("=== GEOCODING HDS DATA ===\n")
  
  # Read the data
  if (file.exists(input_file)) {
    hds_data <- read_csv(input_file)
    cat("Loaded", nrow(hds_data), "rows from", input_file, "\n")
  } else {
    stop("Input file not found:", input_file)
  }
  
  # Apply geocoding
  geocoded_data <- geocode_and_get_stfid(
    data = hds_data,
    address_col = "HSITEAD",  # Address column in HDS data
    city_col = "HCITY",      # City column in HDS data  
    state_col = "HSTATE",    # State column in HDS data
    zip_col = "HZIP",        # ZIP column in HDS data
    existing_stfid_col = "stfid"  # Existing STFID column
  )
  
  # Save results
  write_csv(geocoded_data, output_file)
  cat("Saved geocoded data to", output_file, "\n")
  
  return(geocoded_data)
}

# =================================================================================================== #
# UTILITY FUNCTIONS FOR ADDRESS VALIDATION
# =================================================================================================== #

# Function to validate and clean addresses before geocoding
validate_addresses <- function(data, address_col, city_col = NULL, zip_col = NULL) {
  
  df <- data %>%
    mutate(
      address_length = nchar(str_trim(.[[address_col]])),
      has_number = str_detect(.[[address_col]], "\\d"),
      has_street_indicator = str_detect(toupper(.[[address_col]]), 
                                       "\\b(ST|STREET|AVE|AVENUE|RD|ROAD|DR|DRIVE|LN|LANE|CT|COURT|BLVD|BOULEVARD|PL|PLACE|CIR|CIRCLE)\\b"),
      address_quality = case_when(
        is.na(.[[address_col]]) | .[[address_col]] == "" ~ "missing",
        address_length < 5 ~ "too_short", 
        !has_number ~ "no_house_number",
        !has_street_indicator ~ "no_street_type",
        TRUE ~ "good"
      )
    )
  
  if (!is.null(city_col) && city_col %in% names(data)) {
    df <- df %>%
      mutate(
        has_city = !is.na(.[[city_col]]) & .[[city_col]] != "",
        address_quality = case_when(
          address_quality == "good" & !has_city ~ "missing_city",
          TRUE ~ address_quality
        )
      )
  }
  
  if (!is.null(zip_col) && zip_col %in% names(data)) {
    df <- df %>%
      mutate(
        has_zip = !is.na(.[[zip_col]]) & .[[zip_col]] != "",
        zip_valid = str_detect(.[[zip_col]], "^\\d{5}(-\\d{4})?$"),
        address_quality = case_when(
          address_quality %in% c("good", "missing_city") & !has_zip ~ "missing_zip",
          address_quality %in% c("good", "missing_city") & has_zip & !zip_valid ~ "invalid_zip",
          TRUE ~ address_quality
        )
      )
  }
  
  # Summary
  quality_summary <- df %>%
    count(address_quality, sort = TRUE) %>%
    mutate(percentage = round(n / nrow(df) * 100, 1))
  
  cat("Address Quality Summary:\n")
  print(quality_summary)
  
  return(df)
}
