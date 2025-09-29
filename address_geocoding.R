# Address Geocoding Script
# Author: Anthony McCanny
# Date: Sep. 28, 2025
# Description: Geocodes addresses and retrieves census geographic identifiers using
#              the tidygeocoder package and TIGERweb Census API

library(httr)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)
library(tidygeocoder)


# TIGERweb Census 2010 service and layer IDs
BASE_URL <- "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/tigerWMS_Census2010/MapServer"
BLOCK_LAYER_ID <- 18

# Query block layer for one point with exponential backoff retry
tigerwebAPIlookup <- function(lon, lat, max_retries = 3) {
  for (attempt in 1:max_retries) {
    # Exponential backoff: 0.1s, 0.4s, 1.6s delays
    if (attempt > 1) Sys.sleep(0.1 * 4^(attempt-2))

    resp <- GET(paste0(BASE_URL, "/", BLOCK_LAYER_ID, "/query"),
                query = list(
                  geometry = paste(lon, lat, sep = ","),
                  geometryType = "esriGeometryPoint", inSR = "4326",
                  spatialRel = "esriSpatialRelIntersects",
                  outFields = "GEOID,STATE,COUNTY,TRACT,BLKGRP,BLOCK",
                  returnGeometry = "false", f = "json"
                ))
    
    if (!http_error(resp)) {
      x <- fromJSON(content(resp, "text"), simplifyVector = TRUE)
      return(if (length(x$features)) as_tibble(x$features$attributes) else tibble())
    }
    
    if (attempt == max_retries) return(tibble())
  }
}

# Lookup for a whole data frame with progress reporting
tigerwebAPIlookup_df <- function(df) {
  stopifnot(all(c("lon","lat") %in% names(df)))
  cat("Querying TIGERweb for", nrow(df), "locations with exponential backoff retry...\n")

  results <- purrr::map2_dfr(df$lon, df$lat, ~{
    bl <- tigerwebAPIlookup(.x, .y)
    if (nrow(bl) >= 1) {
      if (nrow(bl) > 1) {
        # point likely lies on a boundary; pick first deterministically
        geoid_first <- as.character(bl$GEOID[1])
        cat(sprintf("TIGERweb returned %d features at lon=%.6f lat=%.6f; using %s\n",
                    nrow(bl), .x, .y, geoid_first))
      }
      geoid <- as.character(bl$GEOID[1])
      tibble::tibble(
        TRACT_GEOID = substr(geoid, 1, 11),
        BLOCK_GEOID = geoid
      )
    } else {
      tibble::tibble(
        TRACT_GEOID = NA_character_,
        BLOCK_GEOID = NA_character_
      )
    }
  }, .progress = TRUE)

  stopifnot(nrow(results) == nrow(df))
  dplyr::bind_cols(df, results)
}
# Main geocoding function
geocode_addresses <- function(df, street_col, city_col, state_col, postalcode_col, 
     geoid_col) {
  
  # Validate required columns exist
  required_cols <- c(street_col, city_col, state_col, postalcode_col, geoid_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
  stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create working copy with standardized column names
  work_df <- df %>%
  mutate(
  street = get(street_col),
  city = get(city_col), 
  state = get(state_col),
  postalcode = get(postalcode_col),
  original_geoid = get(geoid_col)
  )
  
  cat("Summary:\n")
  cat("- Total rows:", nrow(df), "\n")
  cat("- Geocoding all addresses and fetching new GEOIDs...\n")
  
  # Geocode all addresses
  cat("\nGeocoding", nrow(work_df), "addresses...\n")
  
  # Initial geocoding with census - handle large datasets by chunking
  chunk_size <- 10000
  n_chunks <- ceiling(nrow(work_df) / chunk_size)
  
  if (n_chunks > 1) {
    cat("Large dataset detected (", nrow(work_df), " rows). Processing in chunks of", chunk_size, "...\n")
  }
  
  geocoded_results <- vector("list", n_chunks)
  
  for (i in 1:n_chunks) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, nrow(work_df))
    
    if (n_chunks > 1) {
      cat("Processing chunk", i, "of", n_chunks, "(rows", start_idx, "to", end_idx, ")...\n")
    }
    
    chunk_df <- work_df[start_idx:end_idx, ]
    
    geocoded_results[[i]] <- chunk_df %>%
      geocode(street=street, city=city, state=state, postalcode=postalcode, 
              method='census', full_results=TRUE, 
              api_options = list(census_return_type = 'geographies'),
              custom_query = list(
                benchmark = "Public_AR_Current",
                vintage   = "Census2010_Current",
                layers    = "Census Tracts,Census Block Groups,Census Blocks")) %>%
      mutate(
        tract_geoid = ifelse(
          is.na(census_tract) | is.na(state_fips) | is.na(county_fips) | 
          state_fips == "" | county_fips == "" | census_tract == "",
          NA_character_,
          paste0(state_fips, county_fips, census_tract)
        ),
        block_geoid = ifelse(
          is.na(census_tract) | is.na(census_block) | is.na(state_fips) | is.na(county_fips) | 
          census_tract == "" | census_block == "" | state_fips == "" | county_fips == "",
          NA_character_,
          paste0(state_fips, county_fips, census_tract, census_block)
        ),
        blockgroup_geoid = ifelse(
          is.na(block_geoid) | block_geoid == "",
          NA_character_,
          substr(block_geoid, 1, 12)
        )
      )
  }
  
  # Combine all chunks
  if (n_chunks > 1) {
    cat("Combining", n_chunks, "chunks...\n")
  }
  geocoded_results <- bind_rows(geocoded_results)
  
  # Retry failed geocodes with other services
  failed_idx <- which(is.na(geocoded_results$lat) | is.na(geocoded_results$long))
  if (length(failed_idx) > 0) {
  cat("Retrying", length(failed_idx), "failed geocodes...\n")
  
  for (method in c('osm', 'arcgis')) {
  if (length(failed_idx) == 0) break
  
  retry_results <- work_df[failed_idx, ] %>%
  geocode(street=street, city=city, state=state, postalcode=postalcode, method=method)
  
  success_idx <- which(!is.na(retry_results$lat) & !is.na(retry_results$long))
  if (length(success_idx) > 0) {
  geocoded_results[failed_idx[success_idx], c("lat", "long")] <- 
  retry_results[success_idx, c("lat", "long")]
  failed_idx <- failed_idx[-success_idx]
  }
  }
  }
  
  # Get census GEOIDs for all successfully geocoded addresses that need them
  needs_geoid <- which((!is.na(geocoded_results$lat) & !is.na(geocoded_results$long)) & 
      (is.na(geocoded_results$tract_geoid) | is.na(geocoded_results$block_geoid)))
  
  if (length(needs_geoid) > 0) {
  cat("Getting census GEOIDs for", length(needs_geoid), "locations without census data...\n")
  
  coords_df <- geocoded_results[needs_geoid, ] %>%
  select(lat, long) %>%
  rename(lon = long)
  
  geoid_results <- tigerwebAPIlookup_df(coords_df)
  
  # Update with TIGERweb results
  geocoded_results$tract_geoid[needs_geoid] <- geoid_results$TRACT_GEOID
  geocoded_results$block_geoid[needs_geoid] <- geoid_results$BLOCK_GEOID
  geocoded_results$blockgroup_geoid[needs_geoid] <- ifelse(
  is.na(geoid_results$BLOCK_GEOID) | geoid_results$BLOCK_GEOID == "",
  NA_character_,
  substr(geoid_results$BLOCK_GEOID, 1, 12)
  )
  }
  
  # Check GEOID matches and create mismatch dataframe
  geocoded_results <- geocoded_results %>%
  mutate(
  original_geoid_clean = ifelse(original_geoid == "" | is.na(original_geoid), 
                 NA_character_, 
                 trimws(as.character(original_geoid))),
  geoid_match = case_when(
  is.na(original_geoid_clean) | is.na(blockgroup_geoid) ~ NA,
  substr(original_geoid_clean, 1, nchar(original_geoid_clean)) == 
  substr(blockgroup_geoid, 1, nchar(original_geoid_clean)) ~ TRUE,
  TRUE ~ FALSE
  )
  )
  
  # Create mismatch dataframe
  mismatches <- geocoded_results %>%
  filter(!is.na(geoid_match) & geoid_match == FALSE) %>%
  select(street, city, state, postalcode, original_geoid_clean, blockgroup_geoid, lat, long)
  
  # Prepare final result
  result_df <- df
  result_df$lat <- geocoded_results$lat
  result_df$long <- geocoded_results$long
  result_df$blockgroup_geoid <- geocoded_results$blockgroup_geoid
  result_df$block_geoid <- geocoded_results$block_geoid
  
  # Final summary
  total_successful <- sum(!is.na(result_df$lat) & !is.na(result_df$long))
  total_matches <- sum(!is.na(geocoded_results$geoid_match) & geocoded_results$geoid_match == TRUE)
  total_mismatches <- nrow(mismatches)
  
  cat("\nFinal Summary:\n")
  cat("- Successfully geocoded:", total_successful, "out of", nrow(df), "addresses\n")
  cat("- GEOID matches:", total_matches, "\n")
  cat("- GEOID mismatches:", total_mismatches, "\n")
  
  # Save mismatch dataframe to global environment
  assign("geoid_mismatches", mismatches, envir = .GlobalEnv)
  cat("- Mismatch dataframe saved as 'geoid_mismatches'\n")
  
  return(result_df)
}

# =============================================================================
# EXAMPLE VIGNETTE
# =============================================================================
# # Create test dataframe with valid US addresses
# test_addresses <- data.frame(
#   id = 1:10,
#   street = c(
#     "1600 Pennsylvania Ave NW",
#     "350 5th Ave", 
#     "1 Infinite Loop",
#     "221B Baker Street",
#     "742 Evergreen Terrace",
#     "123 Main Street",  # Missing city
#     "456 Oak Avenue",   # Missing state
#     "",                 # Missing street
#     "789 Park Drive",   # Missing county
#     "999 First St"  # Missing postalcode
#   ),
#   city = c(
#     "Washington",
#     "New York",
#     "Cupertino", 
#     "London",
#     "Springfield",
#     "",           # Missing
#     "Los Angeles",
#     "Boston",
#     "Chicago",
#     "Seattle"
#   ),
#   state = c(
#     "DC",
#     "NY",
#     "CA",
#     "NW",
#     "IL", 
#     "TX",
#     "",     # Missing
#     "MA",
#     "IL", 
#     "WA"
#   ),
#   postalcode = c(
#     "",
#     "10118",
#     "95014",
#     "NW1 6XE", 
#     "62701",
#     "77001",
#     "90210",
#     "02101",
#     "60601",
#     ""        # Missing
#   ),
#   blockgroup_geoid = c(
#     "110010001001",
#     "36061006100",
#     "06085501902",
#     "",              # Missing
#     "17167001100",
#     "48201230400",
#     "06037207021",
#     "",              # Missing
#     "17031081800",
#     "53033005300"
#   )
# )
# 
# # Example usage with test data
# geocoded_results <- geocode_addresses(
#   df = test_addresses,
#   street_col = "street",
#   city_col = "city", 
#   state_col = "state",
#   postalcode_col = "postalcode",
#   geoid_col = "blockgroup_geoid"
# )
