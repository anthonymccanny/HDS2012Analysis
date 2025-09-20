# HDS 2012 Data Cleaning and Merging Script
# Author: Anthony McCanny
# Date: 2025-06-04
# Purpose: Merge HDS raw data files, specifically taf, sales, tester, rhgeo, assignment by CONTROL and TESTERID

# =================================================================================================== #
# SETUP: LOAD REQUIRED PACKAGES, SET UP ENVIRONMENT, DECLARE FUNCTIONS
# =================================================================================================== #

# Required packages
packages <- c(
  "haven",     # Reading SAS files (.sas7bdat format)
  "dplyr",     # Data manipulation and transformation
  "readr",     # Reading and writing CSV files
  "stringr",   # String manipulation and regex operations
  "purrr"      # For map functions
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

# Define data paths
raw_data_path <- "Data/HDS_raw_data"


# Function to read SAS files with UTF-8 encoding, and convert empty strings to NA
import_sas <- function(file_path) {
  tryCatch({
    data <- read_sas(file_path, encoding = "UTF-8")                               # Read SAS file with UTF-8 encoding
    data <- data %>% mutate(across(everything(), ~ ifelse(.x == "", NA, .x)))     # Convert empty strings to NA
    cat("Successfully imported", basename(file_path), ":", nrow(data), "rows x", ncol(data), "columns\n")
    return(data)
  }, error = function(e) {
    # Return error if UTF-8 encoding fails
    stop("Failed to read ", basename(file_path), " with UTF-8 encoding: ", e$message)
  })
}

# =================================================================================================== #
# IMPORT SAS DATA FILES
# =================================================================================================== #

cat("=== READING SAS FILES AND FILTERING TO SALES DATA ===\n")

# Read assignment file and filter to sales + RELEASE="1"
assignment_raw <- import_sas(file.path(raw_data_path, "assignment.sas7bdat"))
assignment <- assignment_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL)) %>%  # Filter to sales tests (SA/SB/SH)
  filter(RELEASE == "1" & !is.na(TESTERID))  # Only released tests with valid tester IDs
cat("Assignment: ", nrow(assignment_raw), "→", nrow(assignment), "rows after filtering to only sales and released tests\n")

# Read taf file and filter to sales
taf_raw <- import_sas(file.path(raw_data_path, "taf.sas7bdat"))
taf <- taf_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL))  # Filter to sales tests only
cat("TAF: ", nrow(taf_raw), "→", nrow(taf), "rows after sales filter\n")

# Read sales file (already sales data by definition)
sales <- import_sas(file.path(raw_data_path, "sales.sas7bdat"))

# Read tester file and standardize variable name
tester <- import_sas(file.path(raw_data_path, "tester_censored.sas7bdat")) %>% 
  rename(TESTERID = TesterID)  # Standardize variable name

# Read rhgeo file and filter to sales
rhgeo_raw <- import_sas(file.path(raw_data_path, "rhgeo.sas7bdat"))
rhgeo <- rhgeo_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL))  # Filter to sales tests only
cat("RHGEO: ", nrow(rhgeo_raw), "→", nrow(rhgeo), "rows after sales filter\n")

# Read rechomes file and filter to sales
rechomes_raw <- import_sas(file.path(raw_data_path, "rechomes.sas7bdat"))
rechomes <- rechomes_raw %>%
    filter(grepl("-S[A-Z]-", CONTROL))  # Filter to sales tests only
cat("RECHOMES: ", nrow(rechomes_raw), "→", nrow(rechomes), "rows after sales filter\n")


# =================================================================================================== #
# DATA CLEANING FUNCTIONS
# =================================================================================================== #

# Function to parse various date formats
parse_date_string <- function(date_string, is_birth_date = FALSE) {
  date_clean <- trimws(gsub("|[()]", "", as.character(date_string)))
  # First standardize separators - replace dashes and dots with forward slash
  date_standardized <- gsub("[-\\.]", "/", date_clean)
  # Then handle spaces and whitespace separately  
  date_standardized <- gsub("\\s+", "/", date_standardized)
  # Replace multiple slashes with single slash
  date_standardized <- gsub("//+", "/", date_standardized)

  # Manual corrections for known invalid dates
  corrections <- list(
    "6/31/12" = "2012-06-30",
    "02/29/2011" = "2011-02-28",
    "09/0/1983" = "1983-09-01",
    "08/18/7977" = "1977-08-18",
    "03/15/2948" = "1948-03-15",
    "01/08/1064" = "1964-01-08",
    "03/27/1066" = "1966-03-27",
    "08/02/1874" = "1974-08-02",
    "03/05/1012" = "2012-03-05",
    "05/17" = "2012-05-17"
  )
  
  if (date_standardized %in% names(corrections)) {
    return(as.Date(corrections[[date_clean]]))
  }

  # Date parsing patterns 
  patterns <- list(
    # Standard formats with full 4-digit years
    "^\\d{1,2}/\\d{1,2}/\\d{4}$" = "%m/%d/%Y",
    
    # 2-digit year formats (need special handling for birth vs appointment dates)
    "^\\d{1,2}/\\d{1,2}/\\d{2}$" = function(x) {
      if (is_birth_date) {
        # Birth dates: assume 19xx century
        year_prefix <- "19"
      } else {
        # Appointment dates: assume 20xx century  
        year_prefix <- "20"
      }
      full_date <- paste0(substr(x, 1, nchar(x)-2), year_prefix, substr(x, nchar(x)-1, nchar(x)))
      as.Date(full_date, format = "%m/%d/%Y")
    },
    
    # No separator - 8 digits (MMDDYYYY)
    "^\\d{8}$" = "%m%d%Y",
    
    # No separator - 6 digits (MMDDYY)
    "^\\d{6}$" = function(x) {
      month <- substr(x, 1, 2)
      day <- substr(x, 3, 4)
      year_suffix <- substr(x, 5, 6)
      
      if (is_birth_date) {
        year_full <- paste0("19", year_suffix)
      } else {
        year_full <- paste0("20", year_suffix)
      }
      
      full_date <- paste0(month, "/", day, "/", year_full)
      as.Date(full_date, format = "%m/%d/%Y")
    },
    
    # MMDD/YY format
    "^\\d{4}/\\d{2}$" = function(x) {
      month <- substr(x, 1, 2)
      day <- substr(x, 3, 4)
      year_suffix <- substr(x, 6, 7)
      
      if (is_birth_date) {
        year_full <- paste0("19", year_suffix)
      } else {
        year_full <- paste0("20", year_suffix)
      }
      
      full_date <- paste0(month, "/", day, "/", year_full)
      as.Date(full_date, format = "%m/%d/%Y")
    },
    
    # MMDD/YYYY format (parse as MM/DD/YY)
    "^\\d{4}/\\d{4}$" = function(x) {
      month <- substr(x, 1, 2)
      day <- substr(x, 3, 4)
      year_suffix <- substr(x, 8, 9)  # Take last 2 digits of year
      
      full_date <- paste0(month, "/", day, "/", year_suffix)
      as.Date(full_date, format = "%m/%d/%y")
    },
    
    # Ambiguous MM/YY or DD/MM formats - need intelligent parsing
    "^\\d{1,2}/\\d{2}$" = function(x) {
      parts <- strsplit(x, "/")[[1]]
      first_num <- as.numeric(parts[1])
      second_num <- as.numeric(parts[2])
      
      # Helper functions for validation
      could_be_month <- function(n) !is.na(n) && n >= 1 && n <= 12
      could_be_day <- function(n) !is.na(n) && n >= 1 && n <= 31
      
      # Scenario 1: First number too large for month (13-31) - likely DD/MM format
      if (!could_be_month(first_num) && could_be_day(first_num) && could_be_month(second_num)) {
        if (is_birth_date) {
          warning("Birth date appears to be DD/MM format - not implemented")
          return(NA)
        }
        # Treat as DD/MM/2000 for appointment dates - this will be caught and corrected by the correct_appointment_date function later
        return(as.Date(sprintf("%02d/%02d/2000", second_num, first_num), format = "%m/%d/%Y"))
      }
      
      # Scenario 2: Both numbers valid for month/day - need context clues
      if (could_be_month(first_num) && could_be_day(first_num) && could_be_month(second_num)) {
        if (is_birth_date) {
          # Try interpreting as MM/YY birth date
          birth_year <- 1900 + second_num
          age_in_2011 <- 2011 - birth_year
          if (age_in_2011 >= 18 && age_in_2011 <= 90) {
            return(as.Date(sprintf("%02d/01/%d", first_num, birth_year), format = "%m/%d/%Y"))
          }
        } else {
          # For appointment dates, check if second number suggests year (11-12)
          if (second_num >= 11 && second_num <= 12) {
            # Treat as MM/YY format
            return(as.Date(sprintf("%02d/01/20%02d", first_num, second_num), format = "%m/%d/%Y"))
          } else {
            # Treat as DD/MM/2000 for appointment dates - this will be caught and corrected by the correct_appointment_date function latert
            return(as.Date(sprintf("%02d/%02d/2000", second_num, first_num), format = "%m/%d/%Y"))
          }
        }
      }
      
      # Scenario 3: First number too large for month/day - might be YY/MM
      if (!could_be_month(first_num) && !could_be_day(first_num) && could_be_month(second_num)) {
        if (is_birth_date) {
          birth_year <- 1900 + first_num
          age_in_2011 <- 2011 - birth_year
          if (age_in_2011 >= 18 && age_in_2011 <= 90) {
            return(as.Date(sprintf("%02d/01/%d", second_num, birth_year), format = "%m/%d/%Y"))
          }
        } else if (first_num >= 11 && first_num <= 12) {
          # YY/MM format for 2011-2012
          return(as.Date(sprintf("%02d/01/20%02d", second_num, first_num), format = "%m/%d/%Y"))
        }
      }
      
      # If we reach here, couldn't determine format
      warning(paste("Could not parse ambiguous date format:", x))
      return(NA)
    }
  )
  
  # Try each pattern on the standardized date
  for (pattern in names(patterns)) {
    if (grepl(pattern, date_standardized)) {
      if (is.function(patterns[[pattern]])) {
        return(patterns[[pattern]](date_standardized))
      } else {
        return(as.Date(date_standardized, format = patterns[[pattern]]))
      }
    }
  }
  
  # If we reach here, couldn't determine format
  if (!is.na(date_string) && date_string != "") {
    warning(paste("Could not parse date format. Original:", date_string, "Standardized:", date_standardized))
  }
  return(NA)
}

# Function to parse time strings
parse_time_string <- function(time_string, am_pm_indicator = NULL) {
  time_clean <- trimws(as.character(time_string))

  hour <- NA
  minute <- NA
  
  # Check for invalid time
  if (is.na(time_clean) || time_clean == "" || 
      grepl("^(N/A|NA|n/a|na)$", time_clean, ignore.case = TRUE) ||
      grepl("^[^0-9:;.]+$", time_clean)) {
    return(list(hour = 0, minute = 0))
  }
  
  # Extract hour and minute based on format
  if (grepl("^\\d{4}$", time_clean)) {
    # Military time (e.g., "0315", "1430")
    hour <- as.numeric(substr(time_clean, 1, 2))
    minute <- as.numeric(substr(time_clean, 3, 4))
  } else if (grepl("^\\d{3}$", time_clean)) {
    # 3-digit format (e.g., "357" = 3:57)
    hour <- as.numeric(substr(time_clean, 1, 1))
    minute <- as.numeric(substr(time_clean, 2, 3))
  } else if (grepl("[:;.]", time_clean)) {
    # Separated format (colon, semicolon, or period)
    parts <- strsplit(time_clean, "[:;.]")[[1]]
    hour <- as.numeric(parts[1])
    minute <- if(length(parts) > 1) as.numeric(parts[2]) else 0
  } else {
    return(list(hour = 0, minute = 0))
  }
  
  # Validate and clean minute
  if (is.na(minute) || minute >= 60) minute <- 0
  
  # Handle AM/PM indicator with corrections for extreme cases
  if (!is.null(am_pm_indicator)) {
    # Handle missing AM/PM indicator (-1 or NULL) by guessing based on hour
    if (is.na(am_pm_indicator) || am_pm_indicator == -1) {
      if (!is.na(hour) && hour >= 9 && hour <= 12) {
        am_pm_indicator <- 1  # AM
      } else if (!is.na(hour) && hour >= 1 && hour <= 8) {
        am_pm_indicator <- 2  # PM
      }

    # Correct obviously wrong AM/PM indicators (only if hour is not NA)
    } else if (!is.na(hour) && am_pm_indicator == 1 && hour >= 1 && hour <= 5) {
      am_pm_indicator <- 2  # Change to PM

    } else if (!is.na(hour) && am_pm_indicator == 2 && hour >= 10 && hour <= 11) {
      am_pm_indicator <- 1  # Change to AM
    }
    
    # Convert to 24-hour format (only if hour is not NA)
    if (!is.na(hour) && am_pm_indicator == 2 && hour < 12) {
      hour <- hour + 12  # PM hours (1-11) + 12
    }
  } 

  # Handle invalid hours
  if (is.na(hour) || hour >= 24) hour <- 0
  
  return(list(hour = hour, minute = minute))
}

# Function to correct incorrect appointment dates
correct_appointment_date <- function(df) {
  # Identify rows that need correction
  df <- df %>%
    mutate(
    needs_correction = !is.na(appointment_date) & 
              (appointment_date < as.Date("2011-07-01") | 
               appointment_date > as.Date("2012-10-31"))
    )
  
  # For each row that needs correction, find the correct year from same CONTROL
  for (i in which(df$needs_correction)) {
    current_control <- df$CONTROL[i]
    current_date <- df$appointment_date[i]
    
    # Find other rows with same CONTROL that have valid dates
    same_control_rows <- df %>%
    filter(CONTROL == current_control, 
       !is.na(appointment_date),
       appointment_date >= as.Date("2011-07-01"),
       appointment_date <= as.Date("2012-10-31"))
    
    # If we found valid dates in the same control, use the first one's year
    if (nrow(same_control_rows) > 0) {
    reference_date <- same_control_rows$appointment_date[1]
    reference_year <- format(reference_date, "%Y")
    
    # Keep original month and day, but change year
    corrected_date <- as.Date(paste0(reference_year, "-", 
            format(current_date, "%m-%d")))
    
    # If the corrected year is the same as original, use reference month instead
    if (format(current_date, "%Y") == reference_year) {
    reference_month <- format(reference_date, "%m")
    corrected_date <- as.Date(paste0(reference_year, "-", 
            reference_month, "-", 
            format(current_date, "%d")))
    }
    
    df$appointment_date[i] <- corrected_date
    
    cat("Corrected CONTROL", current_control, ":", 
    format(current_date, "%Y-%m-%d"), "->", 
    format(corrected_date, "%Y-%m-%d"), "\n")
    } else {
    # Set to NA if no valid reference date found
    df$appointment_date[i] <- NA
    warning("No valid reference date found for CONTROL ", current_control, 
      ". Setting appointment_date to NA for incorrect date: ", 
      format(current_date, "%Y-%m-%d"))
    }
  }
  
  # Remove the temporary column
  df <- df %>% select(-needs_correction)
  
  return(df)
}

# Function to correct invalid appointment times (when 00:00 or some non-time value is entered)
correct_invalid_times <- function(df) {
  # Identify rows with 00:00:00 times
  df <- df %>%
    mutate(
      is_midnight = !is.na(appointment_datetime) & 
                    format(appointment_datetime, "%H:%M:%S") == "00:00:00"
    )
  
  # For each row with midnight time, check if other rows exist on same day in same CONTROL
  for (i in which(df$is_midnight)) {
    current_control <- df$CONTROL[i]
    current_date <- as.Date(df$appointment_datetime[i])
    
    # Find other rows with same CONTROL on same date (excluding the current row)
    same_control_same_day <- df %>%
      filter(CONTROL == current_control,
              !is.na(appointment_datetime),
              as.Date(appointment_datetime) == current_date,
              row_number() != i)
    
    # If other appointments exist on same day, set time to NA
    if (nrow(same_control_same_day) > 0) {
      df$appointment_datetime[i] <- NA
      cat("Set appointment_datetime to NA for CONTROL", current_control, 
          "on", format(current_date, "%Y-%m-%d"), 
          "due to other appointments on same day\n")
    }
  }
  
  # Remove temporary column
  df <- df %>% select(-is_midnight)
  
  return(df)
}

# Function to correct AM/PM indicators based on hour values
correct_am_pm_indicator <- function(hour, am_pm_indicator) {
  # Handle missing AM/PM indicator (-1 or NA) by guessing based on hour
  if_else(is.na(am_pm_indicator) | am_pm_indicator == -1,
    case_when(
      !is.na(hour) & hour >= 9 & hour <= 12 ~ TRUE,  # AM
      !is.na(hour) & hour >= 1 & hour <= 8 ~ FALSE,  # PM
      TRUE ~ NA  # Cannot determine, leave as NA
    ),
    # Correct obviously wrong AM/PM indicators (only if hour is not NA)
    case_when(
      !is.na(hour) & am_pm_indicator == 1 & hour >= 1 & hour <= 5 ~ FALSE,  # Change to PM
      !is.na(hour) & am_pm_indicator == 2 & hour >= 10 & hour <= 11 ~ TRUE,  # Change to AM
      am_pm_indicator == 1 ~ TRUE,   # Convert existing codes to TRUE/FALSE
      am_pm_indicator == 2 ~ FALSE,  # Convert existing codes to TRUE/FALSE
      TRUE ~ NA
    )
  )
}
# =================================================================================================== #
# SALES DATA CLEANING
# =================================================================================================== #

cat("=== CLEANING SALES DATA ===\n")

# Basic cleaning and column selection
sales_clean <- sales %>%
  mutate(
    STOTUNIT = as.numeric(STOTUNIT),
    SAVLBAD_BINARY = ifelse(SAVLBAD == "1", 1, 0)
  ) %>%
  select(CONTROL, TESTERID, RACEID, SEQUENCE, SSNDVIS, SAPPTD, SAPPTDY, 
         SBEGH, SBEGAM, STOTUNIT, SAVLBAD_BINARY) %>%
  filter(!is.na(CONTROL) & !is.na(TESTERID))

# Parse appointment dates and times
sales_with_time <- sales_clean %>%
  mutate(
    appointment_date = map_dbl(SAPPTD, ~ as.numeric(parse_date_string(.x, is_birth_date = FALSE))),
    appointment_date = as.Date(appointment_date, origin = "1970-01-01")
  ) %>% 
  correct_appointment_date() %>% # correct dates outside of the study period by setting year to year of visits from same trial
  mutate(
    time_parsed = map2(SBEGH, SBEGAM, parse_time_string),
    hour_24 = map_dbl(time_parsed, ~ .x$hour),
    minute_part = map_dbl(time_parsed, ~ .x$minute),
    appointment_datetime = as.POSIXct(
      paste(appointment_date, sprintf("%02d:%02d:00", hour_24, minute_part)),
      format = "%Y-%m-%d %H:%M:%S"
    ),
    am_indicator = correct_am_pm_indicator(hour = hour_24, am_pm_indicator = SBEGAM)
  ) %>%
  select(-time_parsed, -hour_24, -minute_part) %>%
  correct_invalid_times() # correct cases where appointment time is 00:00 or some non-time value

# Summary of appointment date and time issues
cat("=== APPOINTMENT DATE AND TIME SUMMARY ===\n")

sales_with_time %>%
  summarise(
    na_dates = sum(is.na(appointment_date)),
    na_datetimes = sum(is.na(appointment_datetime)),
    unusual_times = sum(!is.na(appointment_datetime) & 
                        (as.numeric(format(appointment_datetime, "%H")) < 6 | 
                        as.numeric(format(appointment_datetime, "%H")) >= 21)),
    midnight_times = sum(!is.na(appointment_datetime) & 
                        as.numeric(format(appointment_datetime, "%H")) == 0 & 
                        as.numeric(format(appointment_datetime, "%M")) == 0),
    midnight_with_others = sum(!is.na(appointment_datetime) & 
                              as.numeric(format(appointment_datetime, "%H")) == 0 & 
                              as.numeric(format(appointment_datetime, "%M")) == 0 &
                              CONTROL %in% (sales_with_time %>% 
                                          filter(!is.na(appointment_datetime)) %>%
                                          group_by(CONTROL, as.Date(appointment_datetime)) %>%
                                          filter(n() > 1) %>% pull(CONTROL)))
  ) %>%
  {
    cat("NA appointment dates:", .$na_dates, "\n")
    cat("NA appointment datetimes:", .$na_datetimes, "\n") 
    cat("Appointments before 6 AM or after 9 PM:", .$unusual_times, "\n")
    cat("Appointments at exactly 00:00 (midnight):", .$midnight_times, "\n")
    cat("NOTE: The", .$midnight_times, "midnight appointments have no other appointments on the same day,")
    cat(" so they won't impact appointment sequence determination.\n")
  }

# Print data on the CONTROL with rows where datetime is NA but date is not NA
sales_with_time %>%
  filter(CONTROL %in% (sales_with_time %>% 
                        filter(!is.na(appointment_date) & is.na(appointment_datetime)) %>% 
                        pull(CONTROL))) %>%
  {
    cat("All rows in CONTROL(s) where some rows have date but missing datetime:\n")
    print(., n = Inf, width = Inf)
  }

# NOTE: the only case where appointment_datetime is NA but appointment_date is not NA is when it's clear that
# row with the invalid time (e.g., 00:00) is clearly an error corrected in the next row (time: 12:00),
# so there is no information lost when dropping this row in later analyses.

# Determine visit order, aggregate and rename variables
sales_final <- sales_with_time %>%
  group_by(CONTROL) %>%
  arrange(appointment_datetime) %>%
  mutate(visit_order = ifelse(is.na(appointment_datetime), NA, row_number())) %>% # only rows with non-NA datetime will have visit_order, NA values ordered at the end of each CONTROL
  ungroup() %>%
  group_by(CONTROL, TESTERID) %>%
  summarise(
    RACEID = first(RACEID),
    am_indicator_first = first(am_indicator[which.min(visit_order)]),
    STOTUNIT_FIRST = first(STOTUNIT[which.min(visit_order)]),
    SAVLBAD_FIRST = first(SAVLBAD_BINARY[which.min(visit_order)]),
    first_visit_datetime = min(appointment_datetime, na.rm = TRUE),
    num_visits = n(),
    STOTUNIT_TOTAL = if(all(is.na(STOTUNIT))) NA_real_ else sum(STOTUNIT, na.rm = TRUE),
    SAVLBAD_ANY = if(all(is.na(SAVLBAD_BINARY))) NA_real_ else as.numeric(any(SAVLBAD_BINARY == 1, na.rm = TRUE)),
    was_first_visitor = if(all(is.na(visit_order))) NA else any(visit_order == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    RACE = as.numeric(str_extract(as.character(RACEID), "\\d$")),
    first_visit_datetime = if_else(is.infinite(first_visit_datetime), 
                                 as.POSIXct(NA), first_visit_datetime)
  ) %>%
  select(-RACEID)

cat("Final sales dataset:", nrow(sales_final), "rows (one per tester per control)\n")
sales_with_time %>%
  group_by(CONTROL) %>%
  filter(n_distinct(TESTERID) == 2 & !is.na(am_indicator)) %>%
  distinct(CONTROL) %>%
  nrow()

# Count distinct controls with non-missing outcomes and two testers, by first two letters
outcome_summary <- sales_final %>%
  filter(complete.cases(.)) %>%  # Exclude rows with any NA values in any column
  group_by(CONTROL) %>%
  filter(n() == 2) %>%  # Exactly two testers
  ungroup() %>%
  mutate(control_prefix = substr(CONTROL, 1, 2)) %>%
  group_by(control_prefix) %>%
  summarise(distinct_controls = n_distinct(CONTROL), .groups = 'drop') %>%
  arrange(control_prefix)

  cat("Distinct controls with non-missing outcomes and exactly two testers:\n")
  print(outcome_summary)
  cat("Total distinct controls:", sum(outcome_summary$distinct_controls), "\n")

# Create a separate dataset with one row for each appointment; alternate specification
sales_appointments <- sales_with_time %>%
  group_by(CONTROL) %>%
  arrange(appointment_datetime) %>%
  mutate(visit_order = ifelse(is.na(appointment_datetime), NA, row_number()),  # only rows with non-NA datetime will have visit_order, NA values ordered at the end of each CONTROL
        RACE = as.numeric(str_extract(as.character(RACEID), "\\d$")),) %>% 
  ungroup()

# Export cleaned sales data
write_csv(sales_final, "Data/sales_cleaned.csv")
cat("Exported cleaned sales data to Data/sales_cleaned.csv\n")

# =================================================================================================== #
# TESTER DATA CLEANING
# =================================================================================================== #

cat("=== CLEANING TESTER DATA ===\n")

tester_clean <- tester %>%
  mutate(
    birth_date = map_dbl(TDOB, ~ as.numeric(parse_date_string(.x, is_birth_date = TRUE))),
    birth_date = as.Date(birth_date, origin = "1970-01-01"),
    
    # Calculate age as of January 1, 2012
    age = as.numeric(difftime(as.Date("2012-01-01"), birth_date, units = "days")) / 365.25,
    age = round(age, 0),
    age = if_else(age <= 17, NA_real_, age)  # Remove unrealistic ages
  )

cat("Cleaned birth dates for", nrow(tester_clean), "testers\n")
cat("Valid birth dates:", sum(!is.na(tester_clean$birth_date)), "out of", nrow(tester_clean), "\n")

# Show invalid birth dates without saving to memory
tester_clean %>%
  filter(is.na(birth_date)) %>%
  select(TESTERID, TDOB, birth_date, age) %>%
  {
    cat("Testers with invalid birth dates:\n")
    if(nrow(.) > 0) {
      print(.)
    }
  }

# Note: Only four testers have invalid birth dates due to a lack of data entry, with TDOB = "00/00/0000".

# Merge sales and tester data
sales_and_tester <- sales_final %>%
  left_join(tester_clean, by = "TESTERID")

write_csv(sales_and_tester, "Data/sales_and_tester_merged.csv")
cat("Exported merged data to Data/sales_and_tester_merged.csv (", nrow(sales_and_tester), "rows)\n")

# Alternate specification keeping each appointment as a separate row
sales_and_tester_appointments <- sales_appointments %>%
  left_join(tester_clean, by = "TESTERID")
write_csv(sales_and_tester_appointments, "Data/sales_and_tester_appointments.csv")
cat("Exported sales and tester appointments data to Data/sales_and_tester_appointments.csv (", nrow(sales_and_tester_appointments), "rows)\n")

# =================================================================================================== #
# MERGE SALES, TESTER AND RECHOMES DATA
# =================================================================================================== #
cat("=== CHECKING RECHOMES FOR DUPLICATES ===\n")

# Check for duplicates in rechomes by address variables and deduplicate in one step
rechomes_deduplicated <- rechomes %>%
  {
    cat("Initial rechomes rows:", nrow(.), "\n")
    .
  } %>%
  group_by(CONTROL, TESTERID, HSITEAD, HSITETYP, HUNITNO, HCITY, HSTATE, HZIP) %>%
  slice(1) %>%  # Keep first occurrence of each address combination
  ungroup() %>%
  {
    rows_lost <- nrow(rechomes) - nrow(.)
    cat("After deduplication by address variables:", nrow(.), "rows\n")
    cat("Rows lost due to address duplicates:", rows_lost, "\n")
    .
  }

# Check for duplicate rows in rhgeo by key variables
cat("=== CHECKING FOR DUPLICATES IN RHGEO DATA ===\n")
rhgeo_deduplicated <- rhgeo %>%
  {
    cat("Initial rhgeo rows:", nrow(.), "\n")
    .
  } %>%
  distinct() %>%
  {
    cat("After removing exact duplicates:", nrow(.), "rows\n")
    .
  } %>%
  filter(
    !(
      HSITEAD %in% c("unknown", "did not provide", "UNKNOWN", "MODEL HOME") |
      str_detect(
        str_squish(str_to_lower(HSITEAD)),
        regex("\\b(unknown|not provide(?:d)?|n/?a|model home|no address|none|blank|missing|refuse)\\b")
      ) |
      str_detect(HSITEAD, "^\\s*$|^-+$|^0+$") |
      nchar(trimws(HSITEAD)) <= 3
    )
  ) %>%
  {
    filtered_addresses <- rhgeo %>%
      filter(
        HSITEAD %in% c("unknown", "did not provide", "UNKNOWN", "MODEL HOME") |
        str_detect(
          str_squish(str_to_lower(HSITEAD)),
          regex("\\b(unknown|not provide(?:d)?|n/?a|model home|no address|none|blank|missing|refuse)\\b")
        ) |
        str_detect(HSITEAD, "^\\s*$|^-+$|^0+$") |
        nchar(trimws(HSITEAD)) <= 3
      ) %>%
      pull(HSITEAD) %>%
      unique() %>%
      sort()
    
    cat("After removing misformed addresses:", nrow(.), "rows\n")
    cat("Filtered addresses:", paste(filtered_addresses, collapse = ", "), "\n")
    .
  } %>%
  group_by(CONTROL, TESTERID, HSITEAD, HSITETYP, HUNITNO, HCITY, HSTATE, HZIP) %>%
  slice(1) %>%
  ungroup() %>%
  {
    cat("After keeping first entry for remaining duplicates:", nrow(.), "rows\n")
    .
  }
  # Merge rechomes with rhgeo data to identify missing geographical information
  rechomes_rhgeo <- rechomes_deduplicated %>%
    left_join(select(rhgeo_deduplicated, -SEQRH, -RACEID), 
              by = c("CONTROL", "TESTERID", "HSITEAD", "HSITETYP", "HUNITNO", 
                     "HCITY", "HSTATE", "HZIP")) %>%
    {
      # Count rows in rechomes that are missing in rhgeo
      rechomes_missing_in_rhgeo <- rechomes_deduplicated %>%
        anti_join(rhgeo_deduplicated, 
                  by = c("CONTROL", "TESTERID", "HSITEAD", "HSITETYP", "HUNITNO", 
                         "HCITY", "HSTATE", "HZIP"))

      # Count rows in rhgeo that are missing in rechomes  
      rhgeo_missing_in_rechomes <- rhgeo_deduplicated %>%
        anti_join(rechomes_deduplicated, 
                  by = c("CONTROL", "TESTERID", "HSITEAD", "HSITETYP", "HUNITNO", 
                         "HCITY", "HSTATE", "HZIP"))

      cat("=== RECHOMES-RHGEO MERGE ANALYSIS ===\n")
      cat("Rechomes deduplicated rows:", nrow(rechomes_deduplicated), "\n")
      cat("Rhgeo deduplicated rows:", nrow(rhgeo_deduplicated), "\n")
      cat("Merged rows:", nrow(.), "\n")
      cat("Rechomes rows missing in rhgeo:", nrow(rechomes_missing_in_rhgeo), "\n")
      cat("Rhgeo rows missing in rechomes:", nrow(rhgeo_missing_in_rechomes), "\n")
      cat("Percentage of rechomes missing in rhgeo:", round(nrow(rechomes_missing_in_rhgeo) / nrow(rechomes_deduplicated) * 100, 2), "%\n")
      cat("Percentage of rhgeo missing in rechomes:", round(nrow(rhgeo_missing_in_rechomes) / nrow(rhgeo_deduplicated) * 100, 2), "%\n")
      
      # Return the merged data
      .
    }

cat("=== MERGING SALES, TESTER, RECHOMES & RHGEO DATA ===\n")

# First, refine to analytic sample then merge sales_and_tester with rechomes data

# Analytical sample: non-missing outcomes and exactly 2 testers per control
analytic_sales_tester <- sales_and_tester %>%
  filter(!is.na(STOTUNIT_TOTAL), !is.na(SAVLBAD_ANY)) %>%
  group_by(CONTROL) %>%
  filter(n_distinct(TESTERID) == 2) %>%  # Exactly 2 unique testers per control
  ungroup() %>%
  {
    # Report analytical sample progression
    base_rows <- nrow(sales_and_tester)
    complete_cases <- sum(!is.na(sales_and_tester$STOTUNIT_TOTAL) & !is.na(sales_and_tester$SAVLBAD_ANY))
    analytical_rows <- nrow(.)

    cat("=== ANALYTICAL SAMPLE PROGRESSION ===\n")
    cat("Original sales_and_tester rows:", base_rows, "\n")
    cat("After requiring non-missing outcomes:", complete_cases, "\n") 
    cat("After requiring exactly 2 testers per control:", analytical_rows, "\n")
    cat("Unique controls in analytical sample:", n_distinct(.$CONTROL), "\n")
    
    # Return the filtered data
    .
  }

# Now merge analytical sample with rechomes data
sales_tester_rechomes <- analytic_sales_tester %>%
  left_join(rechomes_rhgeo, by = c("CONTROL", "TESTERID")) %>%
  mutate(has_rechomes_data = rowSums(!is.na(select(., HSITEAD, HZIP))) > 0) %>%
  {
    total_rows <- nrow(.)
    total_controls <- n_distinct(.$CONTROL)
    with_rechomes <- sum(.$has_rechomes_data)
    without_rechomes <- sum(!.$has_rechomes_data)
    
    cat("\n=== RECHOMES MERGE ANALYSIS ===\n")
    cat("Total analytical sample controls:", total_controls, "\n")
    
    # Count CONTROLs with at least one tester with no rechomes data
    controls_with_missing_rechomes <- {
      current_data <- .
      current_data %>%
        group_by(CONTROL) %>%
        summarise(has_missing_rechomes = any(!has_rechomes_data), .groups = 'drop') %>%
        summarise(n_controls = sum(has_missing_rechomes)) %>%
        pull(n_controls) %>%
        as.numeric()
    }
    cat("CONTROLs with at least one tester with no rechomes data:", controls_with_missing_rechomes, "\n")

    # Count controls where neither tester has STOTUNIT_TOTAL == 0
    controls_with_no_zero_recommendations <- {
    current_data <- .
    current_data %>%
      group_by(CONTROL) %>%
      summarise(has_zero_recommendations = any(STOTUNIT_TOTAL == 0, na.rm = TRUE), .groups = 'drop') %>%
      summarise(n_controls = sum(!has_zero_recommendations, na.rm = TRUE)) %>%
      pull(n_controls) %>%
      as.numeric()
    }
    cat("CONTROLs where neither tester has STOTUNIT_TOTAL == 0:", controls_with_no_zero_recommendations, "\n")
    
    # Check for rows with recommended homes but no rechomes data
    recommends_no_rechomes <- sum(!.$has_rechomes_data & .$STOTUNIT_TOTAL > 0, na.rm = TRUE)
    cat("Rows with recommendations but missing rechomes data:", recommends_no_rechomes, "\n")
    cat("Percentage of rows with recommendations but missing rechomes data:", round(recommends_no_rechomes / total_rows * 100, 2), "%\n")

    # Count CONTROLs with at least one tester having recommendations but missing rechomes data
    controls_with_recommendations_no_rechomes <- {
    current_data <- .
    current_data %>%
      group_by(CONTROL) %>%
      summarise(has_missing_rechomes = any(!has_rechomes_data & STOTUNIT_TOTAL > 0, na.rm = TRUE), .groups = 'drop') %>%
      summarise(n_controls = sum(has_missing_rechomes, na.rm = TRUE)) %>%
      pull(n_controls) %>%
      as.numeric()
    }
    cat("CONTROLs with at least one tester having recommendations but missing rechomes data:", controls_with_recommendations_no_rechomes, "\n")
    
    invisible(.)
  } %>%
  {
    # Identify rows that need rechomes data but are missing it
    needs_rechomes_fix <- !.$has_rechomes_data & 
          .$STOTUNIT_TOTAL == 1 & .$SAVLBAD_ANY == 1
    
    missing_data <- .[needs_rechomes_fix, ]
    
    if (nrow(missing_data) > 0) {
    # Find potential donor rows (same CONTROL, HADHOME == 1)
    donor_rows <- rechomes %>%
      filter(CONTROL %in% missing_data$CONTROL, HADHOME == 1) %>%
      select(-TESTERID) %>%  # Remove TESTERID 
      group_by(CONTROL) %>%
      slice(1) %>%  # Take first matching row per CONTROL
      ungroup()

    cat(nrow(missing_data), " of the missing rows above are supposed to only have the advertised home recommended (STOTUNIT_TOTAL=1 & SAVLBAD_ANY=1). We check if their partnered tester recorded the information about the advertised home.\n")
    cat("Found HADHOME == 1 donor rows for", nrow(donor_rows), "of these CONTROLs\n")
    
    # For each missing row, try to fill with donor data
    rows_fixed <- 0
    for (i in which(needs_rechomes_fix)) {
      current_control <- .$CONTROL[i]
      donor_match <- donor_rows[donor_rows$CONTROL == current_control, ]
      
      if (nrow(donor_match) > 0) {
      # Fill in all rechomes columns except CONTROL and TESTERID
      rechomes_cols <- names(donor_match)[!names(donor_match) %in% c("CONTROL")]
      for (col in rechomes_cols) {
        .[[col]][i] <- donor_match[[col]][1]
      }
      # Update has_rechomes_data flag
      .$has_rechomes_data[i] <- TRUE
      rows_fixed <- rows_fixed + 1
      }
    }
    
    cat("Successfully fixed", rows_fixed, "rows with donor rechomes data\n")
    
    if (rows_fixed > 0) {
      # Show sample of actually fixed rows by tracking which ones were successfully fixed
      actually_fixed_indices <- c()
      for (i in which(needs_rechomes_fix)) {
        current_control <- .$CONTROL[i]
        donor_match <- donor_rows[donor_rows$CONTROL == current_control, ]
        if (nrow(donor_match) > 0) {
      actually_fixed_indices <- c(actually_fixed_indices, i)
        }
      }
      
      # Show first 3 actually fixed rows
      sample_indices <- head(actually_fixed_indices, 3)
      fixed_rows_sample <- .[sample_indices, ]
      cat("\nSample of actually fixed rows (showing first 3):\n")
      rechomes_sample_cols <- names(rechomes)[!names(rechomes) %in% c("CONTROL", "TESTERID")][1:5]
      print_cols <- c("CONTROL", "TESTERID", "STOTUNIT_TOTAL", "SAVLBAD_ANY", "has_rechomes_data", 
          rechomes_sample_cols)
      print(fixed_rows_sample[, print_cols], n = Inf, width = Inf)
    }
    
    unfixed_count <- nrow(missing_data) - rows_fixed
    if (unfixed_count > 0) {
      cat("Could not fix", unfixed_count, "rows (no HADHOME == 1 donor found)\n")
    }
    } else {
    cat("No rows found missing rechomes data with STOTUNIT_TOTAL=1 & SAVLBAD_ANY=1\n")
    }
    
    # Return the modified dataframe
    .
  } %>%
  {
    # Recount CONTROLs with missing rechomes data after the fix
    current_data <- .
    controls_with_recommendations_no_rechomes_after_fix <- current_data %>%
      group_by(CONTROL) %>%
      summarise(has_missing_rechomes = any(!has_rechomes_data & STOTUNIT_TOTAL > 0, na.rm = TRUE), .groups = 'drop') %>%
      summarise(n_controls = sum(has_missing_rechomes, na.rm = TRUE)) %>%
      pull(n_controls) %>%
      as.numeric()
    
    cat("CONTROLs with at least one tester having recommendations but missing rechomes data (after fix):", controls_with_recommendations_no_rechomes_after_fix, "\n")
    
    # Return the modified dataframe
    .
  } %>%
  # Subset to rows that have rechomes data
  filter(has_rechomes_data) %>%
  # Only keep controls with exactly two testers (both having rechomes data)
  group_by(CONTROL) %>%
  filter(n_distinct(TESTERID) == 2) %>%
  ungroup() %>%
  {
    total_controls <- n_distinct(.$CONTROL)
    total_rows <- nrow(.)
    
    cat("Final analytical sample with rechomes data:\n")
    cat("Total controls:", total_controls, "\n")
    cat("Total rows:", total_rows, "\n")
    
    # Return the filtered dataset
    .
  }
    

# Export merged dataset
write_csv(sales_tester_rechomes, "Data/sales_tester_rechomes_merged.csv")
cat("Exported merged data to Data/sales_tester_rechomes_merged.csv\n")

### CODE ONLY TESTED UP TO HERE!!!

#---#

# =================================================================================================== #
# GEOCODING INTEGRATION FOR MISSING STFIDs
# =================================================================================================== #

cat("=== GEOCODING MISSING STFIDs ===\n")

# Option to run geocoding for missing STFIDs
# Set this to TRUE if you want to run geocoding (warning: takes time!)
RUN_GEOCODING <- FALSE

if (RUN_GEOCODING) {
  cat("Running geocoding for missing STFIDs...\n")
  
  # Source the geocoding functions
  if (file.exists("address_geocoding.R")) {
    source("address_geocoding.R")
    
    # Run geocoding on the merged dataset
    geocoded_data <- geocode_and_get_stfid(
      data = sales_tester_rhgeo,
      address_col = "HSITEAD",
      city_col = "HCITY", 
      state_col = "HSTATE",
      zip_col = "HZIP",
      existing_stfid_col = "stfid"
    )
    
    # Save geocoded results
    write_csv(geocoded_data, "Data/sales_tester_rhgeo_geocoded.csv")
    cat("Exported geocoded data to Data/sales_tester_rhgeo_geocoded.csv\n")
    
    # Update the main dataset variable for further analysis
    sales_tester_rhgeo <- geocoded_data
    
  } else {
    cat("Geocoding script not found. Please ensure address_geocoding.R exists.\n")
    cat("To enable geocoding, set RUN_GEOCODING <- TRUE and ensure address_geocoding.R is available.\n")
  }
} else {
  cat("Geocoding disabled. To enable:\n")
  cat("1. Set RUN_GEOCODING <- TRUE above\n") 
  cat("2. Ensure address_geocoding.R script exists\n")
  cat("3. Re-run this section\n")
  cat("Note: Geocoding makes many API calls and takes considerable time.\n")
}
# =================================================================================================== #
# ROUGH CODE
# =================================================================================================== #


# Function to examine data structure and key variables
examine_data <- function(data, name) {
  cat("\n=== Examining", name, "===\n")
  cat("Dimensions:", dim(data)[1], "rows x", dim(data)[2], "columns\n")
  cat("Column names:", paste(names(data), collapse = ", "), "\n")
  
  # Check for CONTROL and TESTERID variables
  if ("CONTROL" %in% names(data)) {
    cat("CONTROL variable found - unique values:", length(unique(data$CONTROL)), "\n")
    cat("CONTROL sample values:", paste(head(unique(data$CONTROL), 10), collapse = ", "), "\n")
  } else {
    cat("WARNING: CONTROL variable not found in", name, "\n")
  }
  
  if ("TESTERID" %in% names(data)) {
    cat("TESTERID variable found - unique values:", length(unique(data$TESTERID)), "\n")
    cat("TESTERID sample values:", paste(head(unique(data$TESTERID), 10), collapse = ", "), "\n")
  } else {
    cat("WARNING: TESTERID variable not found in", name, "\n")
  }
  
  # Check for duplicates
  if ("CONTROL" %in% names(data) && "TESTERID" %in% names(data)) {
    duplicate_count <- sum(duplicated(data[c("CONTROL", "TESTERID")]))
    cat("Duplicate CONTROL-TESTERID pairs:", duplicate_count, "\n")
  }
  
  cat("Sample of first 3 rows:\n")
  print(head(data, 3))
  cat("\n")
}

# Read the five main files and filter to sales data immediately
cat("=== READING SAS FILES AND FILTERING TO SALES DATA ===\n")

# Read assignment file and filter to sales + RELEASE="1"
assignment_raw <- read_sas_safe(file.path(raw_data_path, "assignment.sas7bdat"))
assignment <- assignment_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL)) %>%
  filter(RELEASE == "1" & !is.na(TESTERID))
cat("Assignment: ", nrow(assignment_raw), "→", nrow(assignment), "rows after sales + RELEASE='1' filter\n")

# Read taf file and filter to sales
taf_raw <- read_sas_safe(file.path(raw_data_path, "taf.sas7bdat"))
taf <- taf_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL))
cat("TAF: ", nrow(taf_raw), "→", nrow(taf), "rows after sales filter\n")

# Read sales file (already sales data by definition)
sales <- read_sas_safe(file.path(raw_data_path, "sales.sas7bdat"))
cat("Sales: ", nrow(sales), "rows (no filter needed)\n")

# Read tester file (no filtering needed - tester-specific data)
tester <- read_sas_safe(file.path(raw_data_path, "tester_censored.sas7bdat")) %>%
    rename(TESTERID = TesterID)  # Standardize variable name
cat("Tester: ", nrow(tester), "rows\n")

# Read rhgeo file and filter to sales
rhgeo_raw <- read_sas_safe(file.path(raw_data_path, "rhgeo.sas7bdat"))
rhgeo <- rhgeo_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL))
cat("RHGEO: ", nrow(rhgeo_raw), "→", nrow(rhgeo), "rows after sales filter\n")

# Read rechomes file and filter to sales
rechomes_raw <- read_sas_safe(file.path(raw_data_path, "rechomes.sas7bdat"))
rechomes <- rechomes_raw %>%
    filter(grepl("-S[A-Z]-", CONTROL))
cat("RECHOMES: ", nrow(rechomes_raw), "→", nrow(rechomes), "rows after sales filter\n")

# Check if all files were read successfully
files_read <- list(assignment = assignment, taf = taf, sales = sales, tester = tester, rhgeo = rhgeo)
failed_files <- names(files_read)[sapply(files_read, is.null)]

if (length(failed_files) > 0) {
  cat("ERROR: Failed to read the following files:", paste(failed_files, collapse = ", "), "\n")
  stop("Cannot proceed with missing data files")
}

cat("\n=== DATA STRUCTURE EXAMINATION ===\n")

# Examine each dataset
examine_data(assignment, "assignment")
examine_data(taf, "taf") 
examine_data(sales, "sales")
examine_data(tester, "tester")
examine_data(rhgeo, "rhgeo")


# Fix tester variable name to match others
cat("\n=== STANDARDIZING VARIABLE NAMES ===\n")
if ("TesterID" %in% names(tester)) {
  tester <- tester %>% rename(TESTERID = TesterID)
  cat("Renamed 'TesterID' to 'TESTERID' in tester file\n")
}



# ===========================
# CHECK FOR OTHER DUPLICATES
# ===========================
cat("\n=== CHECKING FOR UNEXPECTED DUPLICATES ===\n")

# Check assignment duplicates
assignment_dups <- assignment %>%
  group_by(CONTROL, TESTERID) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)
cat("Assignment duplicates (CONTROL-TESTERID):", nrow(assignment_dups), "\n")

# Check how many testers per control
testers_per_control <- sales %>%
    group_by(CONTROL) %>%
    summarise(num_testers = n_distinct(TESTERID), .groups = 'drop')

cat("\nDistribution of testers per control:\n")
testers_table <- table(testers_per_control$num_testers)
print(testers_table)

# More detailed summary
cat("\nSummary statistics of testers per control:\n")
summary_stats <- summary(testers_per_control$num_testers)
print(summary_stats)

# Visual check of the first few controls with their tester counts
cat("\nSample of controls and their tester counts:\n")
print(head(testers_per_control, 10))

# Check sales duplicates  
sales_dups <- sales %>%
  group_by(CONTROL, TESTERID) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)
cat("Sales duplicates (CONTROL-TESTERID):", nrow(sales_dups), "\n")

# Check tester duplicates
tester_dups <- tester %>%
  group_by(TESTERID) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)
cat("Tester duplicates (TESTERID):", nrow(tester_dups), "\n")

# Check TAF duplicates
taf_dups <- taf %>%
  group_by(CONTROL) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)
cat("TAF duplicates (CONTROL):", nrow(taf_dups), "\n")

# Show TAF duplicates in detail
if(nrow(taf_dups) > 0) {
    cat("\nDetailed examination of TAF duplicates:\n")
    taf_duplicate_controls <- taf_dups$CONTROL
    
    taf_duplicates <- taf %>%
        filter(CONTROL %in% taf_duplicate_controls) %>%
        arrange(CONTROL)
    
    print(taf_duplicates)
    
    # Show differences between duplicates for first few controls
    cat("\nDifferences in first few duplicate controls:\n")
    for(control in head(taf_duplicate_controls, 3)) {
        cat("\nExamining CONTROL:", control, "\n")
        taf_subset <- taf %>% filter(CONTROL == control)
        
        # Find columns with differences
        diff_cols <- names(taf_subset)[sapply(names(taf_subset), function(col) {
            length(unique(taf_subset[[col]])) > 1 && !all(is.na(taf_subset[[col]]))
        })]
        
        if(length(diff_cols) > 0) {
            cat("Columns with differences:", paste(diff_cols, collapse=", "), "\n")
            print(taf_subset[, c("CONTROL", diff_cols)])
        } else {
            cat("All columns have identical values\n")
        }
    }
}

# RHGEO duplicates are expected (multiple recommended homes per tester per control)
# Analyze RHGEO: expected to have multiple entries per CONTROL-TESTERID
rhgeo_summary <- rhgeo %>%
    group_by(CONTROL, TESTERID) %>%
    summarise(recommended_homes = n(), .groups = 'drop')
cat("RHGEO: Average recommended homes per tester per control:", round(mean(rhgeo_summary$recommended_homes), 2), "\n")

# Distribution of testers per control
tester_distribution <- rhgeo %>%
    group_by(CONTROL) %>%
    summarise(testers_count = n_distinct(TESTERID), .groups = 'drop')

cat("\nDistribution of unique TESTERIDs per CONTROL:\n")
print(table(tester_distribution$testers_count))
cat("\nSummary statistics of TESTERIDs per CONTROL:\n")
print(summary(tester_distribution$testers_count))

# Sample of controls with their tester counts
cat("\nSample of controls with their tester counts:\n")
print(head(tester_distribution, 10))

if (nrow(assignment_dups) > 0 || nrow(sales_dups) > 0 || nrow(tester_dups) > 0 || nrow(taf_dups) > 0) {
    cat("\nWARNING: Unexpected duplicates found - please review\n")
}

# ===========================
# MERGING STRATEGY
# ===========================
cat("\n=== IMPLEMENTING MERGING STRATEGY ===\n")

# Start with assignment as base (has CONTROL-TESTERID linkages)
cat("Step 1: Starting with assignment as base dataset...\n")
base_data <- assignment

# Add tester information (match by TESTERID - tester-specific data)
cat("Step 2: Adding tester information by TESTERID...\n")
base_data <- base_data %>%
  left_join(tester, by = "TESTERID", suffix = c("", "_tester"))
cat("  After tester merge:", nrow(base_data), "rows\n")

# Add sales information (match by CONTROL and TESTERID)
cat("Step 3: Adding sales information by CONTROL and TESTERID...\n")
base_data <- base_data %>%
  left_join(sales, by = c("CONTROL", "TESTERID"), suffix = c("", "_sales"))
cat("  After sales merge:", nrow(base_data), "rows\n")

# Add TAF information (match by CONTROL - applies to all testers in that control)
cat("Step 4: Adding TAF information by CONTROL (control-level data)...\n")
base_data <- base_data %>%
  left_join(taf, by = "CONTROL", suffix = c("", "_taf"))
cat("  After TAF merge:", nrow(base_data), "rows\n")

# ===========================
# INVESTIGATE DUPLICATES BEFORE RHGEO MERGE
# ===========================
cat("\n=== INVESTIGATING DUPLICATES IN BASE_DATA BEFORE RHGEO ===\n")

# Check for duplicates in base_data (before rhgeo merge)
base_data_dups <- base_data %>%
  group_by(CONTROL, TESTERID) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)

cat("Base data duplicates (CONTROL-TESTERID) before rhgeo:", nrow(base_data_dups), "\n")

if (nrow(base_data_dups) > 0) {
  cat("Sample of duplicate CONTROL-TESTERID combinations:\n")
  print(head(base_data_dups, 10))
  
  # Examine one specific duplicate case in detail
  sample_dup <- base_data_dups[1,]
  cat("\nDetailed examination of first duplicate case:\n")
  cat("CONTROL:", sample_dup$CONTROL, "TESTERID:", sample_dup$TESTERID, "Count:", sample_dup$count, "\n")
  
  detailed_case <- base_data %>%
    filter(CONTROL == sample_dup$CONTROL, TESTERID == sample_dup$TESTERID) %>%
    select(CONTROL, TESTERID, contains("RELEASE"), contains("DATE"), contains("ID"))
  
  cat("Rows for this case:\n")
  print(detailed_case)
  
  # Check which source files contribute to duplicates
  cat("\nChecking source of duplicates by examining key variables:\n")
  
  # Check if duplicates come from assignment file
  assignment_contribution <- assignment %>%
    group_by(CONTROL, TESTERID) %>%
    summarise(assignment_count = n(), .groups = 'drop') %>%
    filter(assignment_count > 1)
  cat("Duplicates from assignment file:", nrow(assignment_contribution), "\n")
  
  # Check if duplicates come from sales file  
  sales_contribution <- sales %>%
    group_by(CONTROL, TESTERID) %>%
    summarise(sales_count = n(), .groups = 'drop') %>%
    filter(sales_count > 1)
  cat("Duplicates from sales file:", nrow(sales_contribution), "\n")
  
  # Check if duplicates come from tester file
  tester_contribution <- tester %>%
    group_by(TESTERID) %>%
    summarise(tester_count = n(), .groups = 'drop') %>%
    filter(tester_count > 1)
  cat("Duplicates from tester file:", nrow(tester_contribution), "\n")
}

# ===========================
# CREATE FINAL DATASETS
# ===========================

# Dataset 1: WITH rhgeo (each row = recommended home per tester per test)
cat("\n=== CREATING DATASET 1: WITH RHGEO (recommended homes level) ===\n")
merged_with_rhgeo <- base_data %>%
  inner_join(rhgeo, by = c("CONTROL", "TESTERID"), suffix = c("", "_rhgeo"))

cat("Dataset 1 dimensions:", nrow(merged_with_rhgeo), "rows x", ncol(merged_with_rhgeo), "columns\n")
cat("This dataset has one row per recommended home per tester per test\n")

# Dataset 2: WITHOUT rhgeo (each row = test information)
cat("\n=== CREATING DATASET 2: WITHOUT RHGEO (test level) ===\n")
merged_without_rhgeo <- base_data

cat("Dataset 2 dimensions:", nrow(merged_without_rhgeo), "rows x", ncol(merged_without_rhgeo), "columns\n")
cat("This dataset has one row per tester per test\n")

# ===========================
# VALIDATE DATA INTEGRITY
# ===========================
cat("\n=== VALIDATING DATA INTEGRITY ===\n")

# Check coverage
cat("Data coverage validation:\n")
cat("  Dataset 1 (with rhgeo) covers", n_distinct(merged_with_rhgeo$CONTROL), "unique tests\n")
cat("  Dataset 1 (with rhgeo) covers", n_distinct(merged_with_rhgeo$TESTERID), "unique testers\n")
cat("  Dataset 2 (without rhgeo) covers", n_distinct(merged_without_rhgeo$CONTROL), "unique tests\n")
cat("  Dataset 2 (without rhgeo) covers", n_distinct(merged_without_rhgeo$TESTERID), "unique testers\n")

# Check for missing key identifiers
missing_checks <- list(
  "Dataset 1 - Missing CONTROL" = sum(is.na(merged_with_rhgeo$CONTROL) | merged_with_rhgeo$CONTROL == ""),
  "Dataset 1 - Missing TESTERID" = sum(is.na(merged_with_rhgeo$TESTERID) | merged_with_rhgeo$TESTERID == ""),
  "Dataset 2 - Missing CONTROL" = sum(is.na(merged_without_rhgeo$CONTROL) | merged_without_rhgeo$CONTROL == ""),
  "Dataset 2 - Missing TESTERID" = sum(is.na(merged_without_rhgeo$TESTERID) | merged_without_rhgeo$TESTERID == "")
)

cat("\nMissing key identifiers:\n")
for (check_name in names(missing_checks)) {
  cat(" ", check_name, ":", missing_checks[[check_name]], "\n")
}

# ===========================
# EXPORT DATASETS
# ===========================
cat("\n=== EXPORTING DATASETS ===\n")

# Create Data directory if it doesn't exist
if (!dir.exists("Data")) {
  dir.create("Data")
}

# Export to CSV
write_csv(merged_with_rhgeo, "Data/merged_hds_with_rhgeo.csv")
write_csv(merged_without_rhgeo, "Data/merged_hds_without_rhgeo.csv")

# Export to RDS
saveRDS(merged_with_rhgeo, "Data/merged_hds_with_rhgeo.rds")
saveRDS(merged_without_rhgeo, "Data/merged_hds_without_rhgeo.rds")

cat("Successfully exported:\n")
cat("  Data/merged_hds_with_rhgeo.csv (", nrow(merged_with_rhgeo), "rows)\n")
cat("  Data/merged_hds_with_rhgeo.rds (", nrow(merged_with_rhgeo), "rows)\n")
cat("  Data/merged_hds_without_rhgeo.csv (", nrow(merged_without_rhgeo), "rows)\n")
cat("  Data/merged_hds_without_rhgeo.rds (", nrow(merged_without_rhgeo), "rows)\n")

# Create sales type indicator for reference
base_data <- base_data %>%
  mutate(
    sales_type = case_when(
      grepl("-SA-", CONTROL) ~ "SA_Asian",
      grepl("-SB-", CONTROL) ~ "SB_Black", 
      grepl("-SH-", CONTROL) ~ "SH_Hispanic",
      TRUE ~ "Other_Sales"
    )
  )

# Show final breakdown
sales_breakdown <- base_data %>%
  count(sales_type, sort = TRUE)
cat("Sales type breakdown:\n")
print(sales_breakdown)

cat("\n=== DATA CLEANING AND MERGING COMPLETE ===\n")
cat("Filtered to sales data only (SA/SB/SH) with RELEASE='1'\n")
cat("Final datasets contain", nrow(merged_with_rhgeo), "and", nrow(merged_without_rhgeo), "rows respectively\n")


