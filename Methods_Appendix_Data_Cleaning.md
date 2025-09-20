# Extended Methods Appendix: Data Cleaning Procedures for HDS (2012) Analysis

## Overview

This document details the comprehensive data cleaning procedures applied to the Housing Discrimination Study (HDS) 2012 dataset. Our cleaning process transforms raw SAS data files into analysis-ready datasets, with particular emphasis on handling complex date/time formats, managing duplicate entries, and ensuring data integrity across multiple linked files.

## Data Sources and Initial Structure

### Raw Data Files

The analysis begins with five primary SAS data files (.sas7bdat format):

1. **assignment.sas7bdat**: Links testers to specific test assignments
2. **taf.sas7bdat**: Test Assignment Form data containing test-level characteristics
3. **sales.sas7bdat**: Sales test visit records with appointment times and outcomes
4. **tester_censored.sas7bdat**: Tester demographic and background information
5. **rhgeo.sas7bdat**: Recommended homes geocoded data for each test

### Data Import Function

We use a custom import function that handles UTF-8 encoding and converts empty strings to NA:

```r
import_sas <- function(file_path) {
  tryCatch({
    data <- read_sas(file_path, encoding = "UTF-8")
    data <- data %>% mutate(across(everything(), ~ ifelse(.x == "", NA, .x)))
    cat("Successfully imported", basename(file_path), ":", nrow(data), "rows x", ncol(data), "columns\n")
    return(data)
  }, error = function(e) {
    stop("Failed to read ", basename(file_path), " with UTF-8 encoding: ", e$message)
  })
}
```

### Initial Data Filtering

Given the HDS 2012 study's scope encompassing both rental and sales tests, we immediately filter all datasets to include only sales tests. Sales tests are identified by control numbers containing the pattern "-S[A-Z]-", specifically:
- SA: Asian testers
- SB: Black testers  
- SH: Hispanic testers

```r
# Read assignment file and filter to sales + RELEASE="1"
assignment_raw <- import_sas(file.path(raw_data_path, "assignment.sas7bdat"))
assignment <- assignment_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL)) %>%  # Filter to sales tests (SA/SB/SH)
  filter(RELEASE == "1" & !is.na(TESTERID))  # Only released tests with valid tester IDs

# Read taf file and filter to sales
taf_raw <- import_sas(file.path(raw_data_path, "taf.sas7bdat"))
taf <- taf_raw %>%
  filter(grepl("-S[A-Z]-", CONTROL))  # Filter to sales tests only
```

For the assignment file, we apply an additional filter to include only released tests (RELEASE="1") with valid tester IDs, ensuring we analyze only completed, valid test assignments.

## Key Data Cleaning Challenges and Solutions

### 1. Date Format Standardization

The raw data contains appointment dates (SAPPTD) and birth dates (TDOB) in highly inconsistent formats, requiring a sophisticated parsing approach.

#### Date Format Variations Encountered

Our analysis identified over 15 distinct date format patterns, including:

- Standard formats: "MM/DD/YYYY", "MM/DD/YY"
- No-separator formats: "MMDDYYYY", "MMDDYY"
- Ambiguous formats: "MM/YY", "DD/MM", "MMDD/YY"
- Non-standard separators: dashes, dots, spaces
- Invalid dates: "6/31/12" (June 31st), "02/29/2011" (leap year error)
- Typos: "08/18/7977", "03/15/2948", "01/08/1064"

#### Parsing Strategy

We developed a comprehensive `parse_date_string()` function that:

1. **Standardizes separators**: Converts all separators (dashes, dots, spaces) to forward slashes
2. **Applies manual corrections**: Maintains a lookup table for known invalid dates
3. **Uses pattern matching**: Tests input against regex patterns in order of specificity
4. **Handles century ambiguity**: 
   - For birth dates: Assumes 19xx for 2-digit years (testers born 1900s)
   - For appointment dates: Assumes 20xx for 2-digit years (tests conducted 2011-2012)
5. **Validates plausibility**: Ensures parsed dates fall within reasonable ranges

```r
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
  
  # ... [pattern matching logic continues]
}
```

#### Special Cases

For ambiguous formats like "MM/YY" or "DD/MM", the function employs contextual logic:
- Numbers >12 must be days (cannot be months)
- For appointment dates without clear structure, defaults to 2011 (study year)
- For birth dates, validates resulting age would be 18-90 in 2011

```r
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
    # Treat as DD/MM/2011 for appointment dates
    return(as.Date(sprintf("%02d/%02d/2011", second_num, first_num), format = "%m/%d/%Y"))
  }
  # ... [additional logic for other scenarios]
}
```

### 2. Appointment Date Correction

Some appointment dates fell outside the study period (July 2011 - October 2012), indicating data entry errors.

#### Correction Algorithm

The `correct_appointment_date()` function:

1. Identifies dates outside the valid study period
2. For each invalid date, finds other appointments with the same CONTROL number
3. Uses the year from valid appointments within the same test
4. If the corrected year matches the original, adjusts the month instead
5. Sets to NA if no valid reference date exists for that CONTROL

```r
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
    # ... [logging output]
    }
  }
  return(df)
}
```

This approach preserves the day and time information while correcting obvious year/month entry errors, maintaining data integrity within test groups.

### 3. Time Format Parsing

Appointment times (SBEGH) exhibit similar format diversity, complicated by AM/PM indicators (SBEGAM).

#### Time Format Variations

- Military format: "0315", "1430"
- 3-digit format: "357" (interpreted as 3:57)
- Separated formats: "3:15", "3;15", "3.15"
- Invalid entries: "N/A", non-numeric values

#### AM/PM Indicator Corrections

The `parse_time_string()` function handles several edge cases:

1. **Missing indicators**: Infers AM/PM based on hour (9-12 likely AM, 1-8 likely PM)
2. **Obviously wrong indicators**: 
   - Times like 1:00-5:00 marked as AM are changed to PM
   - Times like 10:00-11:00 marked as PM are changed to AM
3. **Invalid times**: Sets to 00:00 when unparseable

```r
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
  }
  
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
    
    # Convert to 24-hour format
    if (!is.na(hour) && am_pm_indicator == 2 && hour < 12) {
      hour <- hour + 12  # PM hours (1-11) + 12
    }
  }
  
  return(list(hour = hour, minute = minute))
}
```

### 4. Invalid Time Correction

Many appointments showed exactly 00:00:00 (midnight), which is implausible for real estate showings.

The `correct_invalid_times()` function:
1. Identifies all midnight appointments
2. Checks if other appointments exist on the same day for the same CONTROL
3. If duplicates exist, sets the midnight time to NA (indicating time entry error)
4. Preserves midnight times only when they're the sole appointment that day

```r
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
```

### 5. Visit Order Determination

Since testers often made multiple visits to the same agent, we need to establish chronological order.

Our approach:
1. Groups records by CONTROL
2. Sorts by appointment_datetime
3. Assigns visit_order using row numbers
4. Handles NA datetimes by placing them last

```r
# Determine visit order and aggregate
sales_final <- sales_with_time %>%
  group_by(CONTROL) %>%
  arrange(appointment_datetime) %>%
  mutate(visit_order = row_number()) %>%
  ungroup()
```

### 6. Data Aggregation

The final sales dataset aggregates multiple visits per tester-control combination:

- **First visit metrics**: SBEGAM_FIRST, STOTUNIT_FIRST, SAVLBAD_FIRST
- **Aggregate metrics**: num_visits, STOTUNIT_TOTAL, SAVLBAD_ANY
- **Temporal metrics**: first_visit_datetime, was_first_visitor

```r
sales_final <- sales_with_time %>%
  group_by(CONTROL) %>%
  arrange(appointment_datetime) %>%
  mutate(visit_order = row_number()) %>%
  ungroup() %>%
  group_by(CONTROL, TESTERID) %>%
  summarise(
    RACEID = first(RACEID),
    SBEGAM_FIRST = first(SBEGAM[which.min(visit_order)]),
    STOTUNIT_FIRST = first(STOTUNIT[which.min(visit_order)]),
    SAVLBAD_FIRST = first(SAVLBAD_BINARY[which.min(visit_order)]),
    first_visit_datetime = min(appointment_datetime, na.rm = TRUE),
    num_visits = n(),
    STOTUNIT_TOTAL = sum(STOTUNIT, na.rm = TRUE),
    SAVLBAD_ANY = as.numeric(any(SAVLBAD_BINARY == 1, na.rm = TRUE)),
    was_first_visitor = any(visit_order == 1, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    RACE = as.numeric(str_extract(as.character(RACEID), "\\d$")),
    first_visit_datetime = if_else(is.infinite(first_visit_datetime), 
                                 as.POSIXct(NA), first_visit_datetime)
  ) %>%
  select(-RACEID)
```

This aggregation preserves both visit-specific and cumulative information for analysis.

## Tester Data Processing

### Birth Date Parsing

Tester birth dates use the same parsing function with `is_birth_date = TRUE`, applying century logic appropriate for birth years.

### Age Calculation

Ages are calculated as of January 1, 2012 (mid-study period):
- Uses precise calculation (days / 365.25)
- Removes implausible ages (<18 years old)
- Rounds to nearest whole year

```r
tester_clean <- tester %>%
  mutate(
    birth_date = map_dbl(TDOB, ~ as.numeric(parse_date_string(.x, is_birth_date = TRUE))),
    birth_date = as.Date(birth_date, origin = "1970-01-01"),
    
    # Calculate age as of January 1, 2012
    age = as.numeric(difftime(as.Date("2012-01-01"), birth_date, units = "days")) / 365.25,
    age = round(age, 0),
    age = if_else(age <= 17, NA_real_, age)  # Remove unrealistic ages
  )
```

## Data Validation and Quality Checks

Throughout the cleaning process, we implement numerous validation steps:

1. **Row count tracking**: Documents data loss at each filtering step
2. **Date/time summaries**: Reports counts of NA values, unusual times, midnight appointments
3. **Duplicate detection**: Identifies cases requiring special handling
4. **Cross-file validation**: Ensures CONTROL and TESTERID consistency

### Sales Data Processing and Validation

The sales data undergoes several transformations:

```r
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
  correct_appointment_date() %>%
  mutate(
    time_parsed = map2(SBEGH, SBEGAM, parse_time_string),
    hour_24 = map_dbl(time_parsed, ~ .x$hour),
    minute_part = map_dbl(time_parsed, ~ .x$minute),
    
    appointment_datetime = as.POSIXct(
      paste(appointment_date, sprintf("%02d:%02d:00", hour_24, minute_part)),
      format = "%Y-%m-%d %H:%M:%S"
    )
  ) %>%
  select(-time_parsed, -hour_24, -minute_part) %>%
  correct_invalid_times()
```

### Validation Output

The script provides detailed summaries of data quality issues:

```r
sales_with_time %>%
  summarise(
    na_dates = sum(is.na(appointment_date)),
    na_datetimes = sum(is.na(appointment_datetime)),
    unusual_times = sum(!is.na(appointment_datetime) & 
                       (hour(appointment_datetime) < 6 | hour(appointment_datetime) >= 21)),
    midnight_times = sum(!is.na(appointment_datetime) & 
                        hour(appointment_datetime) == 0 & minute(appointment_datetime) == 0)
  )
```

## Output Files

The cleaning process produces two primary outputs:

1. **sales_cleaned.csv**: Aggregated sales data (one row per tester-control)
2. **sales_and_tester_merged.csv**: Sales data merged with tester demographics

Both files represent fully cleaned, validated datasets ready for statistical analysis.

## Reproducibility Notes

This cleaning approach prioritizes:
- **Transparency**: All transformations are explicit and documented
- **Preservation**: Original data is never modified; all changes create new variables
- **Validation**: Each step includes quality checks and progress reporting
- **Flexibility**: Functions handle edge cases gracefully with appropriate warnings

The modular function design allows researchers to adapt the cleaning process for similar administrative datasets with complex, human-entered date/time fields.