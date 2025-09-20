# Load required libraries
library(lfe)
library(dplyr)
library(tidyr)

# Import the data
data <- read.csv("Data/sales_and_tester_merged.csv")

# Subset the data
filtered_data <- data %>%
    filter(!is.na(was_first_visitor)) %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup() %>%
    mutate(
        RACE = as.factor(RACE),
        CONTROL = as.factor(CONTROL),
        SBEGAM_FIRST = as.factor(SBEGAM_FIRST),
        TPEGAI = as.factor(TPEGAI),
        THHEGAI = as.factor(THHEGAI),
        TSEX = as.factor(TSEX),
        age = as.numeric(age),
        THIGHEDU = as.factor(THIGHEDU)
    )

# Create missingness table for regression variables
missingness_vars <- c("STOTUNIT_TOTAL", "SAVLBAD_ANY", "STOTUNIT_FIRST", "SAVLBAD_FIRST",
                       "RACE", "was_first_visitor", "SBEGAM_FIRST", "TPEGAI", "THHEGAI", 
                       "TSEX", "age", "THIGHEDU", "CONTROL")

missingness_table <- filtered_data %>%
    select(all_of(missingness_vars)) %>%
    summarise(across(everything(), ~sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
    mutate(
        Total_Observations = nrow(filtered_data),
        Missing_Percentage = round((Missing_Count / Total_Observations) * 100, 2),
        Complete_Count = Total_Observations - Missing_Count
    ) %>%
    arrange(desc(Missing_Count))

cat("\n--- Missingness Table for Regression Variables ---\n")
print(missingness_table)

summary(data$SBEGAM_FIRST)

# Run regressions with felm
reccomended_total <- felm(STOTUNIT_TOTAL ~ RACE + was_first_visitor + SBEGAM_FIRST +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL, data = filtered_data)

available_any <- felm(SAVLBAD_ANY ~ RACE + was_first_visitor + SBEGAM_FIRST +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL, data = filtered_data)

reccomended_first <- felm(STOTUNIT_FIRST ~ RACE + was_first_visitor + SBEGAM_FIRST +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL, data = filtered_data)

available_first <- felm(SAVLBAD_FIRST ~ RACE + was_first_visitor + SBEGAM_FIRST +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL, data = filtered_data)

# Display results
summary(reccomended_total)
summary(available_any)
summary(reccomended_first)
summary(available_first)

# Display number of observations used in each regression
cat("Observations (reccomended_total):", reccomended_total$N, "\n")
cat("Observations (available_any):", available_any$N, "\n")
cat("Observations (reccomended_first):", reccomended_first$N, "\n")
cat("Observations (available_first):", available_first$N, "\n")

# Alternate specification keeping each appointment as a separate row
appointments_data <- read.csv("Data/sales_and_tester_appointments.csv")


# Summarize distribution of testers per control group in appointments data
tester_distribution <- data %>%
    group_by(CONTROL) %>%
    summarise(n_testers = n_distinct(TESTERID)) %>%
    count(n_testers, name = "n_control_groups")

cat("\n--- Distribution of testers per control group (appointments data) ---\n")
print(tester_distribution)
# Subset the appointments data
filtered_appointments <- appointments_data %>%
    filter(!is.na(visit_order)) %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup() %>%
# Convert variables to appropriate types for appointments data 
    mutate(
        RACE = as.factor(RACE),
        CONTROL = as.factor(CONTROL),
        SBEGAM = as.factor(SBEGAM),
        TPEGAI = as.factor(TPEGAI),
        THHEGAI = as.factor(THHEGAI),
        TSEX = as.factor(TSEX),
        visit_order = as.factor(visit_order),
        age = as.numeric(age),
        THIGHEDU = as.factor(THIGHEDU)
    )

# Run regressions with felm for appointments data
recommended_units <- felm(STOTUNIT ~ RACE + visit_order + SBEGAM +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL, data = filtered_appointments)

available_binary <- felm(SAVLBAD_BINARY ~ RACE + visit_order + SBEGAM +
                        TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                        CONTROL, data = filtered_appointments)

# Display results for appointments analysis
cat("\n--- Appointments-level Analysis ---\n")
summary(recommended_units)
summary(available_binary)

# Display number of observations used in each regression
cat("Observations (recommended_units):", recommended_units$N, "\n")
cat("Observations (available_binary):", available_binary$N, "\n")


# Analysis of likelihood of getting invited back for a second appointment by race

# Count appointments per tester in each control group
appointment_counts <- appointments_data %>%
    group_by(CONTROL, TESTERID) %>%
    summarise(n_appointments = n(), .groups = 'drop') %>%
    left_join(appointments_data %>% 
              select(CONTROL, TESTERID, RACE, TPEGAI, THHEGAI, TSEX, age, THIGHEDU) %>%
              distinct(), 
              by = c("CONTROL", "TESTERID"))

# Create binary indicator for getting second appointment (2+ appointments)
appointment_counts$got_second_appointment <- as.numeric(appointment_counts$n_appointments >= 2)

# Filter to control groups with exactly 2 testers
callback_data <- appointment_counts %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup() %>%
    mutate(
        RACE = as.factor(RACE),
        CONTROL = as.factor(CONTROL),
        TSEX = as.factor(TSEX),
        THIGHEDU = as.factor(THIGHEDU),
        TPEGAI = as.numeric(TPEGAI),
        THHEGAI = as.numeric(THHEGAI),
        age = as.numeric(age)
    )

# Run regression for likelihood of second appointment
callback_regression <- felm(got_second_appointment ~ RACE + TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                           CONTROL, data = callback_data)

# Display results
cat("\n--- Callback Analysis: Likelihood of Second Appointment ---\n")
summary(callback_regression)
cat("Observations (callback_regression):", callback_regression$N, "\n")

# Summary statistics by race
callback_summary <- callback_data %>%
    group_by(RACE) %>%
    summarise(
        n_testers = n(),
        got_callback = sum(got_second_appointment),
        callback_rate = mean(got_second_appointment),
        .groups = 'drop'
    )

cat("\n--- Callback Rates by Race ---\n")
print(callback_summary)

# Extract intercept from callback regression
intercept <- callback_regression$coefficients[1]
cat("\nIntercept from callback regression:", intercept, "\n")

# Calculate average number of white folks (RACE=1) who got second appointment
white_callback_info <- callback_data %>%
    filter(RACE == 1) %>%
    summarise(
        total_white_testers = n(),
        white_got_callback = sum(got_second_appointment),
        avg_white_callbacks = mean(got_second_appointment)
    )

cat("\n--- White Testers (RACE=1) Callback Information ---\n")
cat("Total white testers:", white_callback_info$total_white_testers, "\n")
cat("Number who got second appointment:", white_callback_info$white_got_callback, "\n")
cat("Average callback rate for white testers:", white_callback_info$avg_white_callbacks, "\n")
