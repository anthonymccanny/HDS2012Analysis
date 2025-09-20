cat("\n=== MARKET AND RACE TEST DISTRIBUTION ANALYSIS ===\n")

# Extract market shortform (first two characters) and race test type (5th character)
# Use distinct CONTROL values to avoid counting the same test multiple times
# Create a dataframe with unique CONTROL values and count of unique TESTERIDs
rechomes_market_race <- rechomes_sales %>%
  group_by(CONTROL) %>%
  summarise(unique_testers = n_distinct(TESTERID)) %>%
  mutate(
    market = substr(CONTROL, 1, 2),
    race_test = substr(CONTROL, 5, 5),
    race_test_label = case_when(
      race_test == "A" ~ "Asian",
      race_test == "B" ~ "Black",
      race_test == "H" ~ "Hispanic",
      TRUE ~ "Other"
    )
  )

# Display summary of tester counts
cat("Number of unique TESTERID values per CONTROL (first 10 rows):\n")
print(head(arrange(rechomes_market_race, desc(unique_testers)), 10))
cat("...\n")

# Overall market and race test distribution by CONTROL
market_race_dist <- rechomes_market_race %>%
  count(market, race_test_label) %>%
  arrange(market, race_test_label)

cat("Overall market and race test distribution (unique CONTROL values):\n")
print(market_race_dist)

market_race_dist_2 <- rechomes_market_race %>%
  filter(unique_testers == 2) %>%
  count(market, race_test_label) %>%
  arrange(market, race_test_label)
  

# Market distribution summary
market_summary <- rechomes_market_race %>%
  count(market) %>%
  arrange(desc(n)) %>%
  mutate(percentage = round(n/sum(n)*100, 2))

cat("\nOverall market distribution (unique CONTROL values):\n")
print(market_summary)

# Race test distribution summary
race_summary <- rechomes_market_race %>%
  count(race_test_label) %>%
  arrange(desc(n)) %>%
  mutate(percentage = round(n/sum(n)*100, 2))

cat("\nOverall race test distribution (unique CONTROL values):\n")
print(race_summary)


# Import salesflags data from SAS file
library(haven)
salesflags <- read_sas("Data/HDS_technical_data/salesflags.sas7bdat")

# Display basic information about the imported data
cat("\n=== SALESFLAGS DATA OVERVIEW ===\n")
cat("Dimensions:", dim(salesflags)[1], "rows,", dim(salesflags)[2], "columns\n")
cat("Column names:", paste(names(salesflags), collapse=", "), "\n\n")

# Display the first few rows
cat("First few rows of salesflags data:\n")
print(head(salesflags))