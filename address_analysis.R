# Address Quality Analysis for Missing RECHOMES Data
library(haven)
library(dplyr)

# Read datasets  
rechomes <- read_sas('Data/HDS_raw_data/rechomes.sas7bdat', encoding='latin1')
rhgeo <- read_sas('Data/HDS_raw_data/rhgeo.sas7bdat')

# Filter to sales
rechomes_sales <- rechomes %>% filter(grepl('-S[A-Z]-', CONTROL))
rhgeo_sales <- rhgeo %>% filter(grepl('-S[A-Z]-', CONTROL))

# Find missing pairs
rechomes_pairs <- rechomes_sales %>% select(CONTROL, TESTERID) %>% distinct()
rhgeo_pairs <- rhgeo_sales %>% select(CONTROL, TESTERID) %>% distinct()
missing_pairs <- anti_join(rechomes_pairs, rhgeo_pairs, by = c('CONTROL', 'TESTERID'))

# Get address data for missing pairs
missing_addresses <- missing_pairs %>%
  left_join(rechomes_sales, by = c('CONTROL', 'TESTERID')) %>%
  select(CONTROL, TESTERID, SEQRH, HSITEAD, HCITY, HSTATE, HZIP)

# Address quality assessment
cat('MISSING PAIRS ADDRESS ANALYSIS\n')
cat('Total missing pairs:', nrow(missing_pairs), '\n')
cat('Total missing address records:', nrow(missing_addresses), '\n\n')

# Check completeness
complete_count <- missing_addresses %>%
  filter(!is.na(HSITEAD) & !is.na(HCITY) & !is.na(HSTATE) & !is.na(HZIP) &
         HSITEAD != '' & HCITY != '' & HSTATE != '' & HZIP != '') %>%
  nrow()

cat('Complete addresses (all fields):', complete_count, 'of', nrow(missing_addresses), '\n')
cat('Completeness rate:', round(complete_count/nrow(missing_addresses)*100, 1), '%\n\n')

# Field-by-field analysis
fields <- c('HSITEAD', 'HCITY', 'HSTATE', 'HZIP')
for(field in fields) {
  missing_count <- sum(is.na(missing_addresses[[field]]) | missing_addresses[[field]] == '')
  cat(field, 'missing:', missing_count, '(', round(missing_count/nrow(missing_addresses)*100, 1), '%)\n')
}

# Sample addresses
cat('\nSample addresses (first 20):\n')
sample_addresses <- missing_addresses %>%
  head(20) %>%
  mutate(
    complete = !is.na(HSITEAD) & !is.na(HCITY) & !is.na(HSTATE) & !is.na(HZIP) &
               HSITEAD != '' & HCITY != '' & HSTATE != '' & HZIP != ''
  )
print(sample_addresses)

# State distribution
cat('\nState distribution of missing addresses:\n')
state_dist <- missing_addresses %>%
  filter(!is.na(HSTATE) & HSTATE != '') %>%
  count(HSTATE, sort = TRUE)
print(state_dist)

# Examples of good and poor addresses
cat('\nExamples of COMPLETE addresses:\n')
good_addresses <- missing_addresses %>%
  filter(!is.na(HSITEAD) & !is.na(HCITY) & !is.na(HSTATE) & !is.na(HZIP) &
         HSITEAD != '' & HCITY != '' & HSTATE != '' & HZIP != '') %>%
  head(10)
print(good_addresses)

cat('\nExamples of INCOMPLETE addresses:\n')
poor_addresses <- missing_addresses %>%
  filter(is.na(HSITEAD) | is.na(HCITY) | is.na(HSTATE) | is.na(HZIP) |
         HSITEAD == '' | HCITY == '' | HSTATE == '' | HZIP == '') %>%
  head(10)
print(poor_addresses)

# Export for geocoding
complete_for_geocoding <- missing_addresses %>%
  filter(!is.na(HSITEAD) & !is.na(HCITY) & !is.na(HSTATE) & !is.na(HZIP) &
         HSITEAD != '' & HCITY != '' & HSTATE != '' & HZIP != '') %>%
  mutate(
    full_address = paste(HSITEAD, HCITY, HSTATE, HZIP, sep = ', ')
  )

write.csv(complete_for_geocoding, 'Data/addresses_for_geocoding.csv', row.names = FALSE)
cat('\nComplete addresses saved to Data/addresses_for_geocoding.csv\n')
cat('Records ready for geocoding:', nrow(complete_for_geocoding), '\n')