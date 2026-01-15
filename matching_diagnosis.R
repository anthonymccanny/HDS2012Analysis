# matching_diagnosis.R
# Validates school matching methodology against Christensen & Timmins (2022) replication data
# Tests our spatial matching approach using C&T's exact coordinates for recommended properties

library(tidyverse)
library(sf)

cat("=== SCHOOL MATCHING VALIDATION ===\n\n")
cat("This script validates our school matching methodology by applying it to\n")
cat("C&T's exact coordinates and comparing results with their published scores.\n\n")

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("Loading data sources...\n")

# Load SABS 2015-16 boundaries
primary <- st_read("Data/Non_HDS/SABS/SABS_1516_SchoolLevels/SABS_1516_Primary.shp",
                   quiet = TRUE)
middle <- st_read("Data/Non_HDS/SABS/SABS_1516_SchoolLevels/SABS_1516_Middle.shp",
                  quiet = TRUE)

# Load SEDA v3 scores
seda <- read_csv("Data/Non_HDS/SEDA_v3/seda_school_pool_cs_v30.csv",
                 show_col_types = FALSE) %>%
  mutate(ncessch = as.character(ncessch))

# Calculate elementary and middle school scores
elem_scores <- seda %>%
  filter(midgrd %in% c(4, 4.5, 5, 5.5)) %>%
  group_by(ncessch) %>%
  summarise(elementary_school_score = mean(mn_avg_ol, na.rm = TRUE),
            .groups = "drop")

middle_scores <- seda %>%
  filter(midgrd %in% c(6.5, 7, 7.5)) %>%
  group_by(ncessch) %>%
  summarise(middle_school_score = mean(mn_avg_ol, na.rm = TRUE),
            .groups = "drop")

# Load C&T recommended properties with coordinates
ct_recs <- readRDS("Data/HuD_Replication/Final Data Sets/recsprocessed_JPE.rds") %>%
  select(CONTROL, TESTERID, SEQRH, Latitude, Longitude, RecPrice, Sqft_Rec)

cat(sprintf("  C&T recommended properties: %d\n", nrow(ct_recs)))

# Load C&T scores (these are property-level, not pair-level)
ct_hud <- readRDS("Data/HuD_Replication/Final Data Sets/HUDprocessed_JPE_testscores_042021.rds")

ct_scores <- ct_hud %>%
  select(CONTROL, TESTERID, RecPrice, Sqft_Rec,
         ct_elem_score = mn_avg_ol_elem_Rec,
         ct_middle_score = mn_avg_ol_middle_Rec) %>%
  distinct()

# Match scores to properties using property identifiers
ct_with_scores <- ct_recs %>%
  left_join(ct_scores,
            by = c("CONTROL", "TESTERID", "RecPrice", "Sqft_Rec"),
            relationship = "many-to-many") %>%
  group_by(CONTROL, TESTERID, SEQRH) %>%
  slice(1) %>%
  ungroup()

cat(sprintf("  Properties with C&T scores: %d\n\n", nrow(ct_with_scores)))

# ==============================================================================
# APPLY OUR MATCHING TO C&T COORDINATES
# ==============================================================================

cat("Applying our spatial matching to C&T's coordinates...\n")

# Convert to spatial object
ct_sf <- st_as_sf(ct_with_scores,
                  coords = c("Longitude", "Latitude"),
                  crs = 4326)
ct_sf <- st_transform(ct_sf, st_crs(primary))

# Match to elementary schools
our_elem <- st_join(ct_sf, primary, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(CONTROL, TESTERID, SEQRH) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(elem_scores, by = "ncessch") %>%
  select(CONTROL, TESTERID, SEQRH,
         our_elem_id = ncessch,
         our_elem_score = elementary_school_score)

# Match to middle schools
our_middle <- st_join(ct_sf, middle, join = st_within) %>%
  st_drop_geometry() %>%
  group_by(CONTROL, TESTERID, SEQRH) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(middle_scores, by = "ncessch") %>%
  select(CONTROL, TESTERID, SEQRH,
         our_middle_id = ncessch,
         our_middle_score = middle_school_score)

# Combine with C&T scores
comparison <- ct_with_scores %>%
  select(CONTROL, TESTERID, SEQRH, ct_elem_score, ct_middle_score) %>%
  left_join(our_elem, by = c("CONTROL", "TESTERID", "SEQRH")) %>%
  left_join(our_middle, by = c("CONTROL", "TESTERID", "SEQRH"))

cat("  Matching complete.\n\n")

# ==============================================================================
# CALCULATE VALIDATION STATISTICS
# ==============================================================================

cat("===================================================================\n")
cat("VALIDATION RESULTS\n")
cat("===================================================================\n\n")

# Elementary schools - base on ALL properties with C&T scores
elem_with_ct <- comparison %>%
  filter(!is.na(ct_elem_score))

n_elem_total <- nrow(elem_with_ct)
n_elem_we_matched <- sum(!is.na(elem_with_ct$our_elem_score))
n_elem_exact <- sum(abs(elem_with_ct$our_elem_score - elem_with_ct$ct_elem_score) < 0.001,
                     na.rm = TRUE)

# Correlation only on properties both matched
elem_both <- elem_with_ct %>% filter(!is.na(our_elem_score))
cor_elem <- cor(elem_both$our_elem_score, elem_both$ct_elem_score)

pct_we_matched_elem <- 100 * n_elem_we_matched / n_elem_total
pct_exact_of_total_elem <- 100 * n_elem_exact / n_elem_total
pct_exact_of_matched_elem <- 100 * n_elem_exact / n_elem_we_matched

cat("ELEMENTARY SCHOOLS:\n")
cat(sprintf("  Total properties with C&T scores:              %d\n", n_elem_total))
cat(sprintf("  Properties we successfully matched:            %d (%.1f%% of total)\n",
            n_elem_we_matched, pct_we_matched_elem))
cat(sprintf("  Exact score matches (within 0.001):            %d (%.1f%% of total)\n",
            n_elem_exact, pct_exact_of_total_elem))
cat(sprintf("  Exact matches as %% of our successful matches:  %.1f%%\n",
            pct_exact_of_matched_elem))
cat(sprintf("  Correlation (for matched properties):          r = %.4f\n\n", cor_elem))

# Middle schools - base on ALL properties with C&T scores
middle_with_ct <- comparison %>%
  filter(!is.na(ct_middle_score))

n_middle_total <- nrow(middle_with_ct)
n_middle_we_matched <- sum(!is.na(middle_with_ct$our_middle_score))
n_middle_exact <- sum(abs(middle_with_ct$our_middle_score - middle_with_ct$ct_middle_score) < 0.001,
                       na.rm = TRUE)

# Correlation only on properties both matched
middle_both <- middle_with_ct %>% filter(!is.na(our_middle_score))
cor_middle <- cor(middle_both$our_middle_score, middle_both$ct_middle_score)

pct_we_matched_middle <- 100 * n_middle_we_matched / n_middle_total
pct_exact_of_total_middle <- 100 * n_middle_exact / n_middle_total
pct_exact_of_matched_middle <- 100 * n_middle_exact / n_middle_we_matched

cat("MIDDLE SCHOOLS:\n")
cat(sprintf("  Total properties with C&T scores:              %d\n", n_middle_total))
cat(sprintf("  Properties we successfully matched:            %d (%.1f%% of total)\n",
            n_middle_we_matched, pct_we_matched_middle))
cat(sprintf("  Exact score matches (within 0.001):            %d (%.1f%% of total)\n",
            n_middle_exact, pct_exact_of_total_middle))
cat(sprintf("  Exact matches as %% of our successful matches:  %.1f%%\n",
            pct_exact_of_matched_middle))
cat(sprintf("  Correlation (for matched properties):          r = %.4f\n\n", cor_middle))


# ==============================================================================
# SAVE DETAILED RESULTS
# ==============================================================================

write_csv(comparison, "Data/school_matching_validation.csv")
cat("Detailed validation results saved to: Data/school_matching_validation.csv\n\n")

cat("Validation complete!\n")
# matching_diagnosis.R
# Diagnose Superfund matching accuracy against Christensen & Timmins replication data

suppressPackageStartupMessages({
  library(readxl)
})

# ==============================================================================
# SUPERFUND MATCHING DIAGNOSIS
# ==============================================================================

source("superfund_matching.R")

cat("=== Superfund Matching Diagnosis ===\n")

# Paths
recs_path <- "Data/HuD_Replication/Final Data Sets/recsprocessed_JPE.rds"
sf_excel_path <- "Data/Non_HDS/Superfund/epa-national-priorities-list-ciesin-mod-v2-2014.xls"

# Load replication data
cat("Loading C&T replication data...\n")
recs <- readRDS(recs_path)
valid_idx <- !is.na(recs$Latitude) & !is.na(recs$Longitude) & !is.na(recs$SFcount)
recs <- recs[valid_idx, ]
cat("Valid observations:", nrow(recs), "\n")

# Load Superfund data (full list for diagnostics)
cat("Loading Superfund site data...\n")
sf <- read_excel(sf_excel_path, sheet = "EPA_NPL_Sites_asof_27Feb2014")
sf$year <- as.numeric(format(sf$NPL_STATUS_DATE, "%Y"))

# Masks for year cutoffs
mask_2011 <- sf$year < 2011
mask_2012 <- sf$year < 2012
mask_2013 <- sf$year < 2013

# Masks for status filters (using 2012 cutoff)
mask_final <- sf$NPL_STATUS == "Currently on the Final NPL"
mask_deleted <- sf$NPL_STATUS == "Deleted from the Final NPL"
mask_proposed <- sf$NPL_STATUS == "Proposed for NPL"
mask_final_only <- mask_2012 & mask_final
mask_final_deleted <- mask_2012 & (mask_final | mask_deleted)
mask_final_proposed <- mask_2012 & (mask_final | mask_proposed)

# Allocate vectors
n <- nrow(recs)
calc_2012_5 <- integer(n)
calc_2012_5mi <- integer(n)
calc_2012_4p9 <- integer(n)
calc_2012_5p1 <- integer(n)
calc_2012_5p2 <- integer(n)
calc_2011_5 <- integer(n)
calc_2013_5 <- integer(n)
calc_final_only <- integer(n)
calc_final_deleted <- integer(n)
calc_final_proposed <- integer(n)

calc_eq_2012_5 <- integer(n)
calc_square_2012_5 <- integer(n)
calc_manh_2012_5 <- integer(n)

miles_to_km <- 1.60934
progress_interval <- max(1, floor(n / 20))

cat("Computing distances and counts...\n")
for (i in 1:n) {
  lon0 <- recs$Longitude[i]
  lat0 <- recs$Latitude[i]

  # Haversine distances (km)
  d_hav <- haversine_km(lon0, lat0, sf$LONGITUDE, sf$LATITUDE)

  calc_2012_5[i] <- sum(d_hav[mask_2012] <= 5, na.rm = TRUE)
  calc_2012_5mi[i] <- sum(d_hav[mask_2012] <= 5 * miles_to_km, na.rm = TRUE)
  calc_2012_4p9[i] <- sum(d_hav[mask_2012] <= 4.9, na.rm = TRUE)
  calc_2012_5p1[i] <- sum(d_hav[mask_2012] <= 5.1, na.rm = TRUE)
  calc_2012_5p2[i] <- sum(d_hav[mask_2012] <= 5.2, na.rm = TRUE)
  calc_2011_5[i] <- sum(d_hav[mask_2011] <= 5, na.rm = TRUE)
  calc_2013_5[i] <- sum(d_hav[mask_2013] <= 5, na.rm = TRUE)

  calc_final_only[i] <- sum(d_hav[mask_final_only] <= 5, na.rm = TRUE)
  calc_final_deleted[i] <- sum(d_hav[mask_final_deleted] <= 5, na.rm = TRUE)
  calc_final_proposed[i] <- sum(d_hav[mask_final_proposed] <= 5, na.rm = TRUE)

  # Planar approximations in km (local scaling)
  lat_rad <- lat0 * pi / 180
  dx <- (sf$LONGITUDE - lon0) * 111.32 * cos(lat_rad)
  dy <- (sf$LATITUDE - lat0) * 111.32
  d_eq <- sqrt(dx^2 + dy^2)

  calc_eq_2012_5[i] <- sum(d_eq[mask_2012] <= 5, na.rm = TRUE)
  calc_square_2012_5[i] <- sum(abs(dx[mask_2012]) <= 5 & abs(dy[mask_2012]) <= 5, na.rm = TRUE)
  calc_manh_2012_5[i] <- sum((abs(dx[mask_2012]) + abs(dy[mask_2012])) <= 5, na.rm = TRUE)

  if (i %% progress_interval == 0) {
    cat("  Processed", i, "/", n, "\n")
  }
}

sfcount <- recs$SFcount

summarize_match <- function(calc_vec) {
  exact <- mean(calc_vec == sfcount) * 100
  within1 <- mean(abs(calc_vec - sfcount) <= 1) * 100
  return(c(exact = exact, within1 = within1))
}

results <- rbind(
  c("Baseline: Haversine, 5 km, NPL<2012", summarize_match(calc_2012_5)),
  c("Haversine, 5 miles, NPL<2012", summarize_match(calc_2012_5mi)),
  c("Haversine, 4.9 km, NPL<2012", summarize_match(calc_2012_4p9)),
  c("Haversine, 5.1 km, NPL<2012", summarize_match(calc_2012_5p1)),
  c("Haversine, 5.2 km, NPL<2012", summarize_match(calc_2012_5p2)),
  c("Haversine, 5 km, NPL<2011", summarize_match(calc_2011_5)),
  c("Haversine, 5 km, NPL<2013", summarize_match(calc_2013_5)),
  c("Haversine, 5 km, Final only", summarize_match(calc_final_only)),
  c("Haversine, 5 km, Final+Deleted", summarize_match(calc_final_deleted)),
  c("Haversine, 5 km, Final+Proposed", summarize_match(calc_final_proposed)),
  c("Planar circle, 5 km, NPL<2012", summarize_match(calc_eq_2012_5)),
  c("Axis-aligned square, 5 km", summarize_match(calc_square_2012_5)),
  c("Manhattan diamond, 5 km", summarize_match(calc_manh_2012_5))
)

results <- data.frame(
  Method = results[, 1],
  Exact_Match = as.numeric(results[, 2]),
  Within_1 = as.numeric(results[, 3]),
  stringsAsFactors = FALSE
)

results$Exact_Match <- round(results$Exact_Match, 2)
results$Within_1 <- round(results$Within_1, 2)

cat("\n=== Match Rate Summary (percent) ===\n")
print(results, row.names = FALSE)

# Write LaTeX table for Methods Appendix
table_path <- "superfund_matching_table.tex"
row_lines <- sprintf("%s & %.1f & %.1f \\\\",
                     results$Method, results$Exact_Match, results$Within_1)
table_lines <- c(
  "\\begin{table}[h]",
  "\\centering",
  "\\caption{Superfund Matching Validation Against C\\&T Replication Data}",
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "Specification & Exact match (\\%) & Within +/-1 (\\%) \\\\",
  "\\midrule",
  row_lines,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(table_lines, table_path)
cat("\nWrote LaTeX table to:", table_path, "\n")

cat("\nNote: The baseline specification provides the highest exact match rate among the tested variants.\n")
