# matching_diagnosis.R
# Standardized validation of external dataset merges vs Christensen & Timmins (2022) replication data

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(readxl)
})

appendix_table_dir <- "Appendix_Tables"
if (!dir.exists(appendix_table_dir)) {
  dir.create(appendix_table_dir, recursive = TRUE)
}

write_rows_tex <- function(rows, filename) {
  writeLines(rows, file.path(appendix_table_dir, filename))
  cat("Wrote LaTeX rows to:", file.path(appendix_table_dir, filename), "\n")
}

match_stats <- function(ct_vals, our_vals, exact_tol = 0.001) {
  ct_ok <- !is.na(ct_vals)
  both_ok <- ct_ok & !is.na(our_vals)

  total <- sum(ct_ok)
  matched <- sum(both_ok)
  exact <- sum(both_ok & abs(our_vals - ct_vals) <= exact_tol)

  match_rate <- if (total > 0) 100 * matched / total else NA_real_
  exact_rate <- if (total > 0) 100 * exact / total else NA_real_
  exact_rate_matched <- if (matched > 0) 100 * exact / matched else NA_real_

  cor_val <- if (matched > 1) {
    cor(our_vals[both_ok], ct_vals[both_ok])
  } else {
    NA_real_
  }

  list(
    total = total,
    matched = matched,
    exact = exact,
    match_rate = match_rate,
    exact_rate = exact_rate,
    exact_rate_matched = exact_rate_matched,
    correlation = cor_val
  )
}

fmt_num <- function(x, digits = 1) {
  if (is.na(x)) "--" else sprintf(paste0("%.", digits, "f"), x)
}

matching_summary <- tibble(
  Dataset = character(),
  N_total = integer(),
  N_matched = integer(),
  Match_rate = double(),
  Exact_rate = double(),
  Exact_rate_matched = double(),
  Correlation = double()
)

# ==============================================================================
# SCHOOL MATCHING VALIDATION
# ==============================================================================

cat("=== SCHOOL MATCHING VALIDATION ===\n")

source("school_score_merging.R")

# Load C&T recommended properties with coordinates
ct_recs <- readRDS("Data/HuD_Replication/Final Data Sets/recsprocessed_JPE.rds") %>%
  select(CONTROL, TESTERID, SEQRH, Latitude, Longitude, RecPrice, Sqft_Rec)

cat(sprintf("  C&T recommended properties: %d\n", nrow(ct_recs)))

# Load C&T scores (property-level)
ct_hud <- readRDS("Data/HuD_Replication/Final Data Sets/HUDprocessed_JPE_testscores_042021.rds")

ct_scores <- ct_hud %>%
  select(CONTROL, TESTERID, RecPrice, Sqft_Rec,
         ct_elem_score = mn_avg_ol_elem_Rec,
         ct_middle_score = mn_avg_ol_middle_Rec) %>%
  distinct()

ct_with_scores <- ct_recs %>%
  left_join(ct_scores,
            by = c("CONTROL", "TESTERID", "RecPrice", "Sqft_Rec"),
            relationship = "many-to-many") %>%
  group_by(CONTROL, TESTERID, SEQRH) %>%
  slice(1) %>%
  ungroup()

cat(sprintf("  Properties with C&T scores: %d\n", nrow(ct_with_scores)))

# Apply our matching function to C&T coordinates
ct_matching_input <- ct_with_scores %>%
  rename(lat = Latitude, long = Longitude)

ct_matched <- merge_school_scores(ct_matching_input, lat_col = "lat", lon_col = "long")

comparison <- ct_matched %>%
  select(CONTROL, TESTERID, SEQRH, ct_elem_score, ct_middle_score,
         our_elem_score = elementary_school_score,
         our_middle_score = middle_school_score)

# Elementary school stats
stats_elem <- match_stats(comparison$ct_elem_score, comparison$our_elem_score, exact_tol = 0.001)
cat(sprintf("Elementary schools: matched %d/%d (%.1f%%), exact %.1f%%, r = %.4f\n",
            stats_elem$matched, stats_elem$total, stats_elem$match_rate,
            stats_elem$exact_rate, stats_elem$correlation))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "School scores (Elementary)",
    N_total = stats_elem$total,
    N_matched = stats_elem$matched,
    Match_rate = stats_elem$match_rate,
    Exact_rate = stats_elem$exact_rate,
    Exact_rate_matched = stats_elem$exact_rate_matched,
    Correlation = stats_elem$correlation
  )

# Middle school stats
stats_middle <- match_stats(comparison$ct_middle_score, comparison$our_middle_score, exact_tol = 0.001)
cat(sprintf("Middle schools: matched %d/%d (%.1f%%), exact %.1f%%, r = %.4f\n",
            stats_middle$matched, stats_middle$total, stats_middle$match_rate,
            stats_middle$exact_rate, stats_middle$correlation))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "School scores (Middle)",
    N_total = stats_middle$total,
    N_matched = stats_middle$matched,
    Match_rate = stats_middle$match_rate,
    Exact_rate = stats_middle$exact_rate,
    Exact_rate_matched = stats_middle$exact_rate_matched,
    Correlation = stats_middle$correlation
  )

# Save detailed school comparison results
write_csv(comparison, "Data/school_matching_validation.csv")
cat("Detailed school validation results saved to: Data/school_matching_validation.csv\n")

# ==============================================================================
# SUPERFUND MATCHING DIAGNOSIS
# ==============================================================================

cat("\n=== SUPERFUND MATCHING DIAGNOSIS ===\n")

source("superfund_merging.R")

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

# Render comparison symbols reliably in LaTeX tables
results$Method <- gsub("<", "$<$", results$Method, fixed = TRUE)

cat("\n=== Match Rate Summary (percent) ===\n")
print(results, row.names = FALSE)

superfund_rows <- sprintf("%s & %.1f & %.1f \\\\",
                          results$Method, results$Exact_Match, results$Within_1)
if (length(superfund_rows) > 0) {
  superfund_rows[length(superfund_rows)] <- sub("\\\\\\\\$", "", superfund_rows[length(superfund_rows)])
}
write_rows_tex(superfund_rows, "superfund_matching_variants_rows.tex")

# Add baseline superfund results to the master summary
stats_sf <- match_stats(sfcount, calc_2012_5, exact_tol = 0)
cat(sprintf("Superfund baseline: matched %d/%d (%.1f%%), exact %.1f%%, r = %.4f\n",
            stats_sf$matched, stats_sf$total, stats_sf$match_rate,
            stats_sf$exact_rate, stats_sf$correlation))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "Superfund count (5 km)",
    N_total = stats_sf$total,
    N_matched = stats_sf$matched,
    Match_rate = stats_sf$match_rate,
    Exact_rate = stats_sf$exact_rate,
    Exact_rate_matched = stats_sf$exact_rate_matched,
    Correlation = stats_sf$correlation
  )

# ==============================================================================
# RSEI (TRI TOXIC CONCENTRATION) MATCHING DIAGNOSIS
# ==============================================================================

cat("\n=== RSEI MATCHING DIAGNOSIS ===\n")

source("rsei_merging.R")

ct_rsei <- readRDS("Data/HuD_Replication/Final Data Sets/recsprocessed_JPE.rds") %>%
  select(CONTROL, TESTERID, SEQRH, Latitude, Longitude, RSEI) %>%
  filter(!is.na(RSEI), !is.na(Latitude), !is.na(Longitude))

cat("  C&T properties with RSEI and coordinates:", nrow(ct_rsei), "\n")

ct_rsei_input <- ct_rsei %>%
  rename(lat = Latitude, long = Longitude)

grid_lookup <- build_rsei_grid_lookup(
  agg_path = "Data/Non_HDS/RSEI/aggmicro2022_2012.csv",
  agg_cache = "Data/Non_HDS/RSEI/rsei_agg_2012.rds",
  lookup_cache = "Data/Non_HDS/RSEI/rsei_grid_lookup_2012.rds"
)

grid_wgs <- prepare_rsei_grid(
  lookup = grid_lookup,
  distance_crs = NULL,
  coords_cache = "Data/Non_HDS/RSEI/rsei_grid_coords_wgs84_2012.rds"
)

grid_albers <- prepare_rsei_grid(
  lookup = grid_lookup,
  distance_crs = 5070,
  coords_cache = "Data/Non_HDS/RSEI/rsei_grid_coords_5070_2012.rds"
)

variants <- tibble(
  Method = c(
    "Centroid NN (WGS84), toxconc",
    "Centroid NN (WGS84), ctconc",
    "Centroid NN (WGS84), nctconc",
    "Centroid NN (WGS84), score",
    "Centroid NN (EPSG:5070), toxconc",
    "Centroid NN (EPSG:5070), ctconc",
    "Centroid NN (EPSG:5070), nctconc",
    "Centroid NN (EPSG:5070), score"
  ),
  grid = c(
    rep("wgs", 4),
    rep("albers", 4)
  ),
  value_col = rep(c("toxconc", "ctconc", "nctconc", "score"), 2)
)

run_rsei_variant <- function(grid_prep, value_col) {
  matched <- match_rsei_centroid(
    ct_rsei_input,
    grid = grid_prep$lookup,
    coords = grid_prep$coords,
    lat_col = "lat",
    lon_col = "long",
    value_cols = value_col,
    prefix = "rsei_",
    keep_cell = FALSE,
    distance_crs = grid_prep$distance_crs
  )

  our_vals <- matched[[paste0("rsei_", value_col)]]
  stats <- match_stats(ct_rsei$RSEI, our_vals, exact_tol = 0.001)
  pearson_log <- cor(log1p(our_vals), log1p(ct_rsei$RSEI), use = "pairwise.complete.obs")
  spearman <- cor(our_vals, ct_rsei$RSEI, use = "pairwise.complete.obs", method = "spearman")

  list(matched = matched, stats = stats, our_vals = our_vals,
       pearson_log = pearson_log, spearman = spearman)
}

variant_results <- vector("list", nrow(variants))
for (i in seq_len(nrow(variants))) {
  grid_prep <- if (variants$grid[i] == "wgs") grid_wgs else grid_albers
  variant_results[[i]] <- run_rsei_variant(grid_prep, variants$value_col[i])
}

variants$Exact_Match_raw <- vapply(variant_results, function(x) x$stats$exact_rate, numeric(1))
variants$Match_rate <- vapply(variant_results, function(x) x$stats$match_rate, numeric(1))
variants$Exact_rate_matched <- vapply(variant_results, function(x) x$stats$exact_rate_matched, numeric(1))
variants$Pearson_raw <- vapply(variant_results, function(x) x$stats$correlation, numeric(1))
variants$Log1p_raw <- vapply(variant_results, function(x) x$pearson_log, numeric(1))
variants$Spearman_raw <- vapply(variant_results, function(x) x$spearman, numeric(1))

variants$Exact_Match <- round(variants$Exact_Match_raw, 2)
variants$Pearson <- round(variants$Pearson_raw, 4)
variants$Log1p <- round(variants$Log1p_raw, 4)
variants$Spearman <- round(variants$Spearman_raw, 4)
variants$Method <- gsub("<", "$<$", variants$Method, fixed = TRUE)

cat("\n=== RSEI Match Rate Summary ===\n")
print(variants[, c("Method", "Exact_Match", "Pearson", "Log1p", "Spearman")], row.names = FALSE)

rsei_rows <- sprintf("%s & %.1f & %.3f & %.3f & %.3f \\\\",
                     variants$Method, variants$Exact_Match,
                     variants$Pearson, variants$Log1p, variants$Spearman)
if (length(rsei_rows) > 0) {
  rsei_rows[length(rsei_rows)] <- sub("\\\\\\\\$", "", rsei_rows[length(rsei_rows)])
}
write_rows_tex(rsei_rows, "rsei_matching_variants_rows.tex")

best_idx <- variants %>%
  mutate(idx = row_number(),
         best_score = ifelse(is.na(Pearson_raw), -Inf, Pearson_raw)) %>%
  arrange(desc(best_score), desc(Exact_Match_raw), desc(Match_rate)) %>%
  slice(1) %>%
  pull(idx)

best_variant <- variants[best_idx, ]
best_stats <- variant_results[[best_idx]]$stats

best_grid_prep <- if (best_variant$grid == "wgs") grid_wgs else grid_albers
best_matched <- run_rsei_variant(best_grid_prep, best_variant$value_col)$matched

best_value <- paste0("rsei_", best_variant$value_col)
comparison_rsei <- best_matched %>%
  select(CONTROL, TESTERID, SEQRH, RSEI,
         our_rsei = all_of(best_value))

write_csv(comparison_rsei, "Data/rsei_matching_validation.csv")
cat("Detailed RSEI validation results saved to: Data/rsei_matching_validation.csv\n")

cat(sprintf("RSEI best match: %s (matched %d/%d, exact %.1f%%, r = %.4f)\n",
            best_variant$Method, best_stats$matched, best_stats$total,
            best_stats$exact_rate, best_stats$correlation))
cat(sprintf("RSEI best match (log1p Pearson = %.4f, Spearman = %.4f)\n",
            variants$Log1p_raw[best_idx], variants$Spearman_raw[best_idx]))

matching_summary <- matching_summary %>%
  add_row(
    Dataset = "RSEI toxic concentration",
    N_total = best_stats$total,
    N_matched = best_stats$matched,
    Match_rate = best_stats$match_rate,
    Exact_rate = best_stats$exact_rate,
    Exact_rate_matched = best_stats$exact_rate_matched,
    Correlation = best_stats$correlation
  )

# ==============================================================================
# COORDINATE VALIDATION (OPTIONAL)
# ==============================================================================

cat("\n=== COORDINATE VALIDATION (OPTIONAL) ===\n")
geocode_path <- "Data/sales_tester_rechomes_geocoded.csv"

if (file.exists(geocode_path)) {
  geo <- read_csv(geocode_path, show_col_types = FALSE)
  needed_keys <- c("CONTROL", "TESTERID", "SEQRH")

  if (!all(needed_keys %in% names(geo)) || !all(needed_keys %in% names(ct_recs))) {
    cat("Skipping coordinate validation: missing join keys (CONTROL/TESTERID/SEQRH).\n")
  } else if (!all(c("lat", "long") %in% names(geo))) {
    cat("Skipping coordinate validation: missing lat/long columns in geocoded data.\n")
  } else {
    geo_joined <- geo %>%
      select(all_of(needed_keys), lat, long) %>%
      inner_join(ct_recs, by = needed_keys)

    cat("Matched rows for coordinate check:", nrow(geo_joined), "\n")

    dist_km <- haversine_km(
      geo_joined$long,
      geo_joined$lat,
      geo_joined$Longitude,
      geo_joined$Latitude
    )

    dist_m <- dist_km * 1000
    coord_summary <- tibble(
      N = length(dist_m),
      Mean_m = mean(dist_m, na.rm = TRUE),
      Median_m = median(dist_m, na.rm = TRUE),
      Pct_within_50m = mean(dist_m <= 50, na.rm = TRUE) * 100,
      Pct_within_100m = mean(dist_m <= 100, na.rm = TRUE) * 100,
      Pct_within_500m = mean(dist_m <= 500, na.rm = TRUE) * 100,
      Pct_within_1km = mean(dist_m <= 1000, na.rm = TRUE) * 100
    )

    coord_row <- sprintf(
      "%d & %.1f & %.1f & %.1f & %.1f & %.1f & %.1f",
      coord_summary$N,
      coord_summary$Mean_m,
      coord_summary$Median_m,
      coord_summary$Pct_within_50m,
      coord_summary$Pct_within_100m,
      coord_summary$Pct_within_500m,
      coord_summary$Pct_within_1km
    )
    write_rows_tex(coord_row, "coordinate_match_rows.tex")
  }
} else {
  cat("Skipping coordinate validation: Data/sales_tester_rechomes_geocoded.csv not found.\n")
}

# ==============================================================================
# MASTER SUMMARY TABLE
# ==============================================================================

summary_rows <- sprintf(
  "%s & %d & %d & %s & %s & %s & %s \\\\",
  matching_summary$Dataset,
  matching_summary$N_total,
  matching_summary$N_matched,
  vapply(matching_summary$Match_rate, fmt_num, character(1), digits = 1),
  vapply(matching_summary$Exact_rate, fmt_num, character(1), digits = 1),
  vapply(matching_summary$Exact_rate_matched, fmt_num, character(1), digits = 1),
  vapply(matching_summary$Correlation, fmt_num, character(1), digits = 3)
)
if (length(summary_rows) > 0) {
  summary_rows[length(summary_rows)] <- sub("\\\\\\\\$", "", summary_rows[length(summary_rows)])
}

write_rows_tex(summary_rows, "matching_summary_rows.tex")

cat("\nValidation complete!\n")
