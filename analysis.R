# Load required libraries
library(lfe)
library(dplyr)
library(tidyr)

# Import the data
data <- read.csv("Data/sales_and_tester_merged.csv") %>%
    filter(
        !is.na(STOTUNIT_TOTAL) & 
        !is.na(SAVLBAD_ANY) &
        !is.na(RACE) &
        !is.na(was_first_visitor) &
        !is.na(am_indicator_first) &
        !is.na(TPEGAI) &
        !is.na(THHEGAI) &
        !is.na(TSEX) &
        !is.na(age) &
        !is.na(THIGHEDU)
    ) %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup() %>%
    mutate(
        RACE = as.factor(RACE),
        CONTROL = as.factor(CONTROL),
        am_indicator_first = as.factor(am_indicator_first),
        TPEGAI = as.factor(TPEGAI),
        THHEGAI = as.factor(THHEGAI),
        TSEX = as.factor(TSEX),
        age = as.numeric(age),
        THIGHEDU = as.factor(THIGHEDU)
    ) %>%
    mutate(ofcolor = ifelse(RACE %in% c(2,3,4), 1, 0))

# Note that the only RACE categories present in valid trials are those indicated by 1, 2, 3, 4 (white, black, hispanic, asian)
summary(data$RACE)

# Run regressions with felm
recommended_total_races <- felm(STOTUNIT_TOTAL ~ RACE + was_first_visitor + am_indicator_first +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL | 0 | CONTROL, data = data)

available_any_races <- felm(SAVLBAD_ANY ~ RACE + was_first_visitor + am_indicator_first +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL | 0 | CONTROL, data = data)

recommended_total_ofcolor <- felm(STOTUNIT_TOTAL ~ ofcolor + was_first_visitor + am_indicator_first +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL | 0 | CONTROL, data = data)

available_any_ofcolor <- felm(SAVLBAD_ANY ~ ofcolor + was_first_visitor + am_indicator_first +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL | 0 | CONTROL, data = data)

# Display summaries of previous models
summary(reccomended_total_races)
summary(available_any_races)
summary(recommended_total_ofcolor)
summary(available_any_ofcolor)

# Alternate specification keeping each appointment as a separate row
appointments_data <- read.csv("Data/sales_and_tester_appointments.csv")  %>%
    filter(
        !is.na(STOTUNIT) &
        !is.na(SAVLBAD_BINARY) &
        !is.na(RACE) &
        !is.na(visit_order) &
        !is.na(am_indicator) &
        !is.na(TPEGAI) &
        !is.na(THHEGAI) &
        !is.na(TSEX) &
        !is.na(age) &
        !is.na(THIGHEDU)
    ) %>%
    group_by(CONTROL) %>%
    filter(n_distinct(TESTERID) == 2) %>%
    ungroup() %>%
    mutate(
        RACE = as.factor(RACE),
        CONTROL = as.factor(CONTROL),
        visit_order = as.factor(visit_order),
        am_indicator = as.factor(am_indicator),
        TPEGAI = as.factor(TPEGAI),
        THHEGAI = as.factor(THHEGAI),
        TSEX = as.factor(TSEX),
        age = as.numeric(age),
        THIGHEDU = as.factor(THIGHEDU)
    ) %>%
    mutate(ofcolor = ifelse(RACE %in% c(2,3,4), 1, 0))


# Run regressions with felm for appointments data
recommended_apps_races <- felm(STOTUNIT ~ RACE + visit_order + am_indicator +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL | 0 | CONTROL, data = appointments_data)

available_apps_races <- felm(SAVLBAD_BINARY ~ RACE + visit_order + am_indicator +
                        TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                        CONTROL | 0 | CONTROL, data = appointments_data)

recommended_apps_ofcolor <- felm(STOTUNIT ~ ofcolor + visit_order + am_indicator +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL | 0 | CONTROL, data = appointments_data)

available_apps_ofcolor <- felm(SAVLBAD_BINARY ~ ofcolor + visit_order + am_indicator +
                        TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                        CONTROL | 0 | CONTROL, data = appointments_data)

# Display results for appointments analysis
summary(recommended_apps_races)
summary(available_apps_races)
summary(recommended_apps_ofcolor)
summary(available_apps_ofcolor)

library(broom)
library(xtable)

# Helper function to extract coefficient, SE, CI
extract_coef_info <- function(model, varname) {
    coefs <- coef(summary(model))
    est <- coefs[varname, "Estimate"]
    se <- coefs[varname, "Cluster s.e."]
    ci <- confint(model, level = 0.95)[varname, ]
    list(est = est, se = se, ci = ci)
}

# Get info for "ofcolor" (Racial Minority) and "RACE" levels
get_race_rows <- function(model, race_levels) {
    rows <- lapply(race_levels, function(race) {
        info <- extract_coef_info(model, race)
        c(
            sprintf("% .4f", info$est),
            sprintf("(% .4f)", info$se),
            sprintf("[% .4f,% .4f]", info$ci[1], info$ci[2])
        )
    })
    do.call(rbind, rows)
}
# Helper to get significance stars
get_stars <- function(pval) {
    if (is.na(pval)) return("")
    if (pval < 0.01) return("\\sym{***}")
    if (pval < 0.05) return("\\sym{**}")
    if (pval < 0.10) return("\\sym{*}")
    return("")
}

# Modified extract_coef_info to include p-value and stars
extract_coef_info <- function(model, varname) {
    coefs <- coef(summary(model))
    est <- coefs[varname, "Estimate"]
    se <- coefs[varname, "Cluster s.e."]
    pval <- coefs[varname, "Pr(>|t|)"]
    ci <- confint(model, level = 0.95)[varname, ]
    star <- get_stars(pval)
    list(est = est, se = se, ci = ci, star = star)
}

# Get info for "ofcolor" (Racial Minority) and "RACE" levels
get_race_rows <- function(model, race_levels) {
    rows <- lapply(race_levels, function(race) {
        info <- extract_coef_info(model, race)
        c(
            sprintf("% .4f%s", info$est, info$star),
            sprintf("(% .4f)", info$se),
            sprintf("[% .4f,% .4f]", info$ci[1], info$ci[2])
        )
    })
    do.call(rbind, rows)
}

# Prepare rows for each column
minority_vars <- c("ofcolor")
race_vars <- c("RACE2", "RACE3", "RACE4") # African American, Hispanic, Asian

# Column 1: Total Recommended Properties
col1_minority <- extract_coef_info(recommended_total_ofcolor, "ofcolor")
col1_race <- get_race_rows(recommended_total_races, race_vars)

# Column 2: Ad Property Ever Available
col2_minority <- extract_coef_info(available_any_ofcolor, "ofcolor")
col2_race <- get_race_rows(available_any_races, race_vars)

# Column 3: Recommended Properties per Appointment
col3_minority <- extract_coef_info(recommended_apps_ofcolor, "ofcolor")
col3_race <- get_race_rows(recommended_apps_races, race_vars)

# Column 4: Ad Property Available per Appointment
col4_minority <- extract_coef_info(available_apps_ofcolor, "ofcolor")
col4_race <- get_race_rows(available_apps_races, race_vars)

# Combine rows for LaTeX table
table_rows <- rbind(
    c("Racial Minority", 
      sprintf("% .4f%s", col1_minority$est, col1_minority$star), sprintf("% .4f%s", col2_minority$est, col2_minority$star),
      sprintf("% .4f%s", col3_minority$est, col3_minority$star), sprintf("% .4f%s", col4_minority$est, col4_minority$star)),
    c("", 
      sprintf("(% .4f)", col1_minority$se), sprintf("(% .4f)", col2_minority$se),
      sprintf("(% .4f)", col3_minority$se), sprintf("(% .4f)", col4_minority$se)),
    c("", 
      sprintf("[% .4f,% .4f]", col1_minority$ci[1], col1_minority$ci[2]),
      sprintf("[% .4f,% .4f]", col2_minority$ci[1], col2_minority$ci[2]),
      sprintf("[% .4f,% .4f]", col3_minority$ci[1], col3_minority$ci[2]),
      sprintf("[% .4f,% .4f]", col4_minority$ci[1], col4_minority$ci[2])),
    c("African American", 
      if (nrow(col1_race) >= 1) col1_race[1,1] else NA, 
      if (nrow(col2_race) >= 1) col2_race[1,1] else NA, 
      if (nrow(col3_race) >= 1) col3_race[1,1] else NA, 
      if (nrow(col4_race) >= 1) col4_race[1,1] else NA),
    c("", 
      if (nrow(col1_race) >= 1) col1_race[1,2] else NA, 
      if (nrow(col2_race) >= 1) col2_race[1,2] else NA, 
      if (nrow(col3_race) >= 1) col3_race[1,2] else NA, 
      if (nrow(col4_race) >= 1) col4_race[1,2] else NA),
    c("", 
      if (nrow(col1_race) >= 1) col1_race[1,3] else NA, 
      if (nrow(col2_race) >= 1) col2_race[1,3] else NA, 
      if (nrow(col3_race) >= 1) col3_race[1,3] else NA, 
      if (nrow(col4_race) >= 1) col4_race[1,3] else NA),
    c("Hispanic", 
      if (nrow(col1_race) >= 2) col1_race[2,1] else NA, 
      if (nrow(col2_race) >= 2) col2_race[2,1] else NA, 
      if (nrow(col3_race) >= 2) col3_race[2,1] else NA, 
      if (nrow(col4_race) >= 2) col4_race[2,1] else NA),
    c("", 
      if (nrow(col1_race) >= 2) col1_race[2,2] else NA, 
      if (nrow(col2_race) >= 2) col2_race[2,2] else NA, 
      if (nrow(col3_race) >= 2) col3_race[2,2] else NA, 
      if (nrow(col4_race) >= 2) col4_race[2,2] else NA),
    c("", 
      if (nrow(col1_race) >= 2) col1_race[2,3] else NA, 
      if (nrow(col2_race) >= 2) col2_race[2,3] else NA, 
      if (nrow(col3_race) >= 2) col3_race[2,3] else NA, 
      if (nrow(col4_race) >= 2) col4_race[2,3] else NA),
    c("Asian", 
      if (nrow(col1_race) >= 3) col1_race[3,1] else NA, 
      if (nrow(col2_race) >= 3) col2_race[3,1] else NA, 
      if (nrow(col3_race) >= 3) col3_race[3,1] else NA, 
      if (nrow(col4_race) >= 3) col4_race[3,1] else NA),
    c("", 
      if (nrow(col1_race) >= 3) col1_race[3,2] else NA, 
      if (nrow(col2_race) >= 3) col2_race[3,2] else NA, 
      if (nrow(col3_race) >= 3) col3_race[3,2] else NA, 
      if (nrow(col4_race) >= 3) col4_race[3,2] else NA),
    c("", 
      if (nrow(col1_race) >= 3) col1_race[3,3] else NA, 
      if (nrow(col2_race) >= 3) col2_race[3,3] else NA, 
      if (nrow(col3_race) >= 3) col3_race[3,3] else NA, 
      if (nrow(col4_race) >= 3) col4_race[3,3] else NA)
)

# Create LaTeX table with proper column labels
latex_table <- function(rows) {
    cat("\\begin{table}[p]\n\\centering\n")
    cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(#^{#1}\\)\\fi}\n")
    cat("\\caption{Discriminatory Steering and Availability of Advertised Properties\\[0.5em]\\textit{Table 5, C\\&T 2022}}\n")
    cat("\\label{tab:correcttable5}\n")
    cat("\\resizebox{\\textwidth}{!}{\n")
    cat("\\begin{tabular}{l*{4}{c}}\n")
    cat("\\toprule\n")
    cat("& \\multicolumn{4}{c}{Dependent Variable} \\\\\n")
    cat("\\cmidrule(lr){2-5}\n")
    cat("&\\multicolumn{1}{c}{\\begin{tabular}{@{}c@{}}Total Recommended\\\\Properties\\end{tabular}} ")
    cat("&\\multicolumn{1}{c}{\\begin{tabular}{@{}c@{}}Ad Property Ever\\\\Available\\end{tabular}} ")
    cat("&\\multicolumn{1}{c}{\\begin{tabular}{@{}c@{}}Recommended Properties\\\\per Appointment\\end{tabular}} ")
    cat("&\\multicolumn{1}{c}{\\begin{tabular}{@{}c@{}}Ad Property Available\\\\per Appointment\\end{tabular}}\\\\\n")
    cat("\\midrule\n")
    for (row in 1:nrow(rows)) {
        cat(paste(rows[row,], collapse=" & "), "\\\\\n")
        if (row %% 3 == 0 && row < nrow(rows)) cat("[1ex]\n")
    }
    cat("\\midrule\n")
    cat(sprintf("Observations      &%d&%d&%d&%d\\\\\n",
        recommended_total_ofcolor$N, available_any_ofcolor$N,
        recommended_apps_ofcolor$N, available_apps_ofcolor$N))
    cat(sprintf("Adjusted R$^2$ (Minority)      &%.4f&%.4f&%.4f&%.4f\\\\\n",
        summary(recommended_total_ofcolor)$adj.r.squared,
        summary(available_any_ofcolor)$adj.r.squared,
        summary(recommended_apps_ofcolor)$adj.r.squared,
        summary(available_apps_ofcolor)$adj.r.squared))
    cat(sprintf("Adjusted R$^2$ (Category)      &%.4f&%.4f&%.4f&%.4f\\\\\n",
        summary(recommended_total_races)$adj.r.squared,
        summary(available_any_races)$adj.r.squared,
        summary(recommended_apps_races)$adj.r.squared,
        summary(available_apps_races)$adj.r.squared))
    cat(sprintf("Number of Trials      &%d&%d&%d&%d\\\\\n",
        length(unique(data$CONTROL)), length(unique(data$CONTROL)),
        length(unique(appointments_data$CONTROL)), length(unique(appointments_data$CONTROL))))
    cat("\\bottomrule\n")
    cat("\\multicolumn{5}{l}{\\footnotesize Cluster-robust standard errors in parentheses. Clustered at the trial level. 95\\% confidence intervals in square brackets.}\\\\\n")
    cat("\\multicolumn{5}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
    cat("\\end{tabular}\n}\n\\end{table}\n")
}

latex_table(table_rows)


# Load cleaned data for Table 6
cleaned_data <- read.csv("Data/cleaned_hds.csv") %>%
    filter(
        !is.na(percent_white) &
        !is.na(RACE) &
        !is.na(am_indicator_first) &
        !is.na(TSEX) &
        !is.na(THHEGAI) &
        !is.na(TPEGAI) &
        !is.na(THIGHEDU) &
        !is.na(TCURTENR) &
        !is.na(age)
    ) %>%
    mutate(
        RACE = as.factor(RACE),
        CONTROL = as.factor(CONTROL),
        am_indicator_first = as.factor(am_indicator_first),
        TSEX = as.factor(TSEX),
        THHEGAI = as.factor(THHEGAI),
        TPEGAI = as.factor(TPEGAI),
        THIGHEDU = as.factor(THIGHEDU),
        TCURTENR = as.factor(TCURTENR),
        age = as.numeric(age),
        ofcolor = ifelse(RACE %in% c(2,3,4), 1, 0)
    )

# Run regressions for Table 6
percent_white_races <- felm(percent_white ~ RACE + am_indicator_first + TSEX + THHEGAI + TPEGAI + THIGHEDU + TCURTENR + age | CONTROL | 0 | CONTROL, data = cleaned_data)
percent_white_ofcolor <- felm(percent_white ~ ofcolor + am_indicator_first + TSEX + THHEGAI + TPEGAI + THIGHEDU + TCURTENR + age | CONTROL | 0 | CONTROL, data = cleaned_data)

summary(percent_white_races)
summary(percent_white_ofcolor)

# Extract results for LaTeX table
race_vars6 <- c("RACE2", "RACE3", "RACE4")
col1_minority6 <- extract_coef_info(percent_white_ofcolor, "ofcolor")
col1_race6 <- get_race_rows(percent_white_races, race_vars6)

table6_rows <- rbind(
    c("Racial Minority", sprintf("% .4f%s", col1_minority6$est, col1_minority6$star)),
    c("", sprintf("(% .4f)", col1_minority6$se)),
    c("", sprintf("[% .4f,% .4f]", col1_minority6$ci[1], col1_minority6$ci[2])),
    c("African American", if (nrow(col1_race6) >= 1) col1_race6[1,1] else NA),
    c("", if (nrow(col1_race6) >= 1) col1_race6[1,2] else NA),
    c("", if (nrow(col1_race6) >= 1) col1_race6[1,3] else NA),
    c("Hispanic", if (nrow(col1_race6) >= 2) col1_race6[2,1] else NA),
    c("", if (nrow(col1_race6) >= 2) col1_race6[2,2] else NA),
    c("", if (nrow(col1_race6) >= 2) col1_race6[2,3] else NA),
    c("Asian", if (nrow(col1_race6) >= 3) col1_race6[3,1] else NA),
    c("", if (nrow(col1_race6) >= 3) col1_race6[3,2] else NA),
    c("", if (nrow(col1_race6) >= 3) col1_race6[3,3] else NA)
)

# LaTeX table for Table 6
latex_table6 <- function(rows) {
    cat("\\begin{table}[p]\n\\centering\n")
    cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(#^{#1}\\)\\fi}\n")
    cat("\\caption{Percent White in Recommended Neighborhoods\\[0.5em]\\textit{Table 6, C\\&T 2022}}\n")
    cat("\\label{tab:correcttable6}\n")
    cat("\\resizebox{!}{!}{\n")
    cat("\\begin{tabular}{l c}\n")
    cat("\\toprule\n")
    cat("&\\multicolumn{1}{c}{Percent White in Recommended Neighborhoods}\\\\\n")
    cat("\\midrule\n")
    for (row in 1:nrow(rows)) {
        cat(paste(rows[row,], collapse=" & "), "\\\\\n")
        if (row %% 3 == 0 && row < nrow(rows)) cat("[1ex]\n")
    }
    cat("\\midrule\n")
    cat(sprintf("Observations      &%d\\\\\n", percent_white_ofcolor$N))
    cat(sprintf("Adjusted R$^2$ (Minority)      &%.4f\\\\\n", summary(percent_white_ofcolor)$adj.r.squared))
    cat(sprintf("Adjusted R$^2$ (Category)      &%.4f\\\\\n", summary(percent_white_races)$adj.r.squared))
    cat(sprintf("Number of Trials      &%d\\\\\n", length(unique(cleaned_data$CONTROL))))
    cat("\\bottomrule\n")
    cat("\\multicolumn{2}{l}{\\footnotesize Cluster-robust standard errors in parentheses. Clustered at the trial level. 95\\% confidence intervals in square brackets.}\\\\\n")
    cat("\\multicolumn{2}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
    cat("\\end{tabular}\n}\n\\end{table}\n")
}

latex_table6(table6_rows)

# Run regressions for each outcome controlling for percent_white, percent_black, percent_asian, percent_hispanic

# --- Model specs --------------------------------------------------------------
outcomes <- c("poverty_rate", "high_skilled_rate", "college_graduate_rate",
              "single_parent_rate", "ownership_rate")

race_controls <- "percent_white + percent_black + percent_asian + percent_hispanic"
covariates <- "RACE + am_indicator_first + TSEX + THHEGAI + TPEGAI + THIGHEDU + TCURTENR + age"
minority_covariates <- "ofcolor + am_indicator_first + TSEX + THHEGAI + TPEGAI + THIGHEDU + TCURTENR + age"

models_race <- lapply(outcomes, function(outcome) {
  fml <- as.formula(paste(outcome, "~", covariates, "+", race_controls, "| CONTROL | 0 | CONTROL"))
  felm(fml, data = cleaned_data)
})

models_minority <- lapply(outcomes, function(outcome) {
  fml <- as.formula(paste(outcome, "~", minority_covariates, "+", race_controls, "| CONTROL | 0 | CONTROL"))
  felm(fml, data = cleaned_data)
})

# --- Helpers: stars, extraction, formatting ----------------------------------

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("\\sym{***}")
  if (p < 0.05) return("\\sym{**}")
  if (p < 0.10) return("\\sym{*}")
  ""
}

# Format integers with comma, but LaTeX-safe (18{,}949)
fint <- function(x) gsub(",", "{,}", formatC(as.integer(x), format = "d", big.mark = ","))

# Extract estimate, SE, CI, p, stars for a coefficient from a felm model
extract_coef_info <- function(model, varname) {
  s <- coef(summary(model))
  if (is.null(s) || !varname %in% rownames(s)) {
    return(list(est = "", est_star = "", se = "", ci = c("", ""), p = NA))
  }
  # Column names vary slightly; be tolerant
  se_col <- if ("Cluster s.e." %in% colnames(s)) "Cluster s.e." else "Std. Error"
  p_col  <- grep("^Pr\\(", colnames(s), value = TRUE)
  if (length(p_col) == 0) p_col <- "Pr(>|t|)"  # fallback

  est <- as.numeric(s[varname, "Estimate"])
  se  <- as.numeric(s[varname, se_col])
  p   <- suppressWarnings(as.numeric(s[varname, p_col[1]]))

  # t critical using model residual df (matches felm's cluster t by default)
  df  <- model$df.residual
  tcrit <- qt(0.975, df = df)
  ci_lo <- est - tcrit * se
  ci_hi <- est + tcrit * se

  stars <- star_code(p)

  list(
    est      = sprintf("% .4f", est),
    est_star = paste0(sprintf("% .4f", est), stars),
    se       = sprintf("(% .4f)", se),
    ci       = c(sprintf("[% .4f", ci_lo), sprintf("% .4f]", ci_hi)),
    p        = p
  )
}

# Get a vector (length = number of models) of a given cell type for a coefficient
cells_for_coef <- function(models, coef_name, type = c("est_star", "se", "ci")) {
  type <- match.arg(type)
  sapply(models, function(m) {
    info <- extract_coef_info(m, coef_name)
    switch(type,
           est_star = info$est_star,
           se       = info$se,
           ci       = paste(info$ci, collapse = ", "))
  }, USE.NAMES = FALSE)
}

# --- Build wide rows with stars on estimates ---------------------------------

race_vars_out <- c("RACE2", "RACE3", "RACE4")  # Black, Hispanic, Asian (White is base)
race_labels   <- c("African American", "Hispanic", "Asian")

outcome_labels <- c("Poverty Rate",
                    "High-Skilled Rate",
                    "College Graduate Rate",
                    "Single Parent Rate",
                    "Ownership Rate")

build_rows <- function(models_minority, models_race, outcome_labels) {
  # Racial Minority (with stars)
  rm_est <- c("Racial Minority", cells_for_coef(models_minority, "ofcolor", "est_star"))
  rm_se  <- c("",                 cells_for_coef(models_minority, "ofcolor", "se"))
  rm_ci  <- c("",                 cells_for_coef(models_minority, "ofcolor", "ci"))

  # Category rows
  race_blocks <- lapply(seq_along(race_vars_out), function(i) {
    est <- c(race_labels[i], cells_for_coef(models_race, race_vars_out[i], "est_star"))
    se  <- c("",             cells_for_coef(models_race, race_vars_out[i], "se"))
    ci  <- c("",             cells_for_coef(models_race, race_vars_out[i], "ci"))
    rbind(est, se, ci)
  })

  rows <- rbind(rm_est, rm_se, rm_ci, do.call(rbind, race_blocks))
  colnames(rows) <- c(" ", outcome_labels)
  rows
}

table_rows_out <- build_rows(models_minority, models_race, outcome_labels)

# --- LaTeX table printer with clean formatting -------------------------------

latex_table_outcomes <- function(rows, models_minority, models_race, outcome_labels, cleaned_data) {
  cat("\\begin{table}[p]\n\\centering\n")
  cat("\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}\n")
  cat("\\caption{Neighborhood Characteristics in Recommended Properties}\n")
  cat("\\label{tab:neighchar_racecontrols}\n")
  cat("\\resizebox{\\textwidth}{!}{%\n")
  cat("\\begin{tabular}{lccccc}\n")
  cat("\\toprule\n")
  cat("& \\multicolumn{5}{c}{Dependent variable} \\\\\n")
  cat("\\cmidrule(lr){2-6}\n")
  cat(paste0("& ", paste(outcome_labels, collapse = " & "), " \\\\\n"))
  cat("\\midrule\n")

  apply(rows, 1, function(r) cat(paste(r, collapse = " & "), " \\\\\n"))

  cat("\\midrule\n")
  # Observations
  cat("Observations")
  for (j in seq_along(models_minority)) cat(sprintf(" & %s", fint(models_minority[[j]]$N)))
  cat("\\\\\n")

  # Adjusted R^2 (Minority)
  cat("Adjusted R$^2$ (Minority)")
  for (j in seq_along(models_minority)) cat(sprintf(" & %.4f", summary(models_minority[[j]])$adj.r.squared))
  cat("\\\\\n")

  # Adjusted R^2 (Category)
  cat("Adjusted R$^2$ (Category)")
  for (j in seq_along(models_race)) cat(sprintf(" & %.4f", summary(models_race[[j]])$adj.r.squared))
  cat("\\\\\n")

  # Number of Trials (unique CONTROL)
  n_trials <- length(unique(cleaned_data$CONTROL))
  cat("Number of Trials")
  for (j in seq_along(models_minority)) cat(sprintf(" & %s", fint(n_trials)))
  cat("\\\\\n")

  cat("\\bottomrule\n")
  cat("\\multicolumn{6}{l}{\\footnotesize Cluster-robust standard errors in parentheses; clustered at the trial level. 95\\% confidence intervals in square brackets.}\\\\\n")
  cat("\\multicolumn{6}{l}{\\footnotesize \\sym{*} \\(p<0.10\\), \\sym{**} \\(p<0.05\\), \\sym{***} \\(p<0.01\\)}\\\\\n")
  cat("\\end{tabular}%\n")
  cat("}\n\\end{table}\n")
}

# --- Render ------------------------------------------------------------------
latex_table_outcomes(table_rows_out, models_minority, models_race, outcome_labels, cleaned_data)


###########################################################################
### APPENDIX REGRESSIONS
###########################################################################

recommended_first <- felm(STOTUNIT_FIRST ~ RACE + was_first_visitor + am_indicator_first +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL, data = filtered_data)

available_first <- felm(SAVLBAD_FIRST ~ RACE + was_first_visitor + am_indicator_first +
                         TPEGAI + THHEGAI + TSEX + age + THIGHEDU | 
                         CONTROL, data = filtered_data)


summary(recommended_first)
summary(available_first)


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
