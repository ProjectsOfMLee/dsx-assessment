# ==============================================================================
# Test Suite for Question 4: TLG — Adverse Events Reporting
#
# Validates the data preparation, filtering, calculations, and outputs from:
#   01_create_ae_summary_table.R  (gtsummary hierarchical table)
#   02_create_visualizations.R    (ggplot2 bar chart + forest plot)
#   03_create_listings.R          (gt AE listing)
#
# Run: Rscript question_4_tlg/test_tlg.R
# ==============================================================================

library(testthat)
library(dplyr)
library(pharmaverseadam)

# --- Load source data (same as scripts use) -----------------------------------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Replicate the common filtering used across all three scripts
adae_teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(ACTARM != "Screen Failure")

adsl_safety <- adsl %>%
  filter(ACTARM != "Screen Failure")

cat("\n========================================\n")
cat("Running Q4 TLG Test Suite\n")
cat("========================================\n\n")

# ==============================================================================
# 1. DATA FILTERING
# ==============================================================================

test_that("TEAE filter: TRTEMFL == 'Y' selects 1122 records from ADAE", {
  teae_all <- adae %>% filter(TRTEMFL == "Y")
  expect_equal(nrow(teae_all), 1122L)
})

test_that("Screen Failure exclusion: 0 Screen Failure records in TEAEs", {
  sf_count <- adae %>%
    filter(TRTEMFL == "Y", ACTARM == "Screen Failure") %>%
    nrow()
  expect_equal(sf_count, 0L)
})

test_that("Final TEAE dataset has 1122 rows after both filters", {
  expect_equal(nrow(adae_teae), 1122L)
})

test_that("Final TEAE dataset has 217 unique subjects", {
  expect_equal(n_distinct(adae_teae$USUBJID), 217L)
})

test_that("Safety population has 254 subjects (excl Screen Failure)", {
  expect_equal(nrow(adsl_safety), 254L)
})

test_that("Three treatment arms in TEAE data", {
  arms <- sort(unique(adae_teae$ACTARM))
  expect_equal(arms, c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose"))
})

test_that("TEAE counts by treatment arm", {
  arm_counts <- adae_teae %>% count(ACTARM) %>% arrange(ACTARM)
  expect_equal(arm_counts$n[arm_counts$ACTARM == "Placebo"], 281L)
  expect_equal(arm_counts$n[arm_counts$ACTARM == "Xanomeline High Dose"], 414L)
  expect_equal(arm_counts$n[arm_counts$ACTARM == "Xanomeline Low Dose"], 427L)
})

test_that("No Screen Failure subjects in safety population", {
  expect_false("Screen Failure" %in% adsl_safety$ACTARM)
})

# ==============================================================================
# 2. SUMMARY TABLE (01_create_ae_summary_table.R)
# ==============================================================================

test_that("AESOC has 23 unique values in TEAE data", {
  expect_equal(n_distinct(adae_teae$AESOC), 23L)
})

test_that("AEDECOD has 230 unique values in TEAE data", {
  expect_equal(n_distinct(adae_teae$AEDECOD), 230L)
})

test_that("AESOC has no NA values in TEAE data", {
  expect_equal(sum(is.na(adae_teae$AESOC)), 0L)
})

test_that("AEDECOD has no NA values in TEAE data", {
  expect_equal(sum(is.na(adae_teae$AEDECOD)), 0L)
})

test_that("USUBJID has no NA values in TEAE data", {
  expect_equal(sum(is.na(adae_teae$USUBJID)), 0L)
})

test_that("Safety population ACTARM has no NA values", {
  expect_equal(sum(is.na(adsl_safety$ACTARM)), 0L)
})

test_that("Safety population arm counts: Placebo=86, High=72, Low=96", {
  arm_counts <- adsl_safety %>% count(ACTARM) %>% arrange(ACTARM)
  expect_equal(arm_counts$n[arm_counts$ACTARM == "Placebo"], 86L)
  expect_equal(arm_counts$n[arm_counts$ACTARM == "Xanomeline High Dose"], 72L)
  expect_equal(arm_counts$n[arm_counts$ACTARM == "Xanomeline Low Dose"], 96L)
})

test_that("Summary table output file exists", {
  expect_true(file.exists("question_4_tlg/ae_summary_table.html"))
})

test_that("Summary table output file is non-empty", {
  expect_gt(file.size("question_4_tlg/ae_summary_table.html"), 0L)
})

# ==============================================================================
# 3. VISUALIZATION: SEVERITY BAR CHART (02_create_visualizations.R)
# ==============================================================================

test_that("AESEV has three levels: MILD, MODERATE, SEVERE", {
  sev_vals <- sort(unique(adae_teae$AESEV))
  expect_equal(sev_vals, c("MILD", "MODERATE", "SEVERE"))
})

test_that("AESEV has no NA values", {
  expect_equal(sum(is.na(adae_teae$AESEV)), 0L)
})

test_that("AESEV counts: MILD=728, MODERATE=353, SEVERE=41", {
  sev_counts <- adae_teae %>% count(AESEV) %>% arrange(AESEV)
  expect_equal(sev_counts$n[sev_counts$AESEV == "MILD"], 728L)
  expect_equal(sev_counts$n[sev_counts$AESEV == "MODERATE"], 353L)
  expect_equal(sev_counts$n[sev_counts$AESEV == "SEVERE"], 41L)
})

test_that("Total AESEV counts sum to total TEAE records", {
  expect_equal(sum(table(adae_teae$AESEV)), nrow(adae_teae))
})

test_that("Severity bar chart output file exists", {
  expect_true(file.exists("question_4_tlg/ae_severity_by_treatment.png"))
})

test_that("Severity bar chart file is non-empty", {
  expect_gt(file.size("question_4_tlg/ae_severity_by_treatment.png"), 0L)
})

# ==============================================================================
# 4. VISUALIZATION: TOP 10 FOREST PLOT (02_create_visualizations.R)
# ==============================================================================

test_that("Top 10 AEs are correctly identified by unique subject count", {
  top10 <- adae_teae %>%
    distinct(USUBJID, AEDECOD) %>%
    count(AEDECOD, name = "n_subj") %>%
    arrange(desc(n_subj)) %>%
    head(10)
  expect_equal(nrow(top10), 10L)
  expect_equal(top10$AEDECOD[1], "PRURITUS")
  expect_equal(top10$n_subj[1], 54L)
  expect_equal(top10$AEDECOD[2], "APPLICATION SITE PRURITUS")
  expect_equal(top10$n_subj[2], 50L)
})

test_that("Denominator for incidence is 225 unique subjects (all ADAE)", {
  n_subjects <- n_distinct(adae$USUBJID)
  expect_equal(n_subjects, 225L)
})

test_that("Clopper-Pearson CI for PRURITUS (n=54, N=225) is correct", {
  n <- 54L
  N <- 225L
  pct <- 100 * n / N
  ci_lower <- 100 * qbeta(0.025, n, N - n + 1)
  ci_upper <- 100 * qbeta(0.975, n + 1, N - n)
  expect_equal(round(pct, 2), 24.00)
  expect_true(ci_lower > 18 && ci_lower < 20)
  expect_true(ci_upper > 29 && ci_upper < 31)
})

test_that("Clopper-Pearson CI properties: lower < pct < upper, within [0, 100]", {
  top10 <- adae_teae %>%
    distinct(USUBJID, AEDECOD) %>%
    count(AEDECOD, name = "n_subj") %>%
    arrange(desc(n_subj)) %>%
    head(10)
  N <- n_distinct(adae$USUBJID)
  for (i in seq_len(nrow(top10))) {
    n <- top10$n_subj[i]
    pct <- 100 * n / N
    ci_lower <- 100 * qbeta(0.025, n, N - n + 1)
    ci_upper <- 100 * qbeta(0.975, n + 1, N - n)
    expect_true(ci_lower >= 0, info = paste("CI lower < 0 for", top10$AEDECOD[i]))
    expect_true(ci_upper <= 100, info = paste("CI upper > 100 for", top10$AEDECOD[i]))
    expect_true(ci_lower < pct, info = paste("CI lower >= pct for", top10$AEDECOD[i]))
    expect_true(ci_upper > pct, info = paste("CI upper <= pct for", top10$AEDECOD[i]))
  }
})

test_that("Forest plot output file exists", {
  expect_true(file.exists("question_4_tlg/ae_top10_forest_plot.png"))
})

test_that("Forest plot file is non-empty", {
  expect_gt(file.size("question_4_tlg/ae_top10_forest_plot.png"), 0L)
})

# ==============================================================================
# 5. LISTING (03_create_listings.R)
# ==============================================================================

test_that("Listing has 1122 TEAE records", {
  listing_data <- adae_teae %>%
    arrange(USUBJID, ASTDT) %>%
    select(USUBJID, ACTARM, AEDECOD, AESEV, AEREL, AESTDTC, AEENDTC)
  expect_equal(nrow(listing_data), 1122L)
})

test_that("Listing has 7 required columns", {
  listing_cols <- c("USUBJID", "ACTARM", "AEDECOD", "AESEV", "AEREL",
                    "AESTDTC", "AEENDTC")
  for (col in listing_cols) {
    expect_true(col %in% names(adae_teae), info = paste("Missing:", col))
  }
})

test_that("AESTDTC has no NA values in listing data", {
  expect_equal(sum(is.na(adae_teae$AESTDTC)), 0L)
})

test_that("AEENDTC has 438 NA values (ongoing AEs)", {
  expect_equal(sum(is.na(adae_teae$AEENDTC)), 438L)
})

test_that("AEREL values are valid (NONE, POSSIBLE, PROBABLE, REMOTE, or NA)", {
  valid_rel <- c("NONE", "POSSIBLE", "PROBABLE", "REMOTE", NA)
  expect_true(all(adae_teae$AEREL %in% valid_rel))
})

test_that("AEREL distribution matches expected counts", {
  rel_counts <- adae_teae %>%
    filter(!is.na(AEREL)) %>%
    count(AEREL) %>%
    arrange(AEREL)
  expect_equal(rel_counts$n[rel_counts$AEREL == "NONE"], 276L)
  expect_equal(rel_counts$n[rel_counts$AEREL == "POSSIBLE"], 329L)
  expect_equal(rel_counts$n[rel_counts$AEREL == "PROBABLE"], 357L)
  expect_equal(rel_counts$n[rel_counts$AEREL == "REMOTE"], 156L)
})

test_that("AEREL has 4 NA values", {
  expect_equal(sum(is.na(adae_teae$AEREL)), 4L)
})

test_that("Listing output file exists", {
  expect_true(file.exists("question_4_tlg/ae_listings.html"))
})

test_that("Listing output file is non-empty", {
  expect_gt(file.size("question_4_tlg/ae_listings.html"), 0L)
})

test_that("ASTDT has no NA values (used for sorting)", {
  expect_equal(sum(is.na(adae_teae$ASTDT)), 0L)
})

# ==============================================================================
# 6. EDGE CASES
# ==============================================================================

test_that("No Screen Failure TEAEs exist in source data (edge: filter is a no-op)", {
  sf_teae <- adae %>%
    filter(TRTEMFL == "Y", ACTARM == "Screen Failure")
  expect_equal(nrow(sf_teae), 0L)
})

test_that("All TEAE subjects are in the safety population", {
  teae_subj <- unique(adae_teae$USUBJID)
  safety_subj <- unique(adsl_safety$USUBJID)
  expect_true(all(teae_subj %in% safety_subj))
})

test_that("Not all safety subjects have TEAEs (254 safety vs 217 with TEAEs)", {
  expect_gt(nrow(adsl_safety), n_distinct(adae_teae$USUBJID))
  expect_equal(nrow(adsl_safety) - n_distinct(adae_teae$USUBJID), 37L)
})

test_that("SEVERE AEs are rare (41 out of 1122 = 3.7%)", {
  severe_pct <- 100 * sum(adae_teae$AESEV == "SEVERE") / nrow(adae_teae)
  expect_true(severe_pct < 5)
  expect_equal(sum(adae_teae$AESEV == "SEVERE"), 41L)
})

test_that("Ongoing AEs (NA AEENDTC) exist across all treatment arms", {
  ongoing_arms <- adae_teae %>%
    filter(is.na(AEENDTC)) %>%
    distinct(ACTARM) %>%
    pull(ACTARM)
  expect_equal(length(ongoing_arms), 3L)
})

test_that("Top AE (PRURITUS) appears in all three treatment arms", {
  pruritus_arms <- adae_teae %>%
    filter(AEDECOD == "PRURITUS") %>%
    distinct(ACTARM) %>%
    pull(ACTARM)
  expect_equal(length(pruritus_arms), 3L)
})

test_that("Subjects can have multiple AEs (max AEs per subject > 1)", {
  ae_per_subj <- adae_teae %>% count(USUBJID)
  expect_gt(max(ae_per_subj$n), 1L)
})

test_that("Subjects can have the same AEDECOD multiple times", {
  dupes <- adae_teae %>%
    count(USUBJID, AEDECOD) %>%
    filter(n > 1)
  expect_gt(nrow(dupes), 0L)
})

test_that("Clopper-Pearson CI for n=1 (rare AE) is valid", {
  # Edge case: single subject with an AE
  N <- 225L
  n <- 1L
  ci_lower <- 100 * qbeta(0.025, n, N - n + 1)
  ci_upper <- 100 * qbeta(0.975, n + 1, N - n)
  pct <- 100 * n / N
  expect_true(ci_lower >= 0)
  expect_true(ci_upper <= 100)
  expect_true(ci_lower < pct)
  expect_true(ci_upper > pct)
})

test_that("Clopper-Pearson CI for n=N (all subjects) is valid", {
  # Edge case: if every subject had the AE
  N <- 225L
  n <- N
  ci_lower <- 100 * qbeta(0.025, n, N - n + 1)
  ci_upper <- 100 * qbeta(0.975, n + 1, N - n)
  expect_true(ci_lower > 95)
  expect_equal(ci_upper, 100)
})

test_that("All four output files exist and are non-empty", {
  files <- c("question_4_tlg/ae_summary_table.html",
             "question_4_tlg/ae_severity_by_treatment.png",
             "question_4_tlg/ae_top10_forest_plot.png",
             "question_4_tlg/ae_listings.html")
  for (f in files) {
    expect_true(file.exists(f), info = paste("Missing:", f))
    expect_gt(file.size(f), 0L)
  }
})

# ==============================================================================
# Run and report
# ==============================================================================
cat("\n========================================\n")
cat("Q4 TLG Test Suite Complete\n")
cat("========================================\n")
