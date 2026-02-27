# ==============================================================================
# Test Suite for Question 2: SDTM DS Domain
#
# Validates the DS domain output from 02_create_ds_domain.R against SDTM
# structural rules, controlled terminology, and edge cases.
#
# Run: Rscript question_2_sdtm/test_ds_domain.R
# ==============================================================================

library(testthat)
library(dplyr)

# --- Source the DS domain script to produce ds_final --------------------------
# Suppress sdtm.oak informational messages about unmapped CT terms
suppressMessages(source("question_2_sdtm/02_create_ds_domain.R"))

cat("\n\n========================================\n")
cat("Running DS Domain Test Suite\n")
cat("========================================\n\n")

# ==============================================================================
# 1. STRUCTURAL TESTS
# ==============================================================================

test_that("DS domain has exactly 12 required columns", {
  expected_cols <- c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM",
                     "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC",
                     "DSSTDTC", "DSSTDY")
  expect_equal(names(ds_final), expected_cols)
})

test_that("DS domain has 850 rows (one per raw record)", {
  expect_equal(nrow(ds_final), 850L)
})

test_that("Column types are correct", {
  expect_type(ds_final$STUDYID, "character")
  expect_type(ds_final$DOMAIN, "character")
  expect_type(ds_final$USUBJID, "character")
  expect_type(ds_final$DSSEQ, "integer")
  expect_type(ds_final$DSTERM, "character")
  expect_type(ds_final$DSDECOD, "character")
  expect_type(ds_final$DSCAT, "character")
  expect_type(ds_final$VISITNUM, "double")
  expect_type(ds_final$VISIT, "character")
  expect_type(ds_final$DSSTDTC, "character")
})

test_that("DS domain is a tibble/data.frame", {
  expect_s3_class(ds_final, "data.frame")
})

# ==============================================================================
# 2. STUDYID AND DOMAIN
# ==============================================================================

test_that("STUDYID is always CDISCPILOT01", {
  expect_true(all(ds_final$STUDYID == "CDISCPILOT01"))
  expect_equal(n_distinct(ds_final$STUDYID), 1L)
})

test_that("DOMAIN is always DS", {
  expect_true(all(ds_final$DOMAIN == "DS"))
  expect_equal(n_distinct(ds_final$DOMAIN), 1L)
})

# ==============================================================================
# 3. USUBJID
# ==============================================================================

test_that("USUBJID has 306 unique subjects", {
  expect_equal(n_distinct(ds_final$USUBJID), 306L)
})

test_that("USUBJID starts with '01-' prefix", {
  expect_true(all(grepl("^01-", ds_final$USUBJID)))
})

test_that("USUBJID has no NA values", {
  expect_equal(sum(is.na(ds_final$USUBJID)), 0L)
})

# ==============================================================================
# 4. DSSEQ
# ==============================================================================

test_that("DSSEQ starts at 1 for every subject", {
  min_seq <- ds_final %>%
    group_by(USUBJID) %>%
    summarise(min_seq = min(DSSEQ), .groups = "drop")
  expect_true(all(min_seq$min_seq == 1L))
})

test_that("DSSEQ is sequential within each subject (no gaps)", {
  seq_check <- ds_final %>%
    group_by(USUBJID) %>%
    summarise(
      n = n(),
      is_sequential = all(sort(DSSEQ) == seq_len(n())),
      .groups = "drop"
    )
  expect_true(all(seq_check$is_sequential))
})

test_that("DSSEQ range is 1 to 4", {
  expect_equal(min(ds_final$DSSEQ), 1L)
  expect_equal(max(ds_final$DSSEQ), 4L)
})

test_that("DSSEQ has no NA values", {
  expect_equal(sum(is.na(ds_final$DSSEQ)), 0L)
})

# ==============================================================================
# 5. DSTERM
# ==============================================================================

test_that("DSTERM has no NA values", {
  expect_equal(sum(is.na(ds_final$DSTERM)), 0L)
})

test_that("OTHER EVENT records have DSTERM from OTHERSP (Final Lab Visit or Final Retrieval Visit)", {
  other_terms <- ds_final %>%
    filter(DSCAT == "OTHER EVENT") %>%
    distinct(DSTERM) %>%
    pull(DSTERM) %>%
    sort()
  expect_equal(other_terms, c("Final Lab Visit", "Final Retrieval Visit"))
})

test_that("DSTERM contains expected disposition terms", {
  expected_terms <- c("Adverse Event", "Protocol Completed", "Randomized",
                      "Final Lab Visit", "Final Retrieval Visit")
  for (term in expected_terms) {
    expect_true(term %in% ds_final$DSTERM,
                info = paste("Missing DSTERM:", term))
  }
})

# ==============================================================================
# 6. DSDECOD (Controlled Terminology)
# ==============================================================================

test_that("DSDECOD has 290 NA values (OTHER EVENT records)", {
  expect_equal(sum(is.na(ds_final$DSDECOD)), 290L)
})

test_that("DSDECOD uses uppercase controlled terminology values", {
  non_na_decod <- ds_final$DSDECOD[!is.na(ds_final$DSDECOD)]
  expect_true(all(non_na_decod == toupper(non_na_decod)))
})

test_that("DSDECOD contains all expected C66727 codelist values", {
  expected_decod <- c("ADVERSE EVENT", "COMPLETED", "DEATH",
                      "LACK OF EFFICACY", "LOST TO FOLLOW-UP",
                      "PHYSICIAN DECISION", "PROTOCOL VIOLATION",
                      "RANDOMIZED", "SCREEN FAILURE",
                      "STUDY TERMINATED BY SPONSOR",
                      "WITHDRAWAL BY SUBJECT")
  actual_decod <- sort(unique(ds_final$DSDECOD[!is.na(ds_final$DSDECOD)]))
  expect_equal(actual_decod, sort(expected_decod))
})

test_that("DSDECOD counts match expected distribution", {
  decod_counts <- ds_final %>%
    filter(!is.na(DSDECOD)) %>%
    count(DSDECOD) %>%
    arrange(DSDECOD)
  expect_equal(decod_counts$n[decod_counts$DSDECOD == "RANDOMIZED"], 254L)
  expect_equal(decod_counts$n[decod_counts$DSDECOD == "COMPLETED"], 110L)
  expect_equal(decod_counts$n[decod_counts$DSDECOD == "SCREEN FAILURE"], 52L)
  expect_equal(decod_counts$n[decod_counts$DSDECOD == "ADVERSE EVENT"], 92L)
})

# ==============================================================================
# 7. DSCAT
# ==============================================================================

test_that("DSCAT has no NA values", {
  expect_equal(sum(is.na(ds_final$DSCAT)), 0L)
})

test_that("DSCAT has exactly three categories", {
  expected_cats <- c("DISPOSITION EVENT", "OTHER EVENT", "PROTOCOL MILESTONE")
  expect_equal(sort(unique(ds_final$DSCAT)), sort(expected_cats))
})

test_that("DSCAT counts match expected distribution", {
  cat_counts <- ds_final %>% count(DSCAT) %>% arrange(DSCAT)
  expect_equal(cat_counts$n[cat_counts$DSCAT == "DISPOSITION EVENT"], 306L)
  expect_equal(cat_counts$n[cat_counts$DSCAT == "OTHER EVENT"], 290L)
  expect_equal(cat_counts$n[cat_counts$DSCAT == "PROTOCOL MILESTONE"], 254L)
})

test_that("RANDOMIZED records map to PROTOCOL MILESTONE", {
  randomized <- ds_final %>% filter(DSDECOD == "RANDOMIZED")
  expect_true(all(randomized$DSCAT == "PROTOCOL MILESTONE"))
  expect_equal(nrow(randomized), 254L)
})

test_that("Non-Randomized DSDECOD records map to DISPOSITION EVENT", {
  disp <- ds_final %>%
    filter(!is.na(DSDECOD), DSDECOD != "RANDOMIZED")
  expect_true(all(disp$DSCAT == "DISPOSITION EVENT"))
})

test_that("Records with NA DSDECOD map to OTHER EVENT", {
  other <- ds_final %>% filter(is.na(DSDECOD))
  expect_true(all(other$DSCAT == "OTHER EVENT"))
  expect_equal(nrow(other), 290L)
})

# ==============================================================================
# 8. VISITNUM AND VISIT
# ==============================================================================

test_that("VISITNUM is numeric and populated for mapped visits", {
  mapped <- ds_final %>%
    filter(!grepl("^UNSCHEDULED", VISIT) & VISIT != "AMBUL ECG REMOVAL")
  expect_equal(sum(is.na(mapped$VISITNUM)), 0L)
})

test_that("VISIT has no NA values", {
  expect_equal(sum(is.na(ds_final$VISIT)), 0L)
})

test_that("Mapped visits have numeric VISITNUM values", {
  mapped <- ds_final %>%
    filter(VISIT %in% c("BASELINE", "WEEK 2", "WEEK 4", "WEEK 6",
                         "WEEK 8", "WEEK 12", "WEEK 16", "WEEK 20",
                         "WEEK 24", "WEEK 26", "SCREENING 1", "RETRIEVAL"))
  expect_true(all(!is.na(mapped$VISITNUM)))
})

test_that("BASELINE visit has VISITNUM = 3", {
  baseline <- ds_final %>% filter(VISIT == "BASELINE")
  expect_true(all(baseline$VISITNUM == 3))
})

# ==============================================================================
# 9. DSDTC (Disposition Date)
# ==============================================================================

test_that("DSDTC has no NA values", {
  expect_equal(sum(is.na(ds_final$DSDTC)), 0L)
})

test_that("DSDTC is in ISO 8601 format (YYYY-MM-DD)", {
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(ds_final$DSDTC))))
})

test_that("DSDTC has iso8601 class from sdtm.oak", {
  expect_true("iso8601" %in% class(ds_final$DSDTC))
})

# ==============================================================================
# 10. DSSTDTC (Disposition Start Date)
# ==============================================================================

test_that("DSSTDTC has no NA values", {
  expect_equal(sum(is.na(ds_final$DSSTDTC)), 0L)
})

test_that("DSSTDTC is character (ISO 8601)", {
  expect_type(ds_final$DSSTDTC, "character")
})

# ==============================================================================
# 11. DSSTDY (Study Day)
# ==============================================================================

test_that("DSSTDY has 52 NA values (Screen Failure subjects)", {
  expect_equal(sum(is.na(ds_final$DSSTDY)), 52L)
})

test_that("DSSTDY = 1 for all Randomized records", {
  randomized <- ds_final %>% filter(DSDECOD == "RANDOMIZED")
  expect_true(all(randomized$DSSTDY == 1L))
})

test_that("DSSTDY range is -16 to 286", {
  expect_equal(min(ds_final$DSSTDY, na.rm = TRUE), -16L)
  expect_equal(max(ds_final$DSSTDY, na.rm = TRUE), 286L)
})

test_that("DSSTDY is integer type", {
  expect_type(ds_final$DSSTDY, "integer")
})

# ==============================================================================
# 12. EDGE CASES
# ==============================================================================

test_that("Screen Failure records have NA DSSTDY (no RFSTDTC)", {
  screen_fail <- ds_final %>% filter(DSDECOD == "SCREEN FAILURE")
  expect_true(all(is.na(screen_fail$DSSTDY)))
  expect_equal(nrow(screen_fail), 52L)
})

test_that("Negative DSSTDY values exist for events before reference date", {
  negative_dy <- ds_final %>% filter(DSSTDY < 0)
  expect_equal(nrow(negative_dy), 7L)
  # All negative DSSTDY records are OTHER EVENT (Final Lab Visit)
  expect_true(all(negative_dy$DSCAT == "OTHER EVENT"))
  expect_true(all(negative_dy$DSTERM == "Final Lab Visit"))
})

test_that("Unscheduled visits have NA VISITNUM (no codelist mapping)", {
  unsched <- ds_final %>% filter(grepl("^UNSCHEDULED", VISIT))
  expect_gt(nrow(unsched), 0L)
  # Unmapped visits produce NA when converted to numeric
  expect_true(all(is.na(unsched$VISITNUM)))
})

test_that("AMBUL ECG REMOVAL visit has NA VISITNUM (no codelist mapping)", {
  ambul <- ds_final %>% filter(VISIT == "AMBUL ECG REMOVAL")
  expect_equal(nrow(ambul), 4L)
  expect_true(all(is.na(ambul$VISITNUM)))
})

test_that("OTHER EVENT records: DSDECOD is always NA", {
  other <- ds_final %>% filter(DSCAT == "OTHER EVENT")
  expect_true(all(is.na(other$DSDECOD)))
})

test_that("DEATH records exist and are categorized correctly", {
  death <- ds_final %>% filter(DSDECOD == "DEATH")
  expect_equal(nrow(death), 3L)
  expect_true(all(death$DSCAT == "DISPOSITION EVENT"))
})

test_that("Subjects with 4 records have both DISPOSITION EVENT and OTHER EVENT", {
  subj_4 <- ds_final %>%
    group_by(USUBJID) %>%
    filter(n() == 4L) %>%
    ungroup()
  # Each such subject should have records in multiple categories
  cats_per_subj <- subj_4 %>%
    group_by(USUBJID) %>%
    summarise(n_cats = n_distinct(DSCAT), .groups = "drop")
  expect_true(all(cats_per_subj$n_cats >= 2L))
})

test_that("No duplicate rows exist in the final dataset", {
  expect_equal(nrow(ds_final), nrow(distinct(ds_final)))
})

test_that("DSDTC and DSSTDTC differ for at most 1 record (different raw sources)", {
  # DSDTC comes from DSDTCOL, DSSTDTC from IT.DSSTDAT — they can legitimately differ
  dsdtc_as_date <- as.Date(as.character(ds_final$DSDTC))
  n_mismatch <- sum(dsdtc_as_date != ds_final$DSSTDTC)
  expect_lte(n_mismatch, 1L)
})

test_that("DSDTC and DSSTDTC are both valid dates for all records", {
  dsdtc_as_date <- as.Date(as.character(ds_final$DSDTC))
  expect_true(all(!is.na(dsdtc_as_date)))
  expect_true(all(!is.na(ds_final$DSSTDTC)))
})

# ==============================================================================
# Run and report
# ==============================================================================
cat("\n========================================\n")
cat("DS Domain Test Suite Complete\n")
cat("========================================\n")
