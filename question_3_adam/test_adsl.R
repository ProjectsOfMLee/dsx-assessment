# ==============================================================================
# Test Suite for Question 3: ADaM ADSL Dataset
#
# Validates the ADSL output from create_adsl.R against the specification:
#   AGEGR9/AGEGR9N, TRTSDTM/TRTSTMF, ITTFL, ABNSBPFL, LSTALVDT, CARPOPFL
#
# Run: Rscript question_3_adam/test_adsl.R
# ==============================================================================

library(testthat)
library(dplyr)
library(lubridate)
library(stringr)

# --- Source the ADSL script to produce adsl -----------------------------------
suppressMessages(source("question_3_adam/create_adsl.R"))

# Keep raw source data for cross-validation
dm_raw <- pharmaversesdtm::dm
vs_raw <- pharmaversesdtm::vs
ex_raw <- pharmaversesdtm::ex
ae_raw <- pharmaversesdtm::ae

cat("\n\n========================================\n")
cat("Running ADSL Test Suite\n")
cat("========================================\n\n")

# ==============================================================================
# 1. STRUCTURAL TESTS
# ==============================================================================

test_that("ADSL has 306 rows (one per DM subject)", {
  expect_equal(nrow(adsl), 306L)
})

test_that("ADSL has 31 columns", {
  expect_equal(ncol(adsl), 31L)
})

test_that("ADSL has one row per subject (no duplicates)", {
  expect_equal(nrow(adsl), n_distinct(adsl$USUBJID))
})

test_that("ADSL subject count matches DM", {
  expect_equal(n_distinct(adsl$USUBJID), n_distinct(dm_raw$USUBJID))
})

test_that("ADSL contains all required derived variables", {
  required <- c("AGEGR9", "AGEGR9N", "TRTSDTM", "TRTSTMF",
                "ITTFL", "ABNSBPFL", "LSTALVDT", "CARPOPFL")
  for (var in required) {
    expect_true(var %in% names(adsl), info = paste("Missing:", var))
  }
})

test_that("ADSL preserves key DM variables", {
  dm_vars <- c("STUDYID", "USUBJID", "SUBJID", "AGE", "AGEU",
               "SEX", "RACE", "ETHNIC", "ARM", "ARMCD", "COUNTRY")
  for (var in dm_vars) {
    expect_true(var %in% names(adsl), info = paste("Missing DM var:", var))
  }
})

test_that("ADSL is a data.frame", {
  expect_s3_class(adsl, "data.frame")
})

test_that("No duplicate rows exist", {
  expect_equal(nrow(adsl), nrow(distinct(adsl)))
})

# ==============================================================================
# 2. COLUMN TYPES
# ==============================================================================

test_that("AGEGR9 is character", {
  expect_type(adsl$AGEGR9, "character")
})

test_that("AGEGR9N is numeric", {
  expect_type(adsl$AGEGR9N, "double")
})

test_that("TRTSDTM is POSIXct", {
  expect_s3_class(adsl$TRTSDTM, "POSIXct")
})

test_that("TRTSTMF is character", {
  expect_type(adsl$TRTSTMF, "character")
})

test_that("ITTFL is character", {
  expect_type(adsl$ITTFL, "character")
})

test_that("ABNSBPFL is character", {
  expect_type(adsl$ABNSBPFL, "character")
})

test_that("LSTALVDT is Date", {
  expect_s3_class(adsl$LSTALVDT, "Date")
})

test_that("CARPOPFL is character", {
  expect_type(adsl$CARPOPFL, "character")
})

test_that("TRTSDT is Date", {
  expect_s3_class(adsl$TRTSDT, "Date")
})

test_that("TRTEDT is Date", {
  expect_s3_class(adsl$TRTEDT, "Date")
})

# ==============================================================================
# 3. AGEGR9 / AGEGR9N
# ==============================================================================

test_that("AGEGR9 has no NA values", {
  expect_equal(sum(is.na(adsl$AGEGR9)), 0L)
})

test_that("AGEGR9N has no NA values", {
  expect_equal(sum(is.na(adsl$AGEGR9N)), 0L)
})

test_that("AGEGR9 categories are correct", {
  valid_cats <- c("<18", "18 - 50", ">50")
  expect_true(all(adsl$AGEGR9 %in% valid_cats))
})

test_that("AGEGR9N values are 1, 2, or 3", {
  expect_true(all(adsl$AGEGR9N %in% c(1, 2, 3)))
})

# Not testable: no subjects with AGE < 18 exist in pharmaversesdtm::dm (min AGE = 50).
# test_that("AGEGR9 correctly maps AGE < 18 to '<18'", {
#   under18 <- adsl %>% filter(AGE < 18)
#   expect_true(all(under18$AGEGR9 == "<18"))
#   expect_true(all(under18$AGEGR9N == 1))
# })

test_that("AGEGR9 correctly maps AGE 18-50 to '18 - 50'", {
  mid <- adsl %>% filter(AGE >= 18, AGE <= 50)
  expect_true(all(mid$AGEGR9 == "18 - 50"))
  expect_true(all(mid$AGEGR9N == 2))
})

test_that("AGEGR9 correctly maps AGE > 50 to '>50'", {
  over50 <- adsl %>% filter(AGE > 50)
  expect_true(all(over50$AGEGR9 == ">50"))
  expect_true(all(over50$AGEGR9N == 3))
})

test_that("AGEGR9 distribution: 1 subject 18-50, 305 subjects >50", {
  expect_equal(sum(adsl$AGEGR9 == "18 - 50"), 1L)
  expect_equal(sum(adsl$AGEGR9 == ">50"), 305L)
})

test_that("AGEGR9 boundary: AGE=50 maps to '18 - 50'", {
  age50 <- adsl %>% filter(AGE == 50)
  expect_equal(nrow(age50), 1L)
  expect_equal(age50$AGEGR9, "18 - 50")
  expect_equal(age50$AGEGR9N, 2)
})

test_that("AGEGR9N is consistent with AGEGR9", {
  mapping <- adsl %>% distinct(AGEGR9, AGEGR9N) %>% arrange(AGEGR9N)
  # Each AGEGR9 should map to exactly one AGEGR9N
  expect_equal(nrow(mapping), n_distinct(adsl$AGEGR9))
})

# ==============================================================================
# 4. TRTSDTM / TRTSTMF
# ==============================================================================

test_that("TRTSDTM has 52 NA values (Screen Failure subjects)", {
  expect_equal(sum(is.na(adsl$TRTSDTM)), 52L)
})

test_that("TRTSTMF has 52 NA values (Screen Failure subjects)", {
  expect_equal(sum(is.na(adsl$TRTSTMF)), 52L)
})

test_that("TRTSTMF is 'H' for all non-NA TRTSDTM (time imputed at hour level)", {
  non_na <- adsl %>% filter(!is.na(TRTSDTM))
  expect_true(all(non_na$TRTSTMF == "H"))
})

test_that("TRTSDTM time is 00:00:00 for all imputed records", {
  non_na <- adsl %>% filter(!is.na(TRTSDTM))
  expect_true(all(hour(non_na$TRTSDTM) == 0))
  expect_true(all(minute(non_na$TRTSDTM) == 0))
  expect_true(all(second(non_na$TRTSDTM) == 0))
})

test_that("TRTSDTM timezone is UTC", {
  expect_equal(attr(adsl$TRTSDTM[!is.na(adsl$TRTSDTM)][1], "tzone"), "UTC")
})

test_that("TRTSDTM corresponds to first valid exposure date", {
  ex_clean <- admiral::convert_blanks_to_na(ex_raw)
  first_ex <- ex_clean %>%
    filter(EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) %>%
    filter(!is.na(EXSTDTC)) %>%
    group_by(STUDYID, USUBJID) %>%
    arrange(EXSTDTC, EXSEQ) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(ex_date = as.Date(substr(EXSTDTC, 1, 10))) %>%
    select(USUBJID, ex_date)
  merged <- adsl %>%
    filter(!is.na(TRTSDT)) %>%
    select(USUBJID, TRTSDT) %>%
    left_join(first_ex, by = "USUBJID")
  expect_true(all(merged$TRTSDT == merged$ex_date))
})

test_that("TRTSDT is the date part of TRTSDTM", {
  non_na <- adsl %>% filter(!is.na(TRTSDTM))
  expect_equal(as.Date(non_na$TRTSDTM), non_na$TRTSDT)
})

test_that("TRTEDT is the date part of TRTEDTM", {
  non_na <- adsl %>% filter(!is.na(TRTEDTM))
  expect_equal(as.Date(non_na$TRTEDTM), non_na$TRTEDT)
})

# ==============================================================================
# 5. ITTFL
# ==============================================================================

test_that("ITTFL has no NA values", {
  expect_equal(sum(is.na(adsl$ITTFL)), 0L)
})

test_that("ITTFL is 'Y' or 'N' only", {
  expect_true(all(adsl$ITTFL %in% c("Y", "N")))
})

test_that("ITTFL = 'Y' when ARM is populated", {
  arm_pop <- adsl %>% filter(!is.na(ARM))
  expect_true(all(arm_pop$ITTFL == "Y"))
})

# Not testable: all 306 subjects in pharmaversesdtm::dm have ARM populated (including Screen Failure).
# test_that("ITTFL = 'N' when ARM is missing", {
#   arm_na <- adsl %>% filter(is.na(ARM))
#   expect_true(all(arm_na$ITTFL == "N"))
# })

test_that("All 306 subjects have ITTFL = 'Y' (all have ARM populated)", {
  expect_equal(sum(adsl$ITTFL == "Y"), 306L)
  expect_equal(sum(is.na(adsl$ARM)), 0L)
})

# ==============================================================================
# 6. ABNSBPFL
# ==============================================================================

test_that("ABNSBPFL has no NA values", {
  expect_equal(sum(is.na(adsl$ABNSBPFL)), 0L)
})

test_that("ABNSBPFL is 'Y' or 'N' only", {
  expect_true(all(adsl$ABNSBPFL %in% c("Y", "N")))
})

test_that("ABNSBPFL distribution: Y=213, N=93 (supine only)", {
  expect_equal(sum(adsl$ABNSBPFL == "Y"), 213L)
  expect_equal(sum(adsl$ABNSBPFL == "N"), 93L)
})

test_that("ABNSBPFL=Y matches subjects with abnormal supine SYSBP in VS", {
  abnormal_subj <- vs_raw %>%
    filter(VSTESTCD == "SYSBP", VSPOS == "SUPINE", VSSTRESU == "mmHg",
           (VSSTRESN >= 140 | VSSTRESN < 100)) %>%
    distinct(USUBJID) %>%
    pull(USUBJID)
  adsl_y <- adsl %>% filter(ABNSBPFL == "Y") %>% pull(USUBJID)
  expect_equal(sort(adsl_y), sort(abnormal_subj))
})

test_that("ABNSBPFL=N subjects have no abnormal supine SYSBP records in VS", {
  normal_subj <- adsl %>% filter(ABNSBPFL == "N") %>% pull(USUBJID)
  abnormal_count <- vs_raw %>%
    filter(USUBJID %in% normal_subj,
           VSTESTCD == "SYSBP", VSPOS == "SUPINE", VSSTRESU == "mmHg",
           (VSSTRESN >= 140 | VSSTRESN < 100)) %>%
    nrow()
  expect_equal(abnormal_count, 0L)
})

# ==============================================================================
# 7. LSTALVDT
# ==============================================================================

test_that("LSTALVDT has no NA values", {
  expect_equal(sum(is.na(adsl$LSTALVDT)), 0L)
})

test_that("LSTALVDT is Date class", {
  expect_s3_class(adsl$LSTALVDT, "Date")
})

test_that("LSTALVDT range is 2012-08-13 to 2015-03-05", {
  expect_equal(min(adsl$LSTALVDT), as.Date("2012-08-13"))
  expect_equal(max(adsl$LSTALVDT), as.Date("2015-03-05"))
})

test_that("LSTALVDT >= TRTSDT for all treated subjects", {
  treated <- adsl %>% filter(!is.na(TRTSDT))
  expect_true(all(treated$LSTALVDT >= treated$TRTSDT))
})

test_that("LSTALVDT >= TRTEDT for all subjects with treatment end", {
  with_end <- adsl %>% filter(!is.na(TRTEDT))
  expect_true(all(with_end$LSTALVDT >= with_end$TRTEDT))
})

# ==============================================================================
# 8. CARPOPFL
# ==============================================================================

test_that("CARPOPFL is 'Y' or NA only", {
  non_na <- adsl$CARPOPFL[!is.na(adsl$CARPOPFL)]
  expect_true(all(non_na == "Y"))
})

test_that("CARPOPFL distribution: Y=44, NA=262", {
  expect_equal(sum(adsl$CARPOPFL == "Y", na.rm = TRUE), 44L)
  expect_equal(sum(is.na(adsl$CARPOPFL)), 262L)
})

test_that("CARPOPFL=Y matches subjects with cardiac AE in AE domain", {
  cardiac_subj <- ae_raw %>%
    filter(toupper(AESOC) == "CARDIAC DISORDERS") %>%
    distinct(USUBJID) %>%
    pull(USUBJID)
  adsl_y <- adsl %>%
    filter(CARPOPFL == "Y") %>%
    pull(USUBJID)
  expect_equal(sort(adsl_y), sort(cardiac_subj))
})

test_that("CARPOPFL=NA subjects have no cardiac AE records", {
  no_cardiac <- adsl %>% filter(is.na(CARPOPFL)) %>% pull(USUBJID)
  cardiac_count <- ae_raw %>%
    filter(USUBJID %in% no_cardiac, toupper(AESOC) == "CARDIAC DISORDERS") %>%
    nrow()
  expect_equal(cardiac_count, 0L)
})

# ==============================================================================
# 9. EDGE CASES: SCREEN FAILURE SUBJECTS
# ==============================================================================

test_that("Screen Failure subjects: 52 subjects with ARMCD=Scrnfail", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")
  expect_equal(nrow(sf), 52L)
})

test_that("Screen Failure: ITTFL is still 'Y' (ARM is populated as 'Screen Failure')", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")
  expect_true(all(sf$ITTFL == "Y"))
  expect_true(all(sf$ARM == "Screen Failure"))
})

test_that("Screen Failure: all treatment dates are NA", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")
  expect_true(all(is.na(sf$TRTSDTM)))
  expect_true(all(is.na(sf$TRTSDT)))
  expect_true(all(is.na(sf$TRTEDTM)))
  expect_true(all(is.na(sf$TRTEDT)))
  expect_true(all(is.na(sf$TRTSTMF)))
})

test_that("Screen Failure: ABNSBPFL is 'N' for all (no VS data)", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")
  expect_true(all(sf$ABNSBPFL == "N"))
})

test_that("Screen Failure: LSTALVDT is still populated (from screening data)", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")
  expect_equal(sum(is.na(sf$LSTALVDT)), 0L)
})

test_that("Screen Failure: CARPOPFL is NA for all (no AE data)", {
  sf <- adsl %>% filter(ARMCD == "Scrnfail")
  expect_true(all(is.na(sf$CARPOPFL)))
})

# ==============================================================================
# 10. EDGE CASES: DEATH SUBJECTS
# ==============================================================================

test_that("Death subjects: 3 subjects with DTHFL='Y'", {
  death <- adsl %>% filter(DTHFL == "Y")
  expect_equal(nrow(death), 3L)
})

test_that("Death subjects: all have ABNSBPFL='Y'", {
  death <- adsl %>% filter(DTHFL == "Y")
  expect_true(all(death$ABNSBPFL == "Y"))
})

test_that("Death subjects: LSTALVDT is populated", {
  death <- adsl %>% filter(DTHFL == "Y")
  expect_equal(sum(is.na(death$LSTALVDT)), 0L)
})

# ==============================================================================
# 11. EDGE CASES: BOUNDARY AGE VALUES
# ==============================================================================

test_that("AGE=50 (boundary) maps to '18 - 50' with AGEGR9N=2", {
  age50 <- adsl %>% filter(AGE == 50)
  expect_equal(nrow(age50), 1L)
  expect_equal(age50$AGEGR9, "18 - 50")
  expect_equal(age50$AGEGR9N, 2)
})

test_that("No subjects with AGE < 18 in this dataset", {
  expect_equal(sum(adsl$AGE < 18), 0L)
})

test_that("No subjects with AGE = 18 in this dataset", {
  expect_equal(sum(adsl$AGE == 18), 0L)
})

test_that("AGE has no NA values", {
  expect_equal(sum(is.na(adsl$AGE)), 0L)
})

test_that("AGE range is 50 to 89", {
  expect_equal(min(adsl$AGE), 50)
  expect_equal(max(adsl$AGE), 89)
})

# ==============================================================================
# 12. EDGE CASES: TREATMENT DATE CONSISTENCY
# ==============================================================================

test_that("TRTSDT <= TRTEDT for all subjects with both dates", {
  both <- adsl %>% filter(!is.na(TRTSDT), !is.na(TRTEDT))
  expect_true(all(both$TRTSDT <= both$TRTEDT))
})

test_that("TRTSDTM <= TRTEDTM for all subjects with both datetimes", {
  both <- adsl %>% filter(!is.na(TRTSDTM), !is.na(TRTEDTM))
  expect_true(all(both$TRTSDTM <= both$TRTEDTM))
})

test_that("TRTSDTM NA count (52) <= TRTEDTM NA count (54)", {
  expect_equal(sum(is.na(adsl$TRTSDTM)), 52L)
  expect_equal(sum(is.na(adsl$TRTEDTM)), 54L)
  expect_lte(sum(is.na(adsl$TRTSDTM)), sum(is.na(adsl$TRTEDTM)))
})

# ==============================================================================
# 13. EDGE CASES: STUDYID CONSISTENCY
# ==============================================================================

test_that("STUDYID is always CDISCPILOT01", {
  expect_true(all(adsl$STUDYID == "CDISCPILOT01"))
})

test_that("USUBJID has no NA values", {
  expect_equal(sum(is.na(adsl$USUBJID)), 0L)
})

# ==============================================================================
# 14. CROSS-VARIABLE CONSISTENCY
# ==============================================================================

test_that("Subjects with TRTSDTM have ITTFL='Y'", {
  treated <- adsl %>% filter(!is.na(TRTSDTM))
  expect_true(all(treated$ITTFL == "Y"))
})

test_that("CARPOPFL='Y' subjects are a subset of ITTFL='Y' subjects", {
  cardiac <- adsl %>% filter(CARPOPFL == "Y")
  expect_true(all(cardiac$ITTFL == "Y"))
})

test_that("All ABNSBPFL='Y' subjects have at least one VS record", {
  abnormal_subj <- adsl %>% filter(ABNSBPFL == "Y") %>% pull(USUBJID)
  vs_subj <- vs_raw %>%
    filter(USUBJID %in% abnormal_subj) %>%
    distinct(USUBJID) %>%
    pull(USUBJID)
  expect_equal(length(abnormal_subj), length(vs_subj))
})

# ==============================================================================
# Run and report
# ==============================================================================
cat("\n========================================\n")
cat("ADSL Test Suite Complete\n")
cat("========================================\n")
