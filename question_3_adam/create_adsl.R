# ==============================================================================
# Question 3: ADaM ADSL Dataset Creation
#
# Creates an ADSL (Subject Level) dataset from SDTM source data using
# {admiral} and tidyverse tools.
#
# Derived variables:
#   AGEGR9 / AGEGR9N  - Age grouping (<18, 18-50, >50)
#   TRTSDTM / TRTSTMF - Treatment start datetime with time imputation
#   ITTFL             - Intent-to-treat flag (Y if ARM is populated)
#   ABNSBPFL          - Abnormal supine systolic BP flag
#   LSTALVDT          - Last known alive date
#   CARPOPFL          - Cardiac adverse event population flag
#
# Input: pharmaversesdtm::dm, vs, ex, ds, ae
# ==============================================================================

# --- Load packages ------------------------------------------------------------
library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# --- Read input SDTM data ----------------------------------------------------
dm <- pharmaversesdtm::dm
vs <- pharmaversesdtm::vs
ex <- pharmaversesdtm::ex
ds <- pharmaversesdtm::ds
ae <- pharmaversesdtm::ae

# Convert blank strings to NA (standard practice for SAS-imported data)
dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
ex <- convert_blanks_to_na(ex)
ds <- convert_blanks_to_na(ds)
ae <- convert_blanks_to_na(ae)

# --- Start with DM as the basis of ADSL --------------------------------------
adsl <- dm %>%
  select(
    STUDYID, USUBJID, SUBJID, RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC,
    DTHDTC, DTHFL, SITEID, AGE, AGEU, SEX, RACE, ETHNIC,
    ARMCD, ARM, ACTARMCD, ACTARM, COUNTRY
  )

# ==============================================================================
# AGEGR9 / AGEGR9N: Age grouping
# Categories: "<18" (1), "18 - 50" (2), ">50" (3)
# ==============================================================================
agegr9_lookup <- exprs(
  ~condition,             ~AGEGR9,    ~AGEGR9N,
  AGE < 18,              "<18",       1,
  between(AGE, 18, 50),  "18 - 50",   2,
  AGE > 50,              ">50",       3
)

adsl <- derive_vars_cat(
  dataset = adsl,
  definition = agegr9_lookup
)

# ==============================================================================
# TRTSDTM / TRTSTMF: Treatment start datetime
#
# Set to the datetime of the patient's first exposure where:
#   - Valid dose: EXDOSE > 0 OR (EXDOSE == 0 AND EXTRT contains "PLACEBO")
#   - Date part of EXSTDTC is complete
# Time imputation: missing time -> 00:00:00, partially missing -> 00 for
# missing components. If only seconds are missing, do NOT set imputation flag.
# ==============================================================================

# Derive datetime from EX start date, imputing missing time parts
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "h",       # impute up to hours
    time_imputation = "00:00:00"    # missing time -> 00:00:00
  )

# Also derive TRTEDTM for use in LSTALVDT derivation
ex_ext <- ex_ext %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "h",
    time_imputation = "last"
  )

# Merge first valid exposure start datetime into ADSL
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Also derive TRTEDTM (last valid exposure end) for LSTALVDT
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

# Derive date-only versions for convenience
adsl <- adsl %>%
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))

# ==============================================================================
# ITTFL: Intent-to-Treat Flag
# "Y" if DM.ARM is not missing, else "N"
# ==============================================================================
adsl <- adsl %>%
  mutate(ITTFL = if_else(!is.na(ARM), "Y", "N"))

# ==============================================================================
# ABNSBPFL: Abnormal Supine Systolic Blood Pressure Flag
# "Y" if patient has any VS observation where:
#   VSTESTCD = "SYSBP" AND VSPOS = "SUPINE" AND VSSTRESU = "mmHg"
#   AND (VSSTRESN >= 140 OR VSSTRESN < 100)
# Else "N"
#
# Note: The detailed spec omits VSPOS, but the variable name specifies
# "supine." The data contains both STANDING and SUPINE SYSBP records,
# so we filter to SUPINE to match the variable's clinical intent.
# ==============================================================================
adsl <- adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = vs,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = ABNSBPFL,
    condition = VSTESTCD == "SYSBP" &
      VSPOS == "SUPINE" &
      VSSTRESU == "mmHg" &
      (VSSTRESN >= 140 | VSSTRESN < 100),
    false_value = "N",
    missing_value = "N"
  )

# ==============================================================================
# LSTALVDT: Last Known Alive Date
#
# Maximum of:
# (1) Last complete VS date with valid result (VSSTRESN and VSSTRESC not both NA)
# (2) Last complete AE onset date (AESTDTC)
# (3) Last complete DS disposition date (DSSTDTC)
# (4) Last date of valid treatment (TRTEDTM date part)
# ==============================================================================

# Prepare VS dates: filter to valid results with complete dates
vs_dates <- vs %>%
  filter(!(is.na(VSSTRESN) & is.na(VSSTRESC))) %>%
  derive_vars_dt(dtc = VSDTC, new_vars_prefix = "VS") %>%
  drop_na(VSDT)

# Prepare AE dates: complete onset dates only
ae_dates <- ae %>%
  derive_vars_dt(dtc = AESTDTC, new_vars_prefix = "AEST") %>%
  drop_na(AESTDT)

# Prepare DS dates: complete disposition dates only
ds_dates <- ds %>%
  derive_vars_dt(dtc = DSSTDTC, new_vars_prefix = "DSST") %>%
  drop_na(DSSTDT)

# Derive LSTALVDT using derive_vars_extreme_event
# Takes the maximum date across VS, AE, DS, and treatment end dates.
# Each event() selects records from a source dataset and maps the relevant
# date to LSTALVDT. mode="last" with order by LSTALVDT picks the latest.
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "vs",
        condition = !is.na(VSDT),
        set_values_to = exprs(LSTALVDT = VSDT)
      ),
      event(
        dataset_name = "ae",
        condition = !is.na(AESTDT),
        set_values_to = exprs(LSTALVDT = AESTDT)
      ),
      event(
        dataset_name = "ds",
        condition = !is.na(DSSTDT),
        set_values_to = exprs(LSTALVDT = DSSTDT)
      ),
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDT),
        set_values_to = exprs(LSTALVDT = TRTEDT)
      )
    ),
    source_datasets = list(
      vs = vs_dates,
      ae = ae_dates,
      ds = ds_dates,
      adsl = adsl
    ),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTALVDT),
    mode = "last",
    new_vars = exprs(LSTALVDT),
    check_type = "none"
  )

# ==============================================================================
# CARPOPFL: Cardiac Adverse Event Population Flag
# "Y" if patient has any AE where uppercase(AESOC) = "CARDIAC DISORDERS"
# Else NA (missing)
# ==============================================================================
adsl <- adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = ae,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = CARPOPFL,
    condition = toupper(AESOC) == "CARDIAC DISORDERS",
    false_value = NA_character_,
    missing_value = NA_character_
  )

# ==============================================================================
# Final dataset
# ==============================================================================
cat("=== ADSL Summary ===\n")
cat("Rows:", nrow(adsl), "\n")
cat("Columns:", ncol(adsl), "\n\n")

# Show the derived variables
cat("=== Derived Variables ===\n")
cat("AGEGR9 distribution:\n")
print(table(adsl$AGEGR9, useNA = "ifany"))
cat("\nAGEGR9N distribution:\n")
print(table(adsl$AGEGR9N, useNA = "ifany"))

cat("\nTRTSDTM sample (first 5):\n")
print(head(adsl$TRTSDTM, 5))
cat("TRTSTMF sample (first 5):\n")
print(head(adsl$TRTSTMF, 5))

cat("\nITTFL distribution:\n")
print(table(adsl$ITTFL, useNA = "ifany"))

cat("\nABNSBPFL distribution:\n")
print(table(adsl$ABNSBPFL, useNA = "ifany"))

cat("\nLSTALVDT sample (first 5):\n")
print(head(adsl$LSTALVDT, 5))
cat("LSTALVDT NAs:", sum(is.na(adsl$LSTALVDT)), "/", nrow(adsl), "\n")

cat("\nCARPOPFL distribution:\n")
print(table(adsl$CARPOPFL, useNA = "ifany"))

cat("\n=== First 5 rows of key variables ===\n")
print(
  adsl %>%
    select(USUBJID, AGE, AGEGR9, AGEGR9N, TRTSDTM, TRTSTMF,
           ITTFL, ABNSBPFL, LSTALVDT, CARPOPFL) %>%
    head(5)
)
