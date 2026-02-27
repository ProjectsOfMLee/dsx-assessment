# ==============================================================================
# Question 2: SDTM DS Domain Creation using {sdtm.oak}
#
# Creates the Disposition (DS) domain from pharmaverseraw::ds_raw using
# the {sdtm.oak} package, following the Pharmaverse AE example pattern.
#
# Required output variables:
#   STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT,
#   VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
# ==============================================================================

# --- Load packages ------------------------------------------------------------
library(sdtm.oak)
library(pharmaverseraw)
library(pharmaversesdtm)
library(dplyr)

# --- Read raw data ------------------------------------------------------------
ds_raw <- pharmaverseraw::ds_raw

# Read DM domain (needed for derive_study_day)
dm <- pharmaversesdtm::dm

# --- Read controlled terminology ----------------------------------------------
# The study CT is provided in the requirements. We use the Pharmaverse examples
# CT spec which contains the C66727 codelist for disposition decoding,
# C74558 for DSCAT, and VISITNUM/VISIT codelists.
# Try multiple paths to handle both sourcing from project root and from this directory.
ct_paths <- c("metadata/sdtm_ct.csv",
              "question_2_sdtm/metadata/sdtm_ct.csv")
ct_path <- ct_paths[file.exists(ct_paths)][1]
study_ct <- read.csv(ct_path, stringsAsFactors = FALSE)

# --- Generate oak_id_vars -----------------------------------------------------
# These internal ID variables are required by sdtm.oak mapping functions
ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

# --- Map Topic Variable: DSTERM -----------------------------------------------
# DSTERM is the primary topic variable for the DS domain.
# Map from IT.DSTERM for standard disposition records.
ds <- assign_no_ct(
  raw_dat = ds_raw,
  raw_var = "IT.DSTERM",
  tgt_var = "DSTERM",
  id_vars = oak_id_vars()
)

# For "Other Event" records (IT.DSTERM is NA), map OTHERSP to DSTERM
ds <- ds %>%
  assign_no_ct(
    raw_dat = condition_add(ds_raw, is.na(IT.DSTERM) & !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )

# --- Map Qualifier Variables ---------------------------------------------------

# DSDECOD: Standardized disposition decoded term using C66727 codelist
ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  )

# DSCAT: Disposition category using C74558 codelist
# "Randomized" maps to "PROTOCOL MILESTONE", all others to "DISPOSITION EVENT"
# We use hardcode_ct with condition_add to set DSCAT based on DSDECOD value

# First, handle PROTOCOL MILESTONE for Randomized records
ds <- ds %>%
  hardcode_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD == "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  )

# Then, handle DISPOSITION EVENT for non-Randomized records
ds <- ds %>%
  hardcode_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD != "Randomized" & !is.na(IT.DSDECOD)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  )

# Handle OTHER EVENT for records where IT.DSDECOD is NA but OTHERSP is populated
ds <- ds %>%
  hardcode_ct(
    raw_dat = condition_add(ds_raw, is.na(IT.DSDECOD) & !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  )

# --- Map Timing Variables ------------------------------------------------------

# DSDTC: Date/time of disposition event (from DSDTCOL date and DSTMCOL time)
ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "DSDTCOL",
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y"),
    id_vars = oak_id_vars()
  )

# DSSTDTC: Start date/time of disposition event (from IT.DSSTDAT)
ds <- ds %>%
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y"),
    id_vars = oak_id_vars()
  )

# VISITNUM: Visit number mapped from INSTANCE using VISITNUM codelist
ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )

# VISIT: Visit name mapped from INSTANCE using VISIT codelist
ds <- ds %>%
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  )

# --- Create SDTM derived variables --------------------------------------------

# STUDYID: from raw STUDY variable
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "STUDY",
    tgt_var = "STUDYID",
    id_vars = oak_id_vars()
  )

# DOMAIN: hardcoded to "DS"
ds <- ds %>%
  hardcode_no_ct(
    raw_dat = ds_raw,
    raw_var = "STUDY",
    tgt_var = "DOMAIN",
    tgt_val = "DS",
    id_vars = oak_id_vars()
  )

# USUBJID: derived from PATNUM with study prefix
ds <- ds %>%
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "PATNUM",
    tgt_var = "USUBJID",
    id_vars = oak_id_vars()
  ) %>%
  dplyr::mutate(USUBJID = paste0("01-", USUBJID))

# DSSEQ: Sequence number within subject
ds <- ds %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) %>%
  # DSSTDY: Study day of disposition start date relative to RFSTDTC
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY"
  )

# --- Enforce SDTM-compliant types ---------------------------------------------
# DSSTDTC: assign_datetime returns Date; SDTM --DTC variables must be character
# VISITNUM: assign_ct returns character; SDTM requires numeric
ds <- ds %>%
  dplyr::mutate(
    DSSTDTC = as.character(DSSTDTC),
    VISITNUM = suppressWarnings(as.numeric(VISITNUM))
  )

# --- Select and order final variables -----------------------------------------
ds_final <- ds %>%
  dplyr::select(
    STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT,
    VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY
  )

# --- Display results ----------------------------------------------------------
cat("=== DS Domain Summary ===\n")
cat("Rows:", nrow(ds_final), "\n")
cat("Columns:", paste(names(ds_final), collapse = ", "), "\n\n")

cat("=== First 10 rows ===\n")
print(head(ds_final, 10))

cat("\n=== Variable classes ===\n")
str(ds_final)

cat("\n=== DSCAT distribution ===\n")
print(table(ds_final$DSCAT, useNA = "ifany"))

cat("\n=== DSDECOD distribution ===\n")
print(table(ds_final$DSDECOD, useNA = "ifany"))
