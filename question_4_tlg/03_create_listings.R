# ==============================================================================
# Question 4.3: AE Listing using {gt}
#
# Creates a detailed listing of treatment-emergent adverse events with:
# Subject ID, Treatment, AE Term, Severity, Relationship, Start/End dates
# Filtered for treatment-emergent events, excluding Screen Failure
# Sorted by subject and event date
#
# Output: ae_listings.html
# ==============================================================================

library(gt)
library(pharmaverseadam)
library(dplyr)

# --- Load data ----------------------------------------------------------------
adae <- pharmaverseadam::adae

# Filter to TEAEs, exclude Screen Failure
adae_listing <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(ACTARM != "Screen Failure") %>%
  arrange(USUBJID, ASTDT) %>%
  select(
    USUBJID,
    ACTARM,
    AEDECOD,
    AESEV,
    AEREL,
    AESTDTC,
    AEENDTC
  ) %>%
  # Replace NA end dates with "NA" string for display
  mutate(
    AEENDTC = if_else(is.na(AEENDTC), "NA", AEENDTC)
  )

# --- Create gt listing --------------------------------------------------------
ae_listing_table <- gt(adae_listing) %>%
  tab_header(
    title = "Listing of Treatment-Emergent Adverse Events by Subject",
    subtitle = "Excluding Screen Failure Patients"
  ) %>%
  cols_label(
    USUBJID = "Unique Subject Identifier",
    ACTARM = "Description of Actual Arm",
    # Sample output labels this as "Reported Term" though AEDECOD is
    # technically "Dictionary-Derived Term" in CDISC. We match the sample.
    AEDECOD = "Reported Term for the Adverse Event",
    AESEV = "Severity/Intensity",
    AEREL = "Causality",
    AESTDTC = "Start Date/Time of Adverse Event",
    AEENDTC = "End Date/Time of Adverse Event"
  ) %>%
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    table.width = pct(100)
  ) %>%
  cols_align(align = "left")

# Save as HTML
gtsave(ae_listing_table, filename = "question_4_tlg/ae_listings.html")
cat("ae_listings.html saved successfully.\n")
cat("Total TEAE records in listing:", nrow(adae_listing), "\n")
