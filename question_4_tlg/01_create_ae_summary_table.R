# ==============================================================================
# Question 4.1: AE Summary Table using {gtsummary}
#
# Creates a hierarchical summary table of treatment-emergent adverse events
# grouped by System Organ Class (AESOC) and Preferred Term (AEDECOD),
# with columns by treatment arm (ACTARM).
#
# Uses tbl_hierarchical() from {gtsummary} for the SOC/PT hierarchy,
# following the FDA Table 10 pattern.
#
# Output: ae_summary_table.html
# ==============================================================================

library(gtsummary)
library(pharmaverseadam)
library(dplyr)

# --- Load data ----------------------------------------------------------------
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# Filter to treatment-emergent AEs and exclude Screen Failure
adae_teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(ACTARM != "Screen Failure") %>%
  mutate(ACTARM = factor(ACTARM))

# Safety population (non-Screen Failure subjects) — used as denominator
adsl_safety <- adsl %>%
  filter(ACTARM != "Screen Failure") %>%
  mutate(ACTARM = factor(ACTARM))

# --- Sort by descending frequency (unique subject count) ----------------------
soc_order <- adae_teae %>%
  distinct(USUBJID, AESOC) %>%
  count(AESOC) %>%
  arrange(desc(n)) %>%
  pull(AESOC)

pt_order <- adae_teae %>%
  distinct(USUBJID, AESOC, AEDECOD) %>%
  count(AESOC, AEDECOD) %>%
  arrange(AESOC, desc(n)) %>%
  pull(AEDECOD) %>%
  unique()

adae_teae <- adae_teae %>%
  mutate(
    AESOC = factor(AESOC, levels = soc_order),
    AEDECOD = factor(AEDECOD, levels = pt_order)
  )

# --- Build the hierarchical summary table -------------------------------------
ae_table <- tbl_hierarchical(
  data = adae_teae,
  variables = c(AESOC, AEDECOD),
  by = ACTARM,
  denominator = adsl_safety,
  id = USUBJID,
  overall_row = TRUE,
  label = list(
    AESOC ~ "Primary System Organ Class",
    AEDECOD ~ "Reported Term for the Adverse Event",
    ..ard_hierarchical_overall.. ~ "Treatment Emergent AEs"
  )
)

# --- Save as HTML -------------------------------------------------------------
ae_table %>%
  as_gt() %>%
  gt::gtsave(filename = "question_4_tlg/ae_summary_table.html")

cat("ae_summary_table.html saved successfully.\n")
