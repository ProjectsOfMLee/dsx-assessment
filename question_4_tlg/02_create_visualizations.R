# ==============================================================================
# Question 4.2: AE Visualizations using {ggplot2}
#
# Plot 1: AE severity distribution by treatment (stacked bar chart)
# Plot 2: Top 10 most frequent AEs with 95% Clopper-Pearson CIs
#
# Output: ae_severity_by_treatment.png, ae_top10_forest_plot.png
# ==============================================================================

library(ggplot2)
library(pharmaverseadam)
library(dplyr)

# --- Load data ----------------------------------------------------------------
adae <- pharmaverseadam::adae

# Filter to TEAEs, exclude Screen Failure
adae_teae <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(ACTARM != "Screen Failure")

# ==============================================================================
# Plot 1: AE severity distribution by treatment (stacked bar chart)
# ==============================================================================

# Order severity levels for stacking (SEVERE at bottom, MILD at top)
adae_teae <- adae_teae %>%
  mutate(AESEV = factor(AESEV, levels = c("SEVERE", "MODERATE", "MILD")))

p1 <- ggplot(adae_teae, aes(x = ACTARM, fill = AESEV)) +
  geom_bar() +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  scale_fill_manual(
    values = c("MILD" = "#F8766D", "MODERATE" = "#00BA38", "SEVERE" = "#619CFF"),
    breaks = c("MILD", "MODERATE", "SEVERE")
  ) +
  theme_grey(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 11)
  )

ggsave("question_4_tlg/ae_severity_by_treatment.png", p1,
       width = 10, height = 7, dpi = 150)
cat("ae_severity_by_treatment.png saved.\n")

# ==============================================================================
# Plot 2: Top 10 most frequent AEs with 95% Clopper-Pearson CIs
# ==============================================================================

# Total number of unique subjects in ADAE (denominator for incidence)
n_subjects <- n_distinct(adae$USUBJID)

# Count unique subjects per AEDECOD
ae_counts <- adae_teae %>%
  distinct(USUBJID, AEDECOD) %>%
  count(AEDECOD, name = "n_subj") %>%
  arrange(desc(n_subj)) %>%
  head(10)

# Calculate incidence rate and 95% Clopper-Pearson CI
ae_counts <- ae_counts %>%
  mutate(
    pct = 100 * n_subj / n_subjects,
    # Clopper-Pearson exact binomial CI
    ci_lower = 100 * qbeta(0.025, n_subj, n_subjects - n_subj + 1),
    ci_upper = 100 * qbeta(0.975, n_subj + 1, n_subjects - n_subj)
  ) %>%
  # Order by descending frequency for the plot
  mutate(AEDECOD = factor(AEDECOD, levels = rev(AEDECOD)))

p2 <- ggplot(ae_counts, aes(x = pct, y = AEDECOD)) +
  geom_point(size = 3) +
  geom_errorbar(aes(xmin = ci_lower, xmax = ci_upper), width = 0.3,
                orientation = "y") +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_subjects, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  theme_grey(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12)
  )

ggsave("question_4_tlg/ae_top10_forest_plot.png", p2,
       width = 10, height = 7, dpi = 150)
cat("ae_top10_forest_plot.png saved.\n")
