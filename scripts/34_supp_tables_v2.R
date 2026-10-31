# ==============================================================================
# Supplementary tables v2
# Inputs (all results CSVs):
#   tableS1_attrition_v2, tableS2_iptw_balance_v2, tableS2b_iptw_balance_full_v2,
#   tableS3_subgroup_interactions_v2, tableS3b_subgroup_fdr_v2,
#   tableS4_sensitivity_v2, tableS5_rmst_v2, tableS5b_rmst_adjusted_v2
# Outputs: manuscript/supplementary/tableS1..S7.docx
# Note   : table numbers follow the order of first citation in manuscript/main.md
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(flextable)
  library(officer)
})

make_docx <- function(df, caption, footnote, path, widths) {
  ft <- flextable(df)
  ft <- theme_booktabs(ft)
  ft <- font(ft, fontname = "Times New Roman", part = "all")
  ft <- fontsize(ft, size = 8.0, part = "body")
  ft <- fontsize(ft, size = 8.0, part = "header")
  ft <- bold(ft, part = "header")
  ft <- align(ft, j = 2:ncol(df), align = "center", part = "all")
  ft <- align(ft, j = 1, align = "left", part = "all")
  for (i in seq_along(widths)) ft <- width(ft, j = i, width = widths[i])
  ft <- set_caption(ft, caption)
  ft <- add_footer_lines(ft, footnote)
  ft <- fontsize(ft, size = 7.5, part = "footer")
  save_as_docx(ft, path = path)
  cat("saved:", path, "\n")
}

# S1 attrition
att <- fread("results/tableS1_attrition_v2.csv")
s1 <- data.frame(
  Step = att$Step,
  Records = format(att$Records, big.mark = ","),
  Patients = format(att$Patients, big.mark = ","),
  `Excluded records` = format(att$Excluded_Records, big.mark = ","),
  `Excluded patients` = format(att$Excluded_Patients, big.mark = ","),
  `Retention (%)` = sprintf("%.2f", att$Retention_Pct),
  check.names = FALSE
)
make_docx(
  s1,
  "Table S1. Cohort selection and attrition cascade.",
  "Records and patients are counted after each sequential exclusion. Retention is relative to the initial raw cohort.",
  "manuscript/supplementary/tableS1_attrition.docx",
  c(2.4, 0.95, 0.95, 1.05, 1.05, 0.85)
)

# S3 balance (Stage I-III)
bal <- fread("results/tableS2_iptw_balance_v2.csv")
bal <- bal[!Variable %in% c("distance", "prop.score")]
s2 <- data.frame(
  Covariate = bal$Variable,
  `SMD before IPTW` = sprintf("%.4f", bal$Diff.Un),
  `SMD after IPTW` = sprintf("%.4f", bal$Diff.Adj),
  check.names = FALSE
)
make_docx(
  s2,
  "Table S3. Covariate balance before and after IPTW in the Stage I-III cohort.",
  "SMD, standardized mean difference. All absolute SMDs after IPTW were below 0.05.",
  "manuscript/supplementary/tableS3_iptw_balance.docx",
  c(3.0, 1.2, 1.2)
)

# S6 balance (full stage)
balb <- fread("results/tableS2b_iptw_balance_full_v2.csv")
balb <- balb[!Variable %in% c("distance", "prop.score")]
s2b <- data.frame(
  Covariate = balb$Variable,
  `SMD before IPTW` = sprintf("%.4f", balb$Diff.Un),
  `SMD after IPTW` = sprintf("%.4f", balb$Diff.Adj),
  check.names = FALSE
)
make_docx(
  s2b,
  "Table S6. Covariate balance before and after IPTW in the full-stage sensitivity cohort.",
  "SMD, standardized mean difference. All absolute SMDs after IPTW were below 0.05.",
  "manuscript/supplementary/tableS6_iptw_balance_full.docx",
  c(3.0, 1.2, 1.2)
)

# S4 subgroup interactions
sg <- fread("results/tableS3_subgroup_interactions_v2.csv")
sgf <- fread("results/tableS3b_subgroup_fdr_v2.csv")
sg <- merge(sg, sgf[, .(Outcome, Subgroup, Level, P_interaction_FDR)],
            by = c("Outcome", "Subgroup", "Level"), all.x = TRUE)
s3 <- data.frame(
  Outcome = sg$Outcome,
  Subgroup = sg$Subgroup,
  Level = sg$Level,
  N = format(sg$N, big.mark = ","),
  Events = format(sg$Events, big.mark = ","),
  `HR (95% CI)` = sg$HR_Str,
  P = sg$P_Str,
  `P interaction` = sg$P_int_Str,
  `P interaction FDR` = ifelse(is.na(sg$P_interaction_FDR), "-", ifelse(sg$P_interaction_FDR < 0.001, "<0.001", sprintf("%.3f", sg$P_interaction_FDR))),
  check.names = FALSE
)
make_docx(
  s3,
  "Table S4. Subgroup analyses and interaction tests for overall and cancer-specific survival.",
  "HRs are within-subgroup IPTW-weighted estimates; interaction P values are from weighted Cox models with the interaction term and covariates; FDR is Benjamini-Hochberg.",
  "manuscript/supplementary/tableS4_subgroup_interactions.docx",
  c(0.55, 1.25, 0.85, 0.75, 0.65, 1.05, 0.55, 0.65, 0.75)
)

# S5 sensitivity
sens <- fread("results/tableS4_sensitivity_v2.csv")
s4 <- data.frame(
  Sensitivity = sens$Sensitivity,
  Outcome = sens$Outcome,
  N = format(sens$N, big.mark = ","),
  Events = format(sens$Events, big.mark = ","),
  `HR (95% CI)` = sens$HR_Str,
  P = sens$P_Str,
  check.names = FALSE
)
make_docx(
  s4,
  "Table S5. Sensitivity analyses for overall and cancer-specific survival.",
  "All models are IPTW-weighted Cox models; the full-stage cohort includes Stage IV. HR, hazard ratio.",
  "manuscript/supplementary/tableS5_sensitivity.docx",
  c(2.4, 0.7, 0.8, 0.75, 1.2, 0.55)
)

# S7 RMST
rmst <- fread("results/tableS5_rmst_v2.csv")
rmsta <- fread("results/tableS5b_rmst_adjusted_v2.csv")
s5 <- data.frame(
  Endpoint = c(rmst$Endpoint, rmsta$Endpoint),
  Time = c(paste0(rmst$Tau_Months / 12, "-year"), paste0(rmsta$Tau_Months / 12, "-year")),
  Method = c(rep("IPTW-weighted", nrow(rmst)), rep("Covariate-adjusted (unweighted)", nrow(rmsta))),
  `Early-onset RMST` = c(sprintf("%.2f", rmst$EOCRC_RMST), rep("-", nrow(rmsta))),
  `Late-onset RMST` = c(sprintf("%.2f", rmst$LOCRC_RMST), rep("-", nrow(rmsta))),
  `Difference (95% CI)` = c(
    sprintf("%.2f (%.2f to %.2f)", rmst$Difference, rmst$Diff_Lower, rmst$Diff_Upper),
    sprintf("%.2f (%.2f to %.2f)", rmsta$Adjusted_Difference, rmsta$Adjusted_Lower, rmsta$Adjusted_Upper)
  ),
  P = c(ifelse(rmst$P < 0.001, "<0.001", sprintf("%.3f", rmst$P)),
        ifelse(rmsta$Adjusted_P < 0.001, "<0.001", sprintf("%.3f", rmsta$Adjusted_P))),
  check.names = FALSE
)
make_docx(
  s5,
  "Table S7. Restricted mean survival time differences at 5 and 10 years.",
  "RMST differences are early- minus late-onset in months; positive values favor early-onset. IPTW, inverse probability of treatment weighting.",
  "manuscript/supplementary/tableS7_rmst.docx",
  c(0.6, 0.7, 1.6, 1.0, 1.0, 1.6, 0.55)
)
