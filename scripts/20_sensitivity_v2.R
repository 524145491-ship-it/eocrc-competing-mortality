# ==============================================================================
# Script: 20_sensitivity_v2.R
# Purpose: v2 S9 - 稳健性检验与亚组交互多重比较校正
# Inputs:
#   - data/cohort_iii_weighted_v2.rds
#   - data/cohort_full_weighted_v2.rds
#   - results/tableS3_subgroup_interactions_v2.csv
# Outputs:
#   - results/tableS4_sensitivity_v2.csv
#   - results/tableS3b_subgroup_fdr_v2.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
})

source("scripts/mc_trace.R")

set.seed(20261029)

df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

df_iii <- readRDS("data/cohort_iii_weighted_v2.rds")
df_iii <- mc_checkpoint(df_iii, "纳入：I-III 期 IPTW 加权队列", id = "Patient_ID")

df_full_w <- readRDS("data/cohort_full_weighted_v2.rds")
df_full_w <- mc_checkpoint(df_full_w, "纳入：全分期 IPTW 加权队列", id = "Patient_ID", side = TRUE)

fit_primary <- function(dt, wcol, label, cohort_label) {
  out <- list()
  for (ep in c("OS", "CSS")) {
    event_var <- if (ep == "OS") "OS_Status" else "CSS_Status"
    fml <- as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ EOCRC"))
    fit <- coxph(fml, data = dt, weights = dt[[wcol]], robust = TRUE)
    s <- summary(fit)$coefficients["EOCRC", ]
    out[[ep]] <- data.table(
      Sensitivity = label, Cohort = cohort_label, Outcome = ep,
      N = nrow(dt), Events = sum(dt[[event_var]] == 1, na.rm = TRUE),
      HR = exp(s["coef"]),
      Lower = exp(s["coef"] - 1.96 * s["robust se"]),
      Upper = exp(s["coef"] + 1.96 * s["robust se"]),
      P = s["Pr(>|z|)"]
    )
  }
  rbindlist(out)
}

cat("Running sensitivity analyses...\n")
sens <- rbind(
  fit_primary(df_iii, "iptw_w", "Primary (Stage I-III, IPTW)", "Stage I-III"),
  fit_primary(df_iii, "iptw_w_trimmed", "Trimmed weights (1%-99%)", "Stage I-III"),
  fit_primary(df_iii[Histology == "Classic adenocarcinoma"], "iptw_w",
              "Classic adenocarcinoma only", "Stage I-III"),
  fit_primary(df_iii[Year_DX <= 2019], "iptw_w", "Exclude 2020-2022", "Stage I-III"),
  fit_primary(df_full_w, "iptw_w", "Full stage (I-IV) sensitivity", "Stage I-IV")
)
sens[, HR_Str := sprintf("%.2f (%.2f-%.2f)", HR, Lower, Upper)]
sens[, P_Str := ifelse(P < 0.001, "<0.001", sprintf("%.3f", P))]
fwrite(sens, "results/tableS4_sensitivity_v2.csv")
print(sens[, .(Sensitivity, Outcome, N, Events, HR_Str, P_Str)])

# 亚组交互 FDR
sg <- fread("results/tableS3_subgroup_interactions_v2.csv")
sg[, P_interaction_FDR := p.adjust(P_interaction, method = "BH"), by = Outcome]
fwrite(sg[, .(Outcome, Subgroup, Level, P_interaction, P_interaction_FDR)],
       "results/tableS3b_subgroup_fdr_v2.csv")

cat("\nS9 v2 completed successfully.\n")
