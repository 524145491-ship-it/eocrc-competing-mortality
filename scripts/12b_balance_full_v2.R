# ==============================================================================
# Script: 12b_balance_full_v2.R
# Purpose: 单独重跑全分期 IPTW 平衡表，补齐追踪回执
# Inputs:
#   - data/cleaned_cohort_full_v2.rds (成人全队列，用于 ID 衔接)
#   - data/cohort_full_weighted_v2.rds (含 ps 与 iptw_w 的加权队列)
# Outputs:
#   - results/tableS2b_iptw_balance_full_v2.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(cobalt)
})

source("scripts/mc_trace.R")

cat("Loading adult full cohort...\n")
df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

cat("Loading weighted full cohort...\n")
df <- readRDS("data/cohort_full_weighted_v2.rds")
df <- mc_checkpoint(df, "纳入：全分期 IPTW 加权队列", id = "Patient_ID")

stopifnot(nrow(df) == nrow(df_full), all(df$Patient_ID %in% df_full$Patient_ID))

covar_names <- c("Sex", "Race", "Year_Group", "Subsite", "Histology", "Grade",
                 "Stage", "Surgery", "Chemo", "Radiation", "LN_Examined")
formula_ps <- as.formula(EOCRC ~ Sex + Race + Year_Group + Subsite + Histology + Grade +
                           Stage + Surgery + Chemo + Radiation + LN_Examined)

ps_fit <- glm(formula_ps, data = df, family = binomial(link = "logit"))
bal_res <- bal.tab(
  x = ps_fit,
  weights = df$iptw_w,
  un = TRUE,
  method = "weighting",
  thresholds = c(m = 0.05)
)
bal_df <- as.data.frame(bal_res$Balance)
bal_df$Variable <- rownames(bal_df)
setDT(bal_df)
fwrite(bal_df, "results/tableS2b_iptw_balance_full_v2.csv")

cat("Max |SMD| unadjusted:", round(max(abs(bal_df$Diff.Un), na.rm = TRUE), 4),
    " adjusted:", round(max(abs(bal_df$Diff.Adj), na.rm = TRUE), 4), "\n")
cat("12b completed successfully.\n")
