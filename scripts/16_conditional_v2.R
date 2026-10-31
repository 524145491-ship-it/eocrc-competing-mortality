# ==============================================================================
# Script: 16_conditional_v2.R
# Purpose: v2 S5 - I-III 期主分析集的 5 年动态条件生存
# Inputs:
#   - data/cohort_iii_weighted_v2.rds
#   - data/cleaned_cohort_full_v2.rds (ID 衔接)
# Outputs:
#   - results/table_conditional_survival_v2.csv
#   - results/table_conditional_survival_stage_v2.csv
# Methods:
#   - survival::survfit，IPTW 加权，地标 0–5 年
#   - 5 年条件 OS = S(x+5)/S(x)；5 年条件 CSS 同理
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
})

source("scripts/mc_trace.R")

set.seed(20261029)

df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

df <- readRDS("data/cohort_iii_weighted_v2.rds")
df <- mc_checkpoint(df, "纳入：I-III 期 IPTW 加权队列", id = "Patient_ID")
stopifnot(all(df$Patient_ID %in% df_full$Patient_ID))

calc_cs_trajectory <- function(sub_dt, time_var, status_var, x_years = 0:5, y_years = 5) {
  s_fit <- survfit(Surv(get(time_var), get(status_var)) ~ 1, data = sub_dt, weights = iptw_w)
  res <- vapply(x_years, function(x) {
    t_x <- x * 12
    t_xy <- (x + y_years) * 12
    s_x <- summary(s_fit, times = t_x)$surv
    if (t_x == 0) s_x <- 1.0
    s_xy <- summary(s_fit, times = t_xy)$surv
    if (length(s_xy) == 0 || is.na(s_xy)) s_xy <- 0
    if (length(s_x) == 0 || is.na(s_x) || s_x == 0) return(NA_real_)
    pmin(pmax(s_xy / s_x, 0), 1)
  }, numeric(1))
  data.frame(Survived_Years = x_years, CS_Rate = res)
}

# 风险集检查
risk_counts <- function(sub_dt, time_var) {
  s_fit <- survfit(Surv(get(time_var), rep(1, nrow(sub_dt))) ~ 1, data = sub_dt, weights = iptw_w)
  data.frame(Time = s_fit$time, N_At_Risk = s_fit$n.risk)
}

cat("Calculating overall conditional survival...\n")
cs_eo_os  <- calc_cs_trajectory(df[EOCRC == 1], "Survival_Months", "OS_Status")
cs_lo_os  <- calc_cs_trajectory(df[EOCRC == 0], "Survival_Months", "OS_Status")
cs_eo_css <- calc_cs_trajectory(df[EOCRC == 1], "Survival_Months", "CSS_Status")
cs_lo_css <- calc_cs_trajectory(df[EOCRC == 0], "Survival_Months", "CSS_Status")

cs_eo_os$Cohort <- "Early-Onset (<50)"; cs_eo_os$Outcome <- "5-Year Overall Survival"
cs_lo_os$Cohort <- "Late-Onset (>=50)"; cs_lo_os$Outcome <- "5-Year Overall Survival"
cs_eo_css$Cohort <- "Early-Onset (<50)"; cs_eo_css$Outcome <- "5-Year Cancer-Specific Survival"
cs_lo_css$Cohort <- "Late-Onset (>=50)"; cs_lo_css$Outcome <- "5-Year Cancer-Specific Survival"

cs_overall <- rbindlist(list(cs_eo_os, cs_lo_os, cs_eo_css, cs_lo_css))
fwrite(cs_overall, "results/table_conditional_survival_v2.csv")
print(cs_overall)

cat("\nCalculating stage-stratified conditional OS...\n")
stages <- c("Stage I", "Stage II", "Stage III")
matrix_rows <- list()
for (stg in stages) {
  for (eo in c(1, 0)) {
    grp <- ifelse(eo == 1, "EOCRC", "LOCRC")
    sub <- df[Stage == stg & EOCRC == eo]
    if (nrow(sub) > 50) {
      res <- as.data.table(calc_cs_trajectory(sub, "Survival_Months", "OS_Status"))
      res[, `:=`(Stage = stg, Cohort = grp, Row_Label = paste(stg, grp))]
      matrix_rows[[paste(stg, grp)]] <- res
    }
  }
}
stage_dt <- rbindlist(matrix_rows)
fwrite(stage_dt, "results/table_conditional_survival_stage_v2.csv")
print(stage_dt)

cat("\nS5 v2 completed successfully.\n")
