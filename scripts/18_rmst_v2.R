# ==============================================================================
# Script: 18_rmst_v2.R
# Purpose: v2 S7 - 限制性平均生存时间（RMST）与绝对生存差
# Inputs:
#   - data/cohort_iii_weighted_v2.rds
#   - data/cleaned_cohort_full_v2.rds (ID 衔接)
# Outputs:
#   - results/tableS5_rmst_v2.csv
#   - results/fig3d_rmst_v2.csv
# Methods:
#   - IPTW 加权 RMST: survival::survfit + summary(rmean=tau)
#   - 未加权协变量调整 RMST: survRM2::rmst2（敏感性）
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
  library(survRM2)
})

source("scripts/mc_trace.R")

set.seed(20261029)

df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

df <- readRDS("data/cohort_iii_weighted_v2.rds")
df <- mc_checkpoint(df, "纳入：I-III 期 IPTW 加权队列", id = "Patient_ID")
stopifnot(all(df$Patient_ID %in% df_full$Patient_ID))

covars <- c("Sex", "Race", "Year_Group", "Subsite", "Histology", "Grade", "Stage",
            "Surgery", "Chemo", "Radiation", "LN_Examined")

# ------------------------------------------------------------------------------
# 1. 加权 RMST（主分析）
# ------------------------------------------------------------------------------
extract_rmst <- function(event_var, endpoint, tau) {
  fml <- as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ Age_Group"))
  fit <- survfit(fml, data = df, weights = iptw_w)
  tb <- summary(fit, rmean = tau)$table
  rn_eo <- grep("Early-Onset", rownames(tb), value = TRUE)
  rn_lo <- grep("Late-Onset", rownames(tb), value = TRUE)
  eo <- tb[rn_eo, ]; lo <- tb[rn_lo, ]
  diff <- eo["rmean"] - lo["rmean"]
  se_diff <- sqrt(eo["se(rmean)"]^2 + lo["se(rmean)"]^2)
  data.table(
    Endpoint = endpoint, Tau_Months = tau,
    EOCRC_RMST = eo["rmean"], EOCRC_SE = eo["se(rmean)"],
    LOCRC_RMST = lo["rmean"], LOCRC_SE = lo["se(rmean)"],
    Difference = diff, Diff_Lower = diff - 1.96 * se_diff, Diff_Upper = diff + 1.96 * se_diff,
    P = 2 * pnorm(-abs(diff / se_diff))
  )
}

rmst_rows <- list()
for (tau in c(60, 120)) {
  rmst_rows[[paste("OS", tau)]] <- extract_rmst("OS_Status", "OS", tau)
  rmst_rows[[paste("CSS", tau)]] <- extract_rmst("CSS_Status", "CSS", tau)
}
rmst_dt <- rbindlist(rmst_rows)
fwrite(rmst_dt, "results/tableS5_rmst_v2.csv")
fwrite(rmst_dt, "results/fig3d_rmst_v2.csv")
print(rmst_dt, digits = 3)

# ------------------------------------------------------------------------------
# 2. 未加权协变量调整 RMST（敏感性）
# ------------------------------------------------------------------------------
cat("\nFitting covariate-adjusted RMST (unweighted sensitivity)...\n")
cov_matrix <- model.matrix(
  as.formula(paste("~", paste(covars, collapse = " + "))),
  data = df
)[, -1, drop = FALSE]

adj_rows <- list()
for (tau in c(60, 120)) {
  for (ep in c("OS", "CSS")) {
    status <- if (ep == "OS") df$OS_Status else df$CSS_Status
    fit <- rmst2(time = df$Survival_Months, status = status, arm = df$EOCRC,
                 tau = tau, covariates = cov_matrix)
    ar <- fit$adjusted.result
    # 行名通常为 "RMST (arm=1)-(arm=0)"
    rn <- grep("RMST", rownames(ar), value = TRUE)[1]
    adj_rows[[paste(ep, tau)]] <- data.table(
      Endpoint = ep, Tau_Months = tau,
      Adjusted_Difference = ar[rn, "Est."],
      Adjusted_Lower = ar[rn, "lower .95"],
      Adjusted_Upper = ar[rn, "upper .95"],
      Adjusted_P = ar[rn, "p"]
    )
  }
}
adj_dt <- rbindlist(adj_rows)
fwrite(adj_dt, "results/tableS5b_rmst_adjusted_v2.csv")
print(adj_dt, digits = 3)

cat("\nS7 v2 completed successfully.\n")
