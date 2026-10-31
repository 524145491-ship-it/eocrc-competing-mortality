# ==============================================================================
# Script: 12_iptw_balance_v2.R
# Purpose: v2 深化方案 S2 - 分人群重估倾向评分与稳定化 IPTW
# Inputs:
#   - data/cleaned_cohort_full_v2.rds
#   - data/cohort_iii_v2.rds
# Outputs:
#   - data/cohort_iii_weighted_v2.rds
#   - data/cohort_full_weighted_v2.rds
#   - results/table1_baseline_v2.csv          (I-III 主分析集)
#   - results/table1b_baseline_full_v2.csv    (全分期敏感性集)
#   - results/tableS2_iptw_balance_v2.csv     (I-III)
#   - results/tableS2b_iptw_balance_full_v2.csv
#   - manuscript/supplementary/figS1_ps_density_v2.png
# Notes:
#   - 主分析集为 I-III 期；全分期仅作敏感性。
#   - 每个目标人群内单独估计倾向评分，不沿用旧权重。
#   - 稳定化权重 + 1%-99% 截断权重同时保留。
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(cobalt)
  library(patchwork)
})

source("scripts/mc_trace.R")
source("scripts/mc_figure.R")

set.seed(20261029)

# ------------------------------------------------------------------------------
# 0. 协变量与工具函数
# ------------------------------------------------------------------------------
covar_names <- c("Sex", "Race", "Year_Group", "Subsite", "Histology", "Grade",
                 "Stage", "Surgery", "Chemo", "Radiation", "LN_Examined")

prep_covariates <- function(dt) {
  dt <- copy(dt)
  dt[, Year_Group := fcase(
    Year_DX <= 2005, "2000-2005",
    Year_DX <= 2010, "2006-2010",
    Year_DX <= 2015, "2011-2015",
    default = "2016-2022"
  )]
  dt[, Year_Group := factor(Year_Group, levels = c("2000-2005", "2006-2010", "2011-2015", "2016-2022"))]
  dt[, Stage := droplevels(factor(Stage))]
  dt
}

fit_iptw <- function(dt, label, bal_file) {
  dt <- prep_covariates(dt)

  formula_ps <- as.formula(EOCRC ~ Sex + Race + Year_Group + Subsite + Histology + Grade +
                             Stage + Surgery + Chemo + Radiation + LN_Examined)
  stopifnot(identical(all.vars(formula_ps)[-1], covar_names))

  cat("\n============================================================\n")
  cat("Fitting IPTW for:", label, " N =", nrow(dt), "\n")
  cat("Formula:", deparse(formula_ps), "\n")

  # 检查每个因子水平是否同时有 EOCRC=0/1，避免完全分离
  for (v in covar_names) {
    tb <- table(dt[[v]], dt$EOCRC)
    zero <- which(tb[, 1] == 0 | tb[, 2] == 0)
    if (length(zero) > 0) {
      cat("WARNING: covariate", v, "has levels with zero cells:",
          paste(rownames(tb)[zero], collapse = ", "), "\n")
    }
  }

  ps_fit <- glm(formula_ps, data = dt, family = binomial(link = "logit"))
  dt[, ps := predict(ps_fit, type = "response")]

  p_eocrc <- mean(dt$EOCRC == 1)
  dt[, iptw_w := ifelse(EOCRC == 1, p_eocrc / ps, (1 - p_eocrc) / (1 - ps))]

  w_bounds <- quantile(dt$iptw_w, probs = c(0.01, 0.99), na.rm = TRUE)
  dt[, iptw_w_trimmed := pmin(pmax(iptw_w, w_bounds[1]), w_bounds[2])]

  bal_res <- bal.tab(
    x = ps_fit,
    weights = dt$iptw_w,
    un = TRUE,
    method = "weighting",
    thresholds = c(m = 0.05)
  )
  bal_df <- as.data.frame(bal_res$Balance)
  bal_df$Variable <- rownames(bal_df)
  setDT(bal_df)
  fwrite(bal_df, bal_file)

  cat("PS summary:\n"); print(summary(dt$ps))
  cat("IPTW summary:\n"); print(summary(dt$iptw_w))
  cat("Trimmed IPTW summary:\n"); print(summary(dt$iptw_w_trimmed))
  cat("Max absolute SMD after weighting:",
      round(max(abs(bal_df$Diff.Adj), na.rm = TRUE), 4), "\n")

  list(dt = dt, bal = bal_df, ps_fit = ps_fit)
}

calc_var_stats <- function(dt, varname) {
  vals <- levels(dt[[varname]])
  if (is.null(vals)) vals <- unique(dt[[varname]])
  res <- lapply(vals, function(v) {
    n_eo <- sum(dt$EOCRC == 1 & dt[[varname]] == v, na.rm = TRUE)
    pct_eo <- n_eo / sum(dt$EOCRC == 1) * 100
    n_lo <- sum(dt$EOCRC == 0 & dt[[varname]] == v, na.rm = TRUE)
    pct_lo <- n_lo / sum(dt$EOCRC == 0) * 100
    w_eo <- sum(dt$iptw_w[dt$EOCRC == 1 & dt[[varname]] == v], na.rm = TRUE)
    pct_w_eo <- w_eo / sum(dt$iptw_w[dt$EOCRC == 1], na.rm = TRUE) * 100
    w_lo <- sum(dt$iptw_w[dt$EOCRC == 0 & dt[[varname]] == v], na.rm = TRUE)
    pct_w_lo <- w_lo / sum(dt$iptw_w[dt$EOCRC == 0], na.rm = TRUE) * 100
    data.frame(
      Variable = varname,
      Category = as.character(v),
      Unweighted_EOCRC = sprintf("%d (%.1f%%)", n_eo, pct_eo),
      Unweighted_LOCRC = sprintf("%d (%.1f%%)", n_lo, pct_lo),
      Weighted_EOCRC = sprintf("%.1f%%", pct_w_eo),
      Weighted_LOCRC = sprintf("%.1f%%", pct_w_lo)
    )
  })
  bind_rows(res)
}

make_table1 <- function(dt, file) {
  tab1_rows <- lapply(covar_names, function(v) calc_var_stats(dt, v))
  tab1 <- bind_rows(tab1_rows)
  fwrite(tab1, file)
  tab1
}

# ------------------------------------------------------------------------------
# 1. I-III 期主分析集
# ------------------------------------------------------------------------------
cat("Loading adult full cohort and Stage I-III subset...\n")
df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

df_iii <- df_full[Stage_Group == "Stage I-III"]
df_iii <- mc_checkpoint(df_iii, "纳入：I-III 期主分析集", id = "Patient_ID")

res_iii <- fit_iptw(df_iii, "Stage I-III primary set", "results/tableS2_iptw_balance_v2.csv")
df_iii_w <- res_iii$dt
df_iii_w <- mc_checkpoint(df_iii_w, "纳入：I-III 期主分析集 IPTW 权重完成", id = "Patient_ID")
saveRDS(df_iii_w, "data/cohort_iii_weighted_v2.rds")
make_table1(df_iii_w, "results/table1_baseline_v2.csv")

# ------------------------------------------------------------------------------
# 2. 全分期敏感性集
# ------------------------------------------------------------------------------
cat("\nUsing full adult cohort for sensitivity analysis...\n")
df_full <- mc_checkpoint(df_full, "纳入：全分期敏感性集", id = "Patient_ID")

res_full <- fit_iptw(df_full, "Full adult cohort (all stages)", "results/tableS2b_iptw_balance_full_v2.csv")
df_full_w <- res_full$dt
df_full_w <- mc_checkpoint(df_full_w, "纳入：全分期敏感性集 IPTW 权重完成", id = "Patient_ID")
saveRDS(df_full_w, "data/cohort_full_weighted_v2.rds")
make_table1(df_full_w, "results/table1b_baseline_full_v2.csv")

# ------------------------------------------------------------------------------
# 3. 倾向评分重叠密度图（附图）
# ------------------------------------------------------------------------------
# 加权前 / 加权后两个面板，绘图代码在 38_figS1_v2.R（同一张图只由一个脚本落盘）
source("scripts/38_figS1_v2.R")

cat("\nS2 v2 completed successfully.\n")
cat("Stage I-III N:", nrow(df_iii_w), " EOCRC:", sum(df_iii_w$EOCRC == 1), " LOCRC:", sum(df_iii_w$EOCRC == 0), "\n")
cat("Full N:", nrow(df_full_w), " EOCRC:", sum(df_full_w$EOCRC == 1), " LOCRC:", sum(df_full_w$EOCRC == 0), "\n")
