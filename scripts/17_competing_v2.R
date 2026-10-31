# ==============================================================================
# Script: 17_competing_v2.R
# Purpose: v2 S6 - I-III 期竞争风险：加权 CIF、调整 csHR、调整 Fine-Gray sdHR、E-value
# Inputs:
#   - data/cohort_iii_weighted_v2.rds
#   - data/cleaned_cohort_full_v2.rds (ID 衔接)
# Outputs:
#   - results/table4_competing_risks_v2.csv
#   - results/table4b_cif_absolute_differences_v2.csv
#   - results/table4c_cause_composition_v2.csv
#   - results/table4d_evalues_v2.csv
#   - results/fig5_cif_curves_v2.csv
#   - results/fig5_finegray_forest_v2.csv
# Methods:
#   - 加权 CIF: survival::survfit 多状态模型
#   - 病因特异 Cox: coxph，IPTW + 协变量
#   - 调整 Fine-Gray: survival::finegray + coxph(weights=fgwt*iptw, cluster=Patient_ID)
#   - E-value: EValue::evalues.HR / evalues.OR
# Notes:
#   - Fine-Gray 在 5 万例分层随机子样本上拟合（扩大数据内存限制），主分析集为 I-III 期。
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
  library(EValue)
})

source("scripts/mc_trace.R")

set.seed(20261029)

df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

df <- readRDS("data/cohort_iii_weighted_v2.rds")
df <- mc_checkpoint(df, "纳入：I-III 期 IPTW 加权队列", id = "Patient_ID")
stopifnot(all(df$Patient_ID %in% df_full$Patient_ID))

df[, cr_f := factor(Comp_Risk_Status, levels = c(0, 1, 2), labels = c("censor", "ccsm", "ncsm"))]
covars <- c("Sex", "Race", "Year_Group", "Subsite", "Histology", "Grade", "Stage",
            "Surgery", "Chemo", "Radiation", "LN_Examined")
covar_formula <- paste(covars, collapse = " + ")

extract_cox <- function(fit, term = "EOCRC") {
  s <- summary(fit)$coefficients
  est <- s[term, "coef"]
  se <- if ("robust se" %in% colnames(s)) s[term, "robust se"] else s[term, "se(coef)"]
  data.table(HR = exp(est), Lower = exp(est - 1.96 * se), Upper = exp(est + 1.96 * se),
             P = s[term, "Pr(>|z|)"])
}

# ------------------------------------------------------------------------------
# 1. 加权 CIF（多状态 survfit）
# ------------------------------------------------------------------------------
cat("Fitting weighted cumulative incidence functions...\n")
km_ms <- survfit(Surv(Survival_Months, cr_f) ~ Age_Group, data = df, weights = iptw_w)
times <- seq(0, 180, by = 6)
sm <- summary(km_ms, times = times)

cif_dt <- data.table(
  Cohort = as.character(sm$strata),
  Time = sm$time,
  CCSM = sm$pstate[, "ccsm"],
  CCSM_SE = as.matrix(sm$std.err)[, which(colnames(sm$pstate) == "ccsm")],
  NCSM = sm$pstate[, "ncsm"],
  NCSM_SE = as.matrix(sm$std.err)[, which(colnames(sm$pstate) == "ncsm")]
)
cif_dt[, Cohort := gsub("Age_Group=", "", Cohort)]
fwrite(cif_dt, "results/fig5_cif_curves_v2.csv")

# 未加权 Gray 检验（描述性对照；加权 CIF 已用 survfit 多状态模型）
gray_fit <- cmprsk::cuminc(ftime = df$Survival_Months, fstatus = df$Comp_Risk_Status, group = df$Age_Group)
gray_dt <- data.table(
  Endpoint = c("CCSM", "NCSM"),
  Chisq = gray_fit$Tests[, "stat"],
  P = gray_fit$Tests[, "pv"]
)
fwrite(gray_dt, "results/table4e_gray_unweighted_v2.csv")

# 5 年与 10 年绝对差
abs_list <- list()
for (tt in c(60, 120)) {
  sub <- cif_dt[Time == tt]
  eo <- sub[Cohort == "Early-Onset (<50)"]
  lo <- sub[Cohort == "Late-Onset (>=50)"]
  for (cause in c("CCSM", "NCSM")) {
    se_col <- paste0(cause, "_SE")
    diff <- eo[[cause]] - lo[[cause]]
    se_diff <- sqrt(eo[[se_col]]^2 + lo[[se_col]]^2)
    z <- diff / se_diff
    abs_list[[paste(tt, cause)]] <- data.table(
      Time_Months = tt,
      Endpoint = cause,
      EOCRC = eo[[cause]], EOCRC_SE = eo[[se_col]],
      LOCRC = lo[[cause]], LOCRC_SE = lo[[se_col]],
      Difference = diff, Diff_Lower = diff - 1.96 * se_diff, Diff_Upper = diff + 1.96 * se_diff,
      P = 2 * pnorm(-abs(z))
    )
  }
}
abs_dt <- rbindlist(abs_list)
fwrite(abs_dt, "results/table4b_cif_absolute_differences_v2.csv")
print(abs_dt, digits = 3)

# ------------------------------------------------------------------------------
# 2. 病因特异 Cox（全 I-III 队列，IPTW + 协变量）
# ------------------------------------------------------------------------------
cat("\nFitting cause-specific Cox models...\n")
fit_cs_ccsm <- coxph(as.formula(paste("Surv(Survival_Months, Comp_Risk_Status == 1) ~ EOCRC +", covar_formula)),
                     data = df, weights = iptw_w, robust = TRUE)
fit_cs_ncsm <- coxph(as.formula(paste("Surv(Survival_Months, Comp_Risk_Status == 2) ~ EOCRC +", covar_formula)),
                     data = df, weights = iptw_w, robust = TRUE)
cs_ccsm <- extract_cox(fit_cs_ccsm); cs_ccsm[, `:=`(Endpoint = "CCSM", Model = "Cause-specific Cox")]
cs_ncsm <- extract_cox(fit_cs_ncsm); cs_ncsm[, `:=`(Endpoint = "NCSM", Model = "Cause-specific Cox")]

# ------------------------------------------------------------------------------
# 3. 调整 Fine-Gray（5 万例分层子样本）
# ------------------------------------------------------------------------------
cat("\nFitting adjusted Fine-Gray models on stratified subsample...\n")
n_fg <- min(50000, nrow(df))
df_fg <- df[, .SD[sample(.N, round(n_fg * .N / nrow(df)))], by = Age_Group]
df_fg[, iptw_copy := iptw_w]

fit_fg_one <- function(etype, label) {
  fg <- finegray(Surv(Survival_Months, cr_f) ~ ., data = df_fg, etype = etype)
  fit <- coxph(as.formula(paste("Surv(fgstart, fgstop, fgstatus) ~ EOCRC +", covar_formula)),
               data = fg, weights = fgwt * iptw_copy, cluster = Patient_ID, robust = TRUE)
  res <- extract_cox(fit)
  res[, `:=`(Endpoint = label, Model = "Fine-Gray subdistribution")]
  res
}
fg_ccsm <- fit_fg_one("ccsm", "CCSM")
fg_ncsm <- fit_fg_one("ncsm", "NCSM")
cat("Fine-Gray CCSM sdHR:", round(fg_ccsm$HR, 3), " (", round(fg_ccsm$Lower, 3), "-", round(fg_ccsm$Upper, 3), ")\n")
cat("Fine-Gray NCSM sdHR:", round(fg_ncsm$HR, 3), " (", round(fg_ncsm$Lower, 3), "-", round(fg_ncsm$Upper, 3), ")\n")

# ------------------------------------------------------------------------------
# 4. 合并主表 + E-value
# ------------------------------------------------------------------------------
model_dt <- rbind(cs_ccsm, cs_ncsm, fg_ccsm, fg_ncsm)
model_dt[, HR_Str := sprintf("%.2f (%.2f-%.2f)", HR, Lower, Upper)]
model_dt[, P_Str := ifelse(P < 0.001, "<0.001", sprintf("%.3f", P))]

evalue_rows <- list()
for (i in seq_len(nrow(model_dt))) {
  r <- model_dt[i]
  ev <- evalues.HR(est = r$HR, lo = r$Lower, hi = r$Upper, rare = FALSE)
  ev_ci <- if (!is.na(ev["E-values", "lower"])) ev["E-values", "lower"] else ev["E-values", "upper"]
  evalue_rows[[i]] <- data.table(
    Endpoint = r$Endpoint, Model = r$Model, HR = r$HR, Lower = r$Lower, Upper = r$Upper,
    Evalue_Point = ev["E-values", "point"], Evalue_CI = ev_ci
  )
}
ev_dt <- rbindlist(evalue_rows)
fwrite(ev_dt, "results/table4d_evalues_v2.csv")

# 转移 OR 的 E-value（来自 S3）
met <- fread("results/table2_metastasis_logistic_v2.csv")
met_ev <- rbindlist(lapply(c("Any Distant Metastasis", "Isolated Lung Metastasis"), function(ep) {
  r <- met[Endpoint == ep]
  ev <- evalues.OR(est = r$Adjusted_OR, lo = r$Adjusted_OR_Lower, hi = r$Adjusted_OR_Upper, rare = FALSE)
  ev_ci <- if (!is.na(ev["E-values", "lower"])) ev["E-values", "lower"] else ev["E-values", "upper"]
  data.table(Endpoint = ep, Model = "Logistic aOR", HR = r$Adjusted_OR,
             Lower = r$Adjusted_OR_Lower, Upper = r$Adjusted_OR_Upper,
             Evalue_Point = ev["E-values", "point"], Evalue_CI = ev_ci)
}))
fwrite(met_ev, "results/table4d_metastasis_evalues_v2.csv")

# 主表 Table 4
table4 <- rbind(
  model_dt[, .(Endpoint, Model, HR, Lower, Upper, P_Str, HR_Str)],
  abs_dt[, .(Endpoint = paste0(Endpoint, " CIF difference (", Time_Months, " mo)"),
             Model = "Absolute CIF difference", HR = Difference, Lower = Diff_Lower,
             Upper = Diff_Upper, P_Str = ifelse(P < 0.001, "<0.001", sprintf("%.3f", P)),
             HR_Str = sprintf("%.4f (%.4f-%.4f)", Difference, Diff_Lower, Diff_Upper))]
)
fwrite(table4, "results/table4_competing_risks_v2.csv")
fwrite(model_dt, "results/fig5_finegray_forest_v2.csv")
print(model_dt[, .(Endpoint, Model, HR_Str, P_Str)])

# ------------------------------------------------------------------------------
# 5. 死因构成随时间变化（加权比例）
# ------------------------------------------------------------------------------
cat("\nCalculating cause-of-death composition...\n")
dec <- df[Comp_Risk_Status %in% c(1, 2)]
dec[, Time_Bin := fcase(
  Survival_Months <= 12, "<=1y",
  Survival_Months <= 36, "1-3y",
  Survival_Months <= 60, "3-5y",
  Survival_Months <= 120, "5-10y",
  default = ">10y"
)]
dec[, Time_Bin := factor(Time_Bin, levels = c("<=1y", "1-3y", "3-5y", "5-10y", ">10y"))]
comp <- dec[, .(
  Total_Deaths = .N,
  CCSM_N = sum(Comp_Risk_Status == 1),
  NCSM_N = sum(Comp_Risk_Status == 2),
  CCSM_Prop = sum(iptw_w * (Comp_Risk_Status == 1)) / sum(iptw_w),
  NCSM_Prop = sum(iptw_w * (Comp_Risk_Status == 2)) / sum(iptw_w)
), by = .(Age_Group, Time_Bin)][order(Age_Group, Time_Bin)]
fwrite(comp, "results/table4c_cause_composition_v2.csv")
print(comp)

cat("\nS6 v2 completed successfully.\n")
