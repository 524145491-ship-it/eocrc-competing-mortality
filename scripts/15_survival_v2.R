# ==============================================================================
# Script: 15_survival_v2.R
# Purpose: v2 S4 - I-III 期主分析集的生存动态（Cox、IPTW、时间分段、连续年龄）
# Inputs:
#   - data/cohort_iii_weighted_v2.rds
#   - data/cleaned_cohort_full_v2.rds (用于 ID 衔接)
# Outputs:
#   - results/table3_survival_models_v2.csv
#   - results/table3b_age_rcs_survival_v2.csv
#   - results/fig3c_timevarying_v2.csv
#   - results/fig3d_age_rcs_survival_v2.csv
#   - results/tableS2b_schoenfeld_v2.csv
# Methods:
#   - 未加权多变量 Cox；IPTW 加权边际 Cox；IPTW + 协变量双重调整
#   - 时间分段 Cox（0-12, 12-36, 36-60, 60-120, >120 月）
#   - 连续年龄 splines::ns(df=4)（等价 5 节点限制性立方样条）
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
  library(splines)
})

source("scripts/mc_trace.R")

set.seed(20261029)

cat("Loading adult full cohort for ID chain...\n")
df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

cat("Loading Stage I-III weighted cohort...\n")
df <- readRDS("data/cohort_iii_weighted_v2.rds")
df <- mc_checkpoint(df, "纳入：I-III 期 IPTW 加权队列", id = "Patient_ID")
stopifnot(all(df$Patient_ID %in% df_full$Patient_ID))

covars <- c("Sex", "Race", "Year_Group", "Subsite", "Histology", "Grade", "Stage",
            "Surgery", "Chemo", "Radiation", "LN_Examined")
covar_formula <- paste(covars, collapse = " + ")

# ------------------------------------------------------------------------------
# 1. 主 Cox 模型：OS 与 CSS
# ------------------------------------------------------------------------------
cat("Fitting Cox models...\n")
fit_os_unadj <- coxph(as.formula(paste("Surv(Survival_Months, OS_Status) ~ EOCRC +", covar_formula)), data = df)
fit_os_w     <- coxph(Surv(Survival_Months, OS_Status) ~ EOCRC, data = df, weights = iptw_w, robust = TRUE)
fit_os_dr    <- coxph(as.formula(paste("Surv(Survival_Months, OS_Status) ~ EOCRC +", covar_formula)),
                      data = df, weights = iptw_w, robust = TRUE)

fit_css_unadj <- coxph(as.formula(paste("Surv(Survival_Months, CSS_Status) ~ EOCRC +", covar_formula)), data = df)
fit_css_w     <- coxph(Surv(Survival_Months, CSS_Status) ~ EOCRC, data = df, weights = iptw_w, robust = TRUE)
fit_css_dr    <- coxph(as.formula(paste("Surv(Survival_Months, CSS_Status) ~ EOCRC +", covar_formula)),
                       data = df, weights = iptw_w, robust = TRUE)

extract_hr <- function(fit, term = "EOCRC") {
  s <- summary(fit)$coefficients
  est <- s[term, "coef"]
  se <- if ("robust se" %in% colnames(s)) s[term, "robust se"] else s[term, "se(coef)"]
  data.table(HR = exp(est), Lower = exp(est - 1.96 * se), Upper = exp(est + 1.96 * se),
             P = s[term, "Pr(>|z|)"])
}

main_res <- rbind(
  data.table(Model = "OS: multivariable Cox (unweighted)", Endpoint = "OS", extract_hr(fit_os_unadj)),
  data.table(Model = "OS: IPTW-weighted Cox", Endpoint = "OS", extract_hr(fit_os_w)),
  data.table(Model = "OS: IPTW + covariate double-adjusted", Endpoint = "OS", extract_hr(fit_os_dr)),
  data.table(Model = "CSS: multivariable Cox (unweighted)", Endpoint = "CSS", extract_hr(fit_css_unadj)),
  data.table(Model = "CSS: IPTW-weighted Cox", Endpoint = "CSS", extract_hr(fit_css_w)),
  data.table(Model = "CSS: IPTW + covariate double-adjusted", Endpoint = "CSS", extract_hr(fit_css_dr))
)
main_res[, HR_Str := sprintf("%.2f (%.2f-%.2f)", HR, Lower, Upper)]
main_res[, P_Str := ifelse(P < 0.001, "<0.001", sprintf("%.3f", P))]
print(main_res)

# ------------------------------------------------------------------------------
# 2. 比例风险检验
# ------------------------------------------------------------------------------
zph_os <- cox.zph(fit_os_w)
zph_css <- cox.zph(fit_css_w)
schoenfeld <- rbind(
  data.table(Endpoint = "OS", Term = rownames(zph_os$table), Chisq = zph_os$table[, "chisq"],
             df = zph_os$table[, "df"], P = zph_os$table[, "p"]),
  data.table(Endpoint = "CSS", Term = rownames(zph_css$table), Chisq = zph_css$table[, "chisq"],
             df = zph_css$table[, "df"], P = zph_css$table[, "p"])
)
fwrite(schoenfeld, "results/tableS2b_schoenfeld_v2.csv")
cat("Schoenfeld EOCRC P: OS =", zph_os$table["EOCRC", "p"], " CSS =", zph_css$table["EOCRC", "p"], "\n")

# ------------------------------------------------------------------------------
# 3. 时间分段 Cox（动态 HR）
# ------------------------------------------------------------------------------
cat("Fitting time-segmented Cox models...\n")
fit_split_one <- function(event_var, label) {
  fml <- as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ ."))
  dsplit <- survSplit(fml, data = df, cut = c(12, 36, 60, 120), episode = "tgroup")
  fit <- coxph(as.formula(paste0("Surv(tstart, Survival_Months, ", event_var,
                                 ") ~ EOCRC:strata(tgroup) + cluster(Patient_ID)")),
               data = dsplit, weights = iptw_w)
  s <- summary(fit)$coefficients
  intervals <- c("0-12 months", "12-36 months", "36-60 months", "60-120 months", ">120 months")
  dt <- data.table(
    Endpoint = label,
    Interval = intervals,
    Mid_Time = c(6, 24, 48, 90, 150),
    HR = exp(s[, "coef"]),
    Lower = exp(s[, "coef"] - 1.96 * s[, "robust se"]),
    Upper = exp(s[, "coef"] + 1.96 * s[, "robust se"]),
    P = s[, "Pr(>|z|)"]
  )
  list(fit = fit, dt = dt)
}

split_css <- fit_split_one("CSS_Status", "CSS")
split_os  <- fit_split_one("OS_Status", "OS")
time_dt <- rbind(split_os$dt, split_css$dt)
time_dt[, HR_Str := sprintf("%.2f (%.2f-%.2f)", HR, Lower, Upper)]
time_dt[, P_Str := ifelse(P < 0.001, "<0.001", sprintf("%.3f", P))]
fwrite(time_dt, "results/fig3c_timevarying_v2.csv")
print(time_dt[, .(Endpoint, Interval, HR_Str, P_Str)])

# 合并主表与动态 HR
table3 <- rbind(
  main_res[, .(Model, Endpoint, HR, Lower, Upper, P_Str = P_Str, HR_Str = HR_Str)],
  time_dt[, .(Model = paste0(Endpoint, " dynamic HR: ", Interval), Endpoint = Endpoint,
              HR, Lower, Upper, P_Str = P_Str, HR_Str = HR_Str)]
)
fwrite(table3, "results/table3_survival_models_v2.csv")

# ------------------------------------------------------------------------------
# 4. 连续年龄 spline Cox（OS 与 CSS）
# ------------------------------------------------------------------------------
cat("Fitting continuous-age spline Cox models...\n")
ref_cov <- data.frame(lapply(df[, ..covars], function(x) {
  if (is.factor(x)) factor(levels(x)[1], levels = levels(x)) else median(x, na.rm = TRUE)
}))
names(ref_cov) <- covars

ages <- seq(18, 90, by = 1)
ref_age <- 50
sel_ages <- c(30, 40, 50, 60, 70, 80)

fit_age_spline <- function(event_var, label) {
  fml_rcs <- as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ ns(Age_Num, df = 4) + ", covar_formula))
  fml_lin <- as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ Age_Num + ", covar_formula))
  fml_no  <- as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ ", covar_formula))
  fit_rcs <- coxph(fml_rcs, data = df, weights = iptw_w, robust = TRUE, x = TRUE)
  fit_lin <- coxph(fml_lin, data = df, weights = iptw_w, robust = TRUE, x = TRUE)
  fit_no  <- coxph(fml_no,  data = df, weights = iptw_w, robust = TRUE, x = TRUE)

  # coxph 稳健方差不支持 anova 嵌套检验；用联合 Wald 检验
  beta_all <- coef(fit_rcs)
  V_all <- vcov(fit_rcs)
  idx_age <- grep("^ns\\(Age_Num", names(beta_all))
  wald_test <- function(ix) {
    ix <- ix[!is.na(beta_all[ix])]
    if (length(ix) == 0) return(NA_real_)
    W <- as.numeric(t(beta_all[ix]) %*% solve(V_all[ix, ix, drop = FALSE]) %*% beta_all[ix])
    pchisq(W, df = length(ix), lower.tail = FALSE)
  }
  p_overall <- wald_test(idx_age)
  p_nonlin <- if (length(idx_age) > 1) wald_test(idx_age[-1]) else NA_real_

  newd <- cbind(data.frame(Age_Num = ages), ref_cov[rep(1, length(ages)), , drop = FALSE])
  refd <- cbind(data.frame(Age_Num = ref_age), ref_cov)
  tt <- delete.response(terms(fit_rcs))
  X <- model.matrix(tt, data = newd)
  Xref <- model.matrix(tt, data = refd)
  beta <- coef(fit_rcs)
  common <- intersect(colnames(X), names(beta))
  common <- common[!is.na(beta[common])]
  L <- X[, common, drop = FALSE] -
    matrix(rep(Xref[, common, drop = FALSE], each = nrow(X)), nrow = nrow(X))
  V <- vcov(fit_rcs)[common, common, drop = FALSE]
  loghr <- as.vector(L %*% beta[common])
  se <- sqrt(rowSums((L %*% V) * L))

  list(
    fit = fit_rcs, p_overall = p_overall, p_nonlin = p_nonlin,
    curve = data.table(Endpoint = label, Age = ages, HR = exp(loghr),
                       Lower = exp(loghr - 1.96 * se), Upper = exp(loghr + 1.96 * se))
  )
}

age_os <- fit_age_spline("OS_Status", "OS")
age_css <- fit_age_spline("CSS_Status", "CSS")
age_curves <- rbind(age_os$curve, age_css$curve)
fwrite(age_curves, "results/fig3d_age_rcs_survival_v2.csv")

age_tests <- rbind(
  data.table(Endpoint = "OS", P_overall = age_os$p_overall, P_nonlinear = age_os$p_nonlin),
  data.table(Endpoint = "CSS", P_overall = age_css$p_overall, P_nonlinear = age_css$p_nonlin)
)
for (i in seq_len(nrow(age_tests))) {
  ep <- age_tests$Endpoint[i]
  od <- age_curves[Endpoint == ep]
  idx <- match(sel_ages, od$Age)
  age_tests[i, `:=`(
    HR_30 = od$HR[idx[1]], HR_30_L = od$Lower[idx[1]], HR_30_U = od$Upper[idx[1]],
    HR_40 = od$HR[idx[2]], HR_40_L = od$Lower[idx[2]], HR_40_U = od$Upper[idx[2]],
    HR_50_ref = 1,
    HR_60 = od$HR[idx[4]], HR_60_L = od$Lower[idx[4]], HR_60_U = od$Upper[idx[4]],
    HR_70 = od$HR[idx[5]], HR_70_L = od$Lower[idx[5]], HR_70_U = od$Upper[idx[5]],
    HR_80 = od$HR[idx[6]], HR_80_L = od$Lower[idx[6]], HR_80_U = od$Upper[idx[6]]
  )]
}
fwrite(age_tests, "results/table3b_age_rcs_survival_v2.csv")
print(age_tests, digits = 3)

cat("\nS4 v2 completed successfully.\n")
