# ==============================================================================
# Script: 14_metastasis_rcs_v2.R
# Purpose: v2 S3 - 器官特异转移谱（年龄组模型 + 连续年龄 RCS 剂量反应）
# Inputs:
#   - data/cleaned_cohort_modern_v2.rds
# Outputs:
#   - results/table2_metastasis_logistic_v2.csv
#   - results/table2b_age_rcs_metastasis.csv
#   - results/fig2c_rcs_curves_v2.csv
# Methods:
#   - 年龄组模型: stats::glm(binomial)，调整性别、种族、年份、亚部位、组织学、分级、CEA
#   - 连续年龄: splines::ns(Age_Num, df=4)（等价 5 节点限制性立方样条）+ glm
#   - 非线性检验: RCS 模型 vs 线性年龄模型的似然比检验
#   - OR 曲线: 相对年龄 50 岁，模型矩阵精确计算线性预测差与 95% CI
# Deviation:
#   - 方案原写 rms::lrm + rms::rcs；lrm 在脑转移等稀有终点不收敛，改用 glm + ns。
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(splines)
})

source("scripts/mc_trace.R")

set.seed(20261029)

cat("Loading modern metastasis cohort v2...\n")
df <- readRDS("data/cleaned_cohort_modern_v2.rds")
df <- mc_checkpoint(df, "纳入：现代转移分析子集 (2010–2022, 成人)", id = "Patient_ID")

# ------------------------------------------------------------------------------
# 1. 协变量准备
# ------------------------------------------------------------------------------
df[, Year_Group := fcase(
  Year_DX <= 2005, "2000-2005",
  Year_DX <= 2010, "2006-2010",
  Year_DX <= 2015, "2011-2015",
  default = "2016-2022"
)]
df[, Year_Group := factor(Year_Group, levels = c("2000-2005", "2006-2010", "2011-2015", "2016-2022"))]
df[, Stage := droplevels(factor(Stage))]
df[, CEA := factor(CEA, levels = c("Normal", "Elevated", "Unknown/Not documented"))]

covars <- c("Sex", "Race", "Year_Group", "Subsite", "Histology", "Grade", "CEA")

endpoints <- list(
  list(var = "Any_Mets", label = "Any Distant Metastasis"),
  list(var = "Met_Liver", label = "Liver Metastasis (Overall)"),
  list(var = "Isolated_Liver", label = "Isolated Liver Metastasis"),
  list(var = "Met_Lung", label = "Lung Metastasis (Overall)"),
  list(var = "Isolated_Lung", label = "Isolated Lung Metastasis"),
  list(var = "Met_Bone", label = "Bone Metastasis"),
  list(var = "Met_Brain", label = "Brain Metastasis"),
  list(var = "Multiple_Mets", label = "Multiple Organ Metastases (>=2)")
)

# ------------------------------------------------------------------------------
# 2. 年龄组模型（EOCRC vs LOCRC）
# ------------------------------------------------------------------------------
cat("Fitting age-group logistic models...\n")
res_list <- list()
for (ep in endpoints) {
  v <- ep$var; lbl <- ep$label
  n_eo <- sum(df$EOCRC == 1 & df[[v]] == 1, na.rm = TRUE)
  pct_eo <- n_eo / sum(df$EOCRC == 1) * 100
  n_lo <- sum(df$EOCRC == 0 & df[[v]] == 1, na.rm = TRUE)
  pct_lo <- n_lo / sum(df$EOCRC == 0) * 100

  fml <- as.formula(paste0(v, " ~ EOCRC + ", paste(covars, collapse = " + ")))
  fit <- glm(fml, data = df, family = binomial(link = "logit"))
  co <- summary(fit)$coefficients
  est <- co["EOCRC", "Estimate"]; se <- co["EOCRC", "Std. Error"]; p <- co["EOCRC", "Pr(>|z|)"]

  crude_or <- (n_eo / (sum(df$EOCRC == 1) - n_eo)) / (n_lo / (sum(df$EOCRC == 0) - n_lo))
  crude_se <- sqrt(1/n_eo + 1/(sum(df$EOCRC == 1) - n_eo) + 1/n_lo + 1/(sum(df$EOCRC == 0) - n_lo))

  res_list[[v]] <- data.table(
    Endpoint = lbl,
    LOCRC_Events = sprintf("%d (%.2f%%)", n_lo, pct_lo),
    EOCRC_Events = sprintf("%d (%.2f%%)", n_eo, pct_eo),
    LOCRC_Rate = pct_lo,
    EOCRC_Rate = pct_eo,
    Crude_OR = crude_or,
    Crude_OR_Lower = exp(log(crude_or) - 1.96 * crude_se),
    Crude_OR_Upper = exp(log(crude_or) + 1.96 * crude_se),
    Adjusted_OR = exp(est),
    Adjusted_OR_Lower = exp(est - 1.96 * se),
    Adjusted_OR_Upper = exp(est + 1.96 * se),
    P_Value = p
  )
}
tab2 <- rbindlist(res_list)
tab2[, Crude_OR_Str := sprintf("%.2f (%.2f-%.2f)", Crude_OR, Crude_OR_Lower, Crude_OR_Upper)]
tab2[, Adjusted_OR_Str := sprintf("%.2f (%.2f-%.2f)", Adjusted_OR, Adjusted_OR_Lower, Adjusted_OR_Upper)]
tab2[, P_Str := ifelse(P_Value < 0.001, "<0.001", sprintf("%.3f", P_Value))]
fwrite(tab2, "results/table2_metastasis_logistic_v2.csv")
print(tab2[, .(Endpoint, EOCRC_Events, LOCRC_Events, Adjusted_OR_Str, P_Str)])

# ------------------------------------------------------------------------------
# 3. 连续年龄 RCS 模型（glm + ns，等价 5 节点限制性立方样条）
# ------------------------------------------------------------------------------
cat("\nFitting continuous-age spline models...\n")

# 参考协变量组合（因素取第一水平）
ref_cov <- data.frame(lapply(df[, ..covars], function(x) {
  if (is.factor(x)) factor(levels(x)[1], levels = levels(x)) else median(x, na.rm = TRUE)
}))
names(ref_cov) <- covars

ages <- seq(18, 90, by = 1)
ref_age <- 50
sel_ages <- c(30, 40, 50, 60, 70, 80)

rcs_or_list <- list()
rcs_test_list <- list()

fit_spline <- function(v) {
  fml_rcs <- as.formula(paste0(v, " ~ ns(Age_Num, df = 4) + ", paste(covars, collapse = " + ")))
  fml_lin <- as.formula(paste0(v, " ~ Age_Num + ", paste(covars, collapse = " + ")))
  fml_no  <- as.formula(paste0(v, " ~ ", paste(covars, collapse = " + ")))

  fit_rcs <- glm(fml_rcs, data = df, family = binomial(link = "logit"), maxit = 50)
  fit_lin <- glm(fml_lin, data = df, family = binomial(link = "logit"), maxit = 50)
  fit_no  <- glm(fml_no,  data = df, family = binomial(link = "logit"), maxit = 50)

  p_overall <- tryCatch({
    a <- as.data.frame(anova(fit_no, fit_rcs, test = "Chisq"))
    a[2, "Pr(>Chi)"]
  }, error = function(e) NA_real_)
  p_nonlin <- tryCatch({
    a <- as.data.frame(anova(fit_lin, fit_rcs, test = "Chisq"))
    a[2, "Pr(>Chi)"]
  }, error = function(e) NA_real_)

  # 相对年龄 50 的 OR：模型矩阵线性预测差（忽略响应项，按系数名对齐）
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
  logor <- as.vector(L %*% beta[common])
  se <- sqrt(rowSums((L %*% V) * L))

  list(
    fit = fit_rcs,
    p_overall = p_overall,
    p_nonlin = p_nonlin,
    or = data.table(Age = ages, OR = exp(logor),
                    Lower = exp(logor - 1.96 * se), Upper = exp(logor + 1.96 * se))
  )
}

for (ep in endpoints) {
  v <- ep$var; lbl <- ep$label
  fit_obj <- tryCatch(fit_spline(v), error = function(e) {
    cat("SPLINE FAILED for", lbl, ":", conditionMessage(e), "\n")
    NULL
  })
  if (is.null(fit_obj)) {
    rcs_test_list[[v]] <- data.table(Endpoint = lbl, P_overall = NA_real_, P_nonlinear = NA_real_,
                                     OR_30 = NA_real_, OR_40 = NA_real_, OR_50_ref = NA_real_,
                                     OR_60 = NA_real_, OR_70 = NA_real_, OR_80 = NA_real_)
    next
  }

  od <- copy(fit_obj$or); od[, Endpoint := lbl]
  rcs_or_list[[v]] <- od

  idx <- match(sel_ages, od$Age)
  rcs_test_list[[v]] <- data.table(
    Endpoint = lbl,
    P_overall = fit_obj$p_overall,
    P_nonlinear = fit_obj$p_nonlin,
    OR_30 = od$OR[idx[1]], OR_30_L = od$Lower[idx[1]], OR_30_U = od$Upper[idx[1]],
    OR_40 = od$OR[idx[2]], OR_40_L = od$Lower[idx[2]], OR_40_U = od$Upper[idx[2]],
    OR_50_ref = 1,
    OR_60 = od$OR[idx[4]], OR_60_L = od$Lower[idx[4]], OR_60_U = od$Upper[idx[4]],
    OR_70 = od$OR[idx[5]], OR_70_L = od$Lower[idx[5]], OR_70_U = od$Upper[idx[5]],
    OR_80 = od$OR[idx[6]], OR_80_L = od$Lower[idx[6]], OR_80_U = od$Upper[idx[6]]
  )
  cat(sprintf("%-45s P_overall=%.3g  P_nonlinear=%.3g\n", lbl, fit_obj$p_overall, fit_obj$p_nonlin))
}

rcs_curves <- rbindlist(rcs_or_list)
fwrite(rcs_curves, "results/fig2c_rcs_curves_v2.csv")

rcs_tests <- rbindlist(rcs_test_list, fill = TRUE)
rcs_tests[, P_overall_FDR := p.adjust(P_overall, method = "BH")]
rcs_tests[, P_nonlinear_FDR := p.adjust(P_nonlinear, method = "BH")]
fwrite(rcs_tests, "results/table2b_age_rcs_metastasis.csv")

cat("\nRCS tests:\n")
print(rcs_tests[, .(Endpoint, P_overall, P_nonlinear, P_nonlinear_FDR)])
cat("\nS3 v2 completed successfully.\n")
