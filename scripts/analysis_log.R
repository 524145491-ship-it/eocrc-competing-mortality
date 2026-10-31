# MedCortex · 分析代码留痕（自动生成，勿手改）
# RunCode 每次成功执行后自动追加，这是本项目全流程分析代码的可复现记录。
# 定稿的干净脚本请另存为 scripts/ 下有序命名的文件（如 01_prepare.R、02_model.R）。

# [2026-10-23 17:05:59]
# 1. 清理 results/ 目录下所有文件
res_files <- list.files("results", full.names = TRUE, all.files = TRUE, no.. = TRUE)
if (length(res_files) > 0) unlink(res_files, recursive = TRUE)

# 2. 清理 figures/ 目录下所有文件
fig_files <- list.files("figures", full.names = TRUE, all.files = TRUE, no.. = TRUE)
if (length(fig_files) > 0) unlink(fig_files, recursive = TRUE)

# 3. 清理 data/ 目录下衍生文件（保留 data_exploration.md 原始数据底牌）
unlink(c("data/clean_cohort.rds", "data/weighted_cohort.rds"))

# 4. 清理 plan/ 目录下上一课题设计文件
plan_files <- list.files("plan", full.names = TRUE, all.files = TRUE, no.. = TRUE)
if (length(plan_files) > 0) unlink(plan_files, recursive = TRUE)

# 5. 清理 scripts/ 目录下上一课题具体脚本与日志（保留 mc_* 平台支持库）
old_scripts <- c(
  "scripts/01_cohort_prep.R",
  "scripts/02_iptw_balancing.R",
  "scripts/03_rcs_time_varying.R",
  "scripts/04_conditional_survival.R",
  "scripts/05_competing_risks.R",
  "scripts/06_subgroups_and_subsites.R",
  "scripts/07_sensitivity_evalues.R",
  "scripts/analysis_audit.md",
  "scripts/analysis_log.R",
  "scripts/analysis_log.py"
)
unlink(old_scripts)

# 6. 清理 analysis_trace/ 与 .medcortex/figure_qc
trace_files <- list.files("analysis_trace", full.names = TRUE, all.files = TRUE, no.. = TRUE)
if (length(trace_files) > 0) unlink(trace_files, recursive = TRUE)

qc_files <- list.files(".medcortex/figure_qc", full.names = TRUE, all.files = TRUE, no.. = TRUE)
if (length(qc_files) > 0) unlink(qc_files, recursive = TRUE)

cat("清理完成！当前各目录文件状态：\n")
cat("results/ 文件数:", length(list.files("results")), "\n")
cat("figures/ 文件数:", length(list.files("figures")), "\n")
cat("data/ 剩余文件:", paste(list.files("data"), collapse = ", "), "\n")
cat("plan/ 剩余文件:", paste(list.files("plan"), collapse = ", "), "\n")
cat("scripts/ 剩余文件:", paste(list.files("scripts"), collapse = ", "), "\n")
cat("analysis_trace/ 剩余文件:", length(list.files("analysis_trace")), "\n")

# [2026-10-23 17:14:32]
# 检查 R 环境和必备包
pkgs <- c("data.table", "tidyverse", "survival", "survminer", "cmprsk", "WeightIt", "cobalt", "flextable", "officer", "rms", "patchwork")
installed <- sapply(pkgs, requireNamespace, quietly = TRUE)
print(installed)

# 检查当前目录及 scripts/ 目录
list.files("scripts")

# [2026-10-23 17:14:57]
library(data.table)
# 快速读取一个文件的前几行查看列名
sample_dt <- fread("结直肠癌数据/SEER/升结肠.csv", nrows = 5)
print(dim(sample_dt))
cols <- names(sample_dt)
print(head(cols, 30))
# 查找重要字段名称
cat("\nID 相关字段:\n")
print(grep("ID|Patient|Recode|Number", cols, value = TRUE, ignore.case = TRUE))

cat("\n年龄相关字段:\n")
print(grep("Age", cols, value = TRUE, ignore.case = TRUE))

cat("\n生存与死因相关字段:\n")
print(grep("Survival|Month|Death|Cause|Vital", cols, value = TRUE, ignore.case = TRUE))

cat("\n转移相关字段:\n")
print(grep("Met|Liver|Lung|Bone|Brain", cols, value = TRUE, ignore.case = TRUE))

cat("\n分期与治疗相关字段:\n")
print(grep("Stage|AJCC|Surg|Chemo|Rad", cols, value = TRUE, ignore.case = TRUE))

# [2026-10-23 17:15:20]
library(data.table)

# 读取升结肠做结构探测
dt <- fread("结直肠癌数据/SEER/升结肠.csv", nrows = 1000)

cat("Age single ages sample:\n")
print(table(head(dt[["Age recode with single ages and 90+"]], 20)))

cat("\nYear of diagnosis range:\n")
print(range(dt[["Year of diagnosis"]]))

cat("\nSequence number table:\n")
print(table(dt[["Sequence number"]])[1:5])

cat("\nSurvival months class & sample:\n")
print(summary(as.numeric(dt[["Survival months"]])))

cat("\nCause of death classifications:\n")
print(table(dt[["SEER cause-specific death classification"]]))
print(table(dt[["SEER other cause of death classification"]]))

cat("\nMets fields sample values:\n")
print(table(dt[["SEER Combined Mets at DX-liver (2010+)"]]))
print(table(dt[["SEER Combined Mets at DX-lung (2010+)"]]))

cat("\nCEA sample values:\n")
print(table(dt[["CEA Pretreatment Interpretation Recode (2010+)"]]))

# [2026-10-23 17:15:27]
# 探测分期字段与跨年份整合
dt_years <- fread("结直肠癌数据/SEER/升结肠.csv", select = c(
  "Year of diagnosis",
  "Combined Summary Stage with Expanded Regional Codes (2004+)",
  "Summary stage 2000 (1998-2017)",
  "Derived AJCC Stage Group, 6th ed (2004-2015)",
  "Derived AJCC Stage Group, 7th ed (2010-2015)",
  "7th Edition Stage Group Recode (2016-2017)",
  "Derived EOD 2018 Stage Group Recode (2018+)"
), nrows = 20000)

cat("Years:\n")
print(range(dt_years$`Year of diagnosis`))

# 看各年份下各分期字段的非空率
table_years <- dt_years[, .(
  n = .N,
  ajcc6_nonblank = sum(`Derived AJCC Stage Group, 6th ed (2004-2015)` != "Blank(s)"),
  ajcc7_2010_nonblank = sum(`Derived AJCC Stage Group, 7th ed (2010-2015)` != "Blank(s)"),
  ajcc7_2016_nonblank = sum(`7th Edition Stage Group Recode (2016-2017)` != "Blank(s)"),
  eod2018_nonblank = sum(`Derived EOD 2018 Stage Group Recode (2018+)` != "Blank(s)"),
  summary_stage_nonblank = sum(`Combined Summary Stage with Expanded Regional Codes (2004+)` != "Blank(s)")
), by = `Year of diagnosis`][order(`Year of diagnosis`)]
print(table_years)

# [2026-10-23 17:15:36]
dt_surg <- fread("结直肠癌数据/SEER/升结肠.csv", select = c(
  "RX Summ--Surg Prim Site (1998+)",
  "Reason no cancer-directed surgery"
), nrows = 20000)

cat("Reason no cancer-directed surgery:\n")
print(table(dt_surg$`Reason no cancer-directed surgery`))

cat("\nRX Summ--Surg Prim Site (1998+) sample:\n")
print(table(head(dt_surg$`RX Summ--Surg Prim Site (1998+)`, 30)))

# [2026-10-23 17:18:38]
dirs <- c("data", "results", "figures", "figures/panels", "figures/submission", "manuscript/tables", "manuscript/supplementary")
for (d in dirs) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}
print(sapply(dirs, dir.exists))

# [2026-10-23 17:18:52]
library(data.table)
dt_test <- fread("结直肠癌数据/SEER/升结肠.csv", nrows = 100)
cols <- names(dt_test)
cat("Grade fields:\n")
print(grep("Grade", cols, value = TRUE))
cat("\nNodes fields:\n")
print(grep("nodes", cols, value = TRUE, ignore.case = TRUE))
cat("\nChemo / Radiation fields:\n")
print(grep("Chemo|Radiation", cols, value = TRUE, ignore.case = TRUE))

# [2026-10-23 17:19:03]
library(data.table)

files <- list.files("结直肠癌数据/SEER", pattern = "\\.csv$", full.names = TRUE)
print(files)

# 测试高效读取必要列
req_cols <- c(
  "Patient ID",
  "Age recode with single ages and 90+",
  "Year of diagnosis",
  "Sequence number",
  "Sex",
  "Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)",
  "Primary Site - labeled",
  "Histologic Type ICD-O-3",
  "Grade Recode (thru 2017)",
  "Derived Summary Grade 2018 (2018+)",
  "Reason no cancer-directed surgery",
  "Derived AJCC Stage Group, 6th ed (2004-2015)",
  "Derived AJCC Stage Group, 7th ed (2010-2015)",
  "7th Edition Stage Group Recode (2016-2017)",
  "Derived EOD 2018 Stage Group Recode (2018+)",
  "Combined Summary Stage with Expanded Regional Codes (2004+)",
  "Summary stage 2000 (1998-2017)",
  "Regional nodes examined (1988+)",
  "Chemotherapy recode (yes, no/unk)",
  "Radiation recode",
  "SEER Combined Mets at DX-liver (2010+)",
  "SEER Combined Mets at DX-lung (2010+)",
  "SEER Combined Mets at DX-bone (2010+)",
  "SEER Combined Mets at DX-brain (2010+)",
  "CEA Pretreatment Interpretation Recode (2010+)",
  "Survival months",
  "Vital status recode (study cutoff used)",
  "SEER cause-specific death classification",
  "SEER other cause of death classification"
)

t0 <- Sys.time()
# 快速读一个文件试一试列名是否全部吻合
dt_check <- fread(files[1], select = req_cols, nrows = 5)
print(dim(dt_check))
print(Sys.time() - t0)

# [2026-10-23 17:21:46]
library(data.table)
library(cobalt)

df <- readRDS("data/cleaned_cohort_full.rds")
cat("Cohort rows:", nrow(df), "\n")
cat("EOCRC cases:", sum(df$EOCRC == 1), "LOCRC cases:", sum(df$EOCRC == 0), "\n")

# 检查协变量分布
cat("Summary of EOCRC by Sex:\n")
print(table(df$Sex, df$Age_Group))

# [2026-10-23 17:24:54]
library(data.table)
df_m <- readRDS("data/cleaned_cohort_modern.rds")
cat("Modern cohort N:", nrow(df_m), "\n")
cat("EOCRC in modern:", sum(df_m$EOCRC == 1), "LOCRC:", sum(df_m$EOCRC == 0), "\n")

# 检查各转移发生率
mets_vars <- c("Met_Liver", "Met_Lung", "Met_Bone", "Met_Brain")
for (m in mets_vars) {
  cat(m, "EOCRC:", mean(df_m[EOCRC == 1][[m]]) * 100, "% | LOCRC:", mean(df_m[EOCRC == 0][[m]]) * 100, "%\n")
}
cat("Table of Met_Pattern:\n")
print(table(df_m$Met_Pattern, df_m$Age_Group))

# [2026-10-23 17:27:02]
library(data.table)
library(survival)

df <- readRDS("data/cohort_full_weighted.rds")
cat("Cohort rows:", nrow(df), "\n")

# 加权 Cox
fit_os <- coxph(Surv(Survival_Months, OS_Status) ~ EOCRC, data = df, weights = iptw_w, robust = TRUE)
print(summary(fit_os))

fit_css <- coxph(Surv(Survival_Months, CSS_Status) ~ EOCRC, data = df, weights = iptw_w, robust = TRUE)
print(summary(fit_css))

# cox.zph 检验
zph_os <- cox.zph(fit_os)
print(zph_os)

# [2026-10-23 17:27:36]
fit_split <- coxph(Surv(tstart, Survival_Months, OS_Status) ~ EOCRC:strata(tgroup) + cluster(Patient_ID), data = df_survsplit, weights = iptw_w)
print(summary(fit_split)$coefficients)

# [2026-10-23 17:27:52]
library(splines)
fit_spline <- coxph(Surv(Survival_Months, CSS_Status) ~ EOCRC + EOCRC:ns(Survival_Months, df = 3), data = df, weights = iptw_w, robust = TRUE)
print(summary(fit_spline)$coefficients)

# 预测 1 到 120 个月的时变 HR
t_seq <- seq(1, 120, by = 1)
# 根据系数计算线性预测值差值
coefs <- coef(fit_spline)
b_ns <- ns(t_seq, df = 3, knots = attr(ns(df$Survival_Months, df = 3), "knots"), Boundary.knots = attr(ns(df$Survival_Months, df = 3), "Boundary.knots"))
log_hr <- coefs["EOCRC"] + b_ns %*% coefs[grep("ns", names(coefs))]
hr_curve <- exp(log_hr)
cat("HR at 6m, 12m, 24m, 36m, 60m, 120m:\n")
print(data.frame(Month = c(6, 12, 24, 36, 60, 120), HR = hr_curve[c(6, 12, 24, 36, 60, 120)]))

# [2026-10-23 17:28:03]
fit_css_std <- coxph(Surv(Survival_Months, CSS_Status) ~ EOCRC, data = df, weights = iptw_w, robust = TRUE)
zph_css <- cox.zph(fit_css_std)
print(zph_css)

# 从 survival 内部的 smooth 提取拟合点
# plot(zph_css) 在底层调用了 loess 或 smooth.spline
y <- zph_css$y[, 1]
x <- zph_css$x
fit_smooth <- smooth.spline(x, y, df = 4)
pred_grid <- seq(1, 120, by = 1)
pred_smooth <- predict(fit_smooth, pred_grid)
hr_grid <- exp(pred_smooth$y)
cat("Sample HR(t) from zph smooth:\n")
print(data.frame(Month = c(6, 12, 24, 36, 60, 120), HR = hr_grid[c(6, 12, 24, 36, 60, 120)]))

# [2026-10-23 17:30:26]
# 在 R 运行环境中检查报错原因
source("scripts/mc_figure.R")
library(data.table)
library(survival)
library(ggplot2)

df <- readRDS("data/cohort_full_weighted.rds")
km_os <- survfit(Surv(Survival_Months, OS_Status) ~ Age_Group, data = df, weights = iptw_w)

get_surv_df <- function(km_fit) {
  d <- data.frame(
    time = km_fit$time,
    surv = km_fit$surv,
    strata = rep(names(km_fit$strata), km_fit$strata)
  )
  d$strata <- gsub("Age_Group=", "", d$strata)
  d
}
os_df <- get_surv_df(km_os)

p3a <- ggplot(os_df[os_df$time <= 120, ], aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 0.55) +
  scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_x_continuous(breaks = seq(0, 120, by = 24), limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Follow-up (Months)", y = "Overall Survival", title = "IPTW Weighted Overall Survival") +
  theme_mc(width_mm = 60) +
  theme(legend.position = "none")

# 测试 mc_label_ends
print("Testing mc_label_ends...")
p3a_test <- mc_label_ends(p3a, panel_mm = 55, size_pt = 6.5)
print("mc_label_ends passed.")

# [2026-10-23 17:30:49]
km_css <- survfit(Surv(Survival_Months, CSS_Status) ~ Age_Group, data = df, weights = iptw_w)
css_df <- get_surv_df(km_css)

p3b <- ggplot(css_df[css_df$time <= 120, ], aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 0.55) +
  scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_x_continuous(breaks = seq(0, 120, by = 24), limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Follow-up (Months)", y = "Cancer-Specific Survival", title = "IPTW Weighted Cancer-Specific Survival") +
  theme_mc(width_mm = 60) +
  theme(legend.position = "none")

p3b <- mc_label_ends(p3b, panel_mm = 55, size_pt = 6.5)

time_hr_dt <- data.frame(
  Interval = c("0-12m", "12-36m", "36-60m", "60-120m", ">120m"),
  Mid_Time = c(6, 24, 48, 90, 150),
  HR = c(0.85, 0.92, 0.76, 0.61, 0.45),
  HR_Lower = c(0.81, 0.88, 0.72, 0.57, 0.40),
  HR_Upper = c(0.89, 0.96, 0.80, 0.65, 0.50)
)

p3c <- ggplot(time_hr_dt, aes(x = Mid_Time, y = HR)) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "#D9534F", linewidth = 0.35) +
  geom_ribbon(aes(ymin = HR_Lower, ymax = HR_Upper), fill = "#384D7A", alpha = 0.15) +
  geom_line(color = "#384D7A", linewidth = 0.6) +
  geom_point(color = "#384D7A", size = 1.8, shape = 16) +
  geom_text(aes(label = sprintf("%.2f", HR)), vjust = -0.9, size = 2.0, color = "#1F2937") +
  scale_x_continuous(breaks = c(6, 24, 48, 90, 150), labels = c("0-12m", "1-3y", "3-5y", "5-10y", ">10y"), limits = c(0, 165)) +
  scale_y_continuous(limits = c(0.4, 1.3), breaks = seq(0.4, 1.2, by = 0.2)) +
  labs(x = "Survival Time Interval", y = "Adjusted Hazard Ratio (aHR)", title = "Time-Varying Hazard Ratio (CSS)") +
  annotate("text", x = 160, y = 1.03, label = "Reference (HR = 1.0)", size = 2.0, color = "#D9534F", hjust = 1) +
  theme_mc(width_mm = 65)

print("Testing mc_compose...")
fig3 <- mc_compose(p3a, p3b, p3c, ncol = 3, widths = c(1, 1, 1.15), width_mm = 180, height_mm = 80)
print("Testing mc_save...")
mc_save(fig3, "figures/fig3_survival_rcs.png", width_mm = 180, height_mm = 80)
print("Saved successfully!")

# [2026-10-23 17:33:51]
library(data.table)
library(survival)

df <- readRDS("data/cohort_full_weighted.rds")

# 计算条件生存函数
calc_cs <- function(sub_dt, time_var, status_var, x_years = 0:5, y_years = 5) {
  # x: 已存活年数 (月: x * 12)
  # y: 未来再存活年数 (月: y * 12)
  s_fit <- survfit(Surv(get(time_var), get(status_var)) ~ 1, data = sub_dt, weights = iptw_w)
  res <- sapply(x_years, function(x) {
    t_x <- x * 12
    t_xy <- (x + y_years) * 12
    
    # 获取 S(t_x) 和 S(t_xy)
    s_x <- summary(s_fit, times = t_x)$surv
    if (t_x == 0) s_x <- 1.0
    s_xy <- summary(s_fit, times = t_xy)$surv
    if (length(s_xy) == 0 || is.na(s_xy)) s_xy <- 0
    if (length(s_x) == 0 || is.na(s_x) || s_x == 0) return(NA)
    
    cs_val <- s_xy / s_x
    pmin(pmax(cs_val, 0), 1)
  })
  data.frame(Survived_Years = x_years, CS_5y = res)
}

cs_eo_os <- calc_cs(df[EOCRC == 1], "Survival_Months", "OS_Status")
cs_lo_os <- calc_cs(df[EOCRC == 0], "Survival_Months", "OS_Status")
print("EOCRC 5y-COS:")
print(cs_eo_os)
print("LOCRC 5y-COS:")
print(cs_lo_os)

# [2026-10-24 07:41:03]
library(data.table)
library(cmprsk)

df <- readRDS("data/cohort_full_weighted.rds")
cat("Comp_Risk_Status counts:\n")
print(table(df$Comp_Risk_Status, df$Age_Group))

# 测试 cuminc 计算 (大样本抽样或分层计算快速测试)
set.seed(42)
sample_idx <- sample(nrow(df), 50000)
c_fit <- cuminc(ftime = df$Survival_Months[sample_idx], fstatus = df$Comp_Risk_Status[sample_idx], group = df$Age_Group[sample_idx])
print(names(c_fit))

# [2026-10-24 07:41:11]
t0 <- Sys.time()
c_fit_full <- cuminc(ftime = df$Survival_Months, fstatus = df$Comp_Risk_Status, group = df$Age_Group)
print(Sys.time() - t0)
print(c_fit_full$Tests)

# [2026-10-24 07:41:27]
# 测试 crr 计算时间
t0 <- Sys.time()
# 采用代表性子样本或标准协变量拟合
sample_fg <- sample(nrow(df), 20000)
fg_fit_ccsm <- crr(ftime = df$Survival_Months[sample_fg], fstatus = df$Comp_Risk_Status[sample_fg], cov1 = df$EOCRC[sample_fg], failcode = 1, cencode = 0)
print(Sys.time() - t0)
print(summary(fg_fit_ccsm))

# [2026-10-24 07:45:39]
library(data.table)
library(survival)

df <- readRDS("data/cohort_full_weighted.rds")

# 检验 Subsite 交互
fit_site_int <- coxph(Surv(Survival_Months, CSS_Status) ~ EOCRC * Subsite, data = df, weights = iptw_w, robust = TRUE)
print(summary(fit_site_int)$coefficients)

# 检验 Stage 交互
fit_stage_int <- coxph(Surv(Survival_Months, CSS_Status) ~ EOCRC * Stage, data = df, weights = iptw_w, robust = TRUE)
print(summary(fit_stage_int)$coefficients)

# [2026-10-24 07:47:58]
print(table(df_stg4$Subsite))
print(class(df_stg4$Subsite))

# [2026-10-24 07:48:08]
ps_target <- "Right colon"
sub_test <- df_stg4[df_stg4$Subsite == ps_target, ]
cat("Rows in sub_test:", nrow(sub_test), "\n")
fit_s4 <- coxph(Surv(Survival_Months, CSS_Status) ~ EOCRC, data = sub_test, weights = iptw_w, robust = TRUE)
print(summary(fit_s4)$coefficients)

# [2026-10-24 07:50:47]
# E-value 标准计算函数 (VanderWeele & Ding 2017)
calc_evalue <- function(est, lower, upper) {
  # 如果 HR < 1，先取倒数
  rr <- ifelse(est < 1, 1 / est, est)
  rr_lim <- ifelse(est < 1, 1 / upper, lower)
  
  e_val <- rr + sqrt(rr * (rr - 1))
  e_val_ci <- ifelse(rr_lim > 1, rr_lim + sqrt(rr_lim * (rr_lim - 1)), 1.0)
  
  data.frame(
    Estimate = est,
    CI_Lower = lower,
    CI_Upper = upper,
    E_Value = round(e_val, 2),
    E_Value_CI = round(e_val_ci, 2)
  )
}

# 针对 OS 和 CSS 测试
print("E-value for OS HR = 0.46 (0.46-0.47):")
print(calc_evalue(0.463, 0.455, 0.472))

print("E-value for CSS HR = 0.73 (0.72-0.75):")
print(calc_evalue(0.734, 0.721, 0.748))

# [2026-10-24 07:50:58]
library(flextable)
library(officer)

# 测试 flextable 创建三线表样式
sample_tab <- data.frame(
  Variable = c("Age <50", "Age >=50"),
  N = c(60819, 405605),
  Percentage = c("13.0%", "87.0%")
)

ft <- flextable(sample_tab)
ft <- theme_booktabs(ft) # 标准三线表样式 (顶线、表头下线、底线)
ft <- autofit(ft)
save_as_docx(ft, path = "manuscript/tables/test.docx")
print(file.exists("manuscript/tables/test.docx"))
unlink("manuscript/tables/test.docx")

# [2026-10-24 07:54:33]
library(flextable)
library(officer)

s4 <- read.csv("results/tableS4_sensitivity_evalue.csv", stringsAsFactors = FALSE)
calc_evalue <- function(est, lower, upper) {
  rr <- ifelse(est < 1, 1 / est, est)
  rr_lim <- ifelse(est < 1, 1 / upper, lower)
  e_val <- rr + sqrt(rr * (rr - 1))
  e_val_ci <- ifelse(rr_lim > 1, rr_lim + sqrt(rr_lim * (rr_lim - 1)), 1.0)
  c(e_val = round(e_val, 2), e_val_ci = round(e_val_ci, 2))
}

for (i in 1:nrow(s4)) {
  ev <- calc_evalue(s4$HR[i], s4$HR_Lower[i], s4$HR_Upper[i])
  s4$E_Value_Point[i] <- ev["e_val"]
  s4$E_Value_CI_Limit[i] <- ev["e_val_ci"]
}

write.csv(s4, "results/tableS4_sensitivity_evalue.csv", row.names = FALSE)
print(s4)

# 重新生成 docx
ft <- flextable(s4)
ft <- theme_booktabs(ft)
ft <- font(ft, fontname = "Calibri", part = "all")
ft <- fontsize(ft, size = 9, part = "body")
ft <- fontsize(ft, size = 9.5, part = "header")
ft <- bold(ft, part = "header")
ft <- align(ft, align = "center", part = "body")
ft <- align(ft, j = 1, align = "left", part = "body")
ft <- set_caption(ft, caption = "Supplementary Table S4. Sensitivity analyses with weight trimming, histological restriction, and E-value sensitivity to unmeasured confounding.")
ft <- autofit(ft)
save_as_docx(ft, path = "manuscript/supplementary/tableS4_sensitivity_evalue.docx")
print("Updated table S4 docx!")

# [2026-10-24 08:42:36]
cat("R session test: OK\n")

# [2026-10-24 08:44:34]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(patchwork)
})

source("scripts/mc_figure.R")

bal_df <- as.data.table(read.csv("results/tableS2_iptw_balance.csv"))

# Panel a: CONSORT 纳排流程图 (彻底解决重叠、对齐与截断问题)
consort_boxes <- data.frame(
  x = c(3.6, 3.6, 3.6, 3.6, 3.6),
  y = c(10, 7.5, 5.0, 2.5, 0.2),
  label = c(
    "SEER CRC Raw Cohort (2000-2022)\nN = 654,149 records (631,157 patients)",
    "First Primary Malignancy Only\nN = 530,027",
    "Valid Follow-up Time (> 0 months)\nN = 504,397",
    "Confirmed Colorectal Adenocarcinoma\nN = 466,424",
    "Full Analysis Cohort (N = 466,424)\nEOCRC: 60,819 | LOCRC: 405,605"
  )
)

excl_boxes <- data.frame(
  x = c(8.8, 8.8, 8.8),
  y = c(8.75, 6.25, 3.75),
  label = c(
    "Excluded prior malignancies\n(n = 124,122)",
    "Excluded survival <= 0 months\n(n = 25,630)",
    "Excluded non-adenocarcinoma histology\n(n = 37,973)"
  )
)

p1a <- ggplot() +
  # 主流程框 (无重合，半宽2.25，右界5.85)
  geom_rect(data = consort_boxes, aes(xmin = x - 2.25, xmax = x + 2.25, ymin = y - 0.68, ymax = y + 0.68),
            fill = "#F8FAFC", color = "#384D7A", linewidth = 0.45) +
  geom_text(data = consort_boxes, aes(x = x, y = y, label = label), size = 2.45, lineheight = 0.95, color = "#1E293B", fontface = "plain") +
  # 排除框 (左界6.6，右界11.0，与主框保持0.75净空)
  geom_rect(data = excl_boxes, aes(xmin = x - 2.2, xmax = x + 2.2, ymin = y - 0.52, ymax = y + 0.52),
            fill = "#FEF2F2", color = "#C4615A", linewidth = 0.35) +
  geom_text(data = excl_boxes, aes(x = x, y = y, label = label), size = 2.2, lineheight = 0.9, color = "#991B1B") +
  # 垂直主干箭头
  geom_segment(aes(x = 3.6, xend = 3.6, y = 9.32, yend = 8.18), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 3.6, xend = 3.6, y = 6.82, yend = 5.68), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 3.6, xend = 3.6, y = 4.32, yend = 3.18), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 3.6, xend = 3.6, y = 1.82, yend = 0.88), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.35, color = "#384D7A") +
  # 水平分支箭头 (端点恰好停在排除框左边缘6.55前，绝不戳入框内)
  geom_segment(aes(x = 3.6, xend = 6.55, y = 8.75, yend = 8.75), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = "#C4615A") +
  geom_segment(aes(x = 3.6, xend = 6.55, y = 6.25, yend = 6.25), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = "#C4615A") +
  geom_segment(aes(x = 3.6, xend = 6.55, y = 3.75, yend = 3.75), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = "#C4615A") +
  scale_x_continuous(limits = c(0.8, 11.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.8, 11.2), expand = c(0, 0)) +
  labs(title = "Study Cohort Selection (CONSORT)") +
  theme_void() +
  theme(plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5, margin = margin(b = 3, unit = "mm")))

# Panel b: Love Plot (SMD Comparison)
love_data <- bal_df[!Variable %in% c("distance", "prop.score")]
# 精炼显示变量名，防止过长挤占图形空间
love_data[, Clean_Var := gsub("Year_Group", "Period: ", Variable)]
love_data[, Clean_Var := gsub("Subsite", "Site: ", Clean_Var)]
love_data[, Clean_Var := gsub("Histology", "Histology: ", Clean_Var)]
love_data[, Clean_Var := gsub("Grade", "Grade: ", Clean_Var)]
love_data[, Clean_Var := gsub("Stage", "Stage: ", Clean_Var)]
love_data[, Clean_Var := gsub("Surgery", "Surgery: ", Clean_Var)]
love_data[, Clean_Var := gsub("Chemo", "Chemo: ", Clean_Var)]
love_data[, Clean_Var := gsub("Radiation", "Radiation: ", Clean_Var)]
love_data[, Clean_Var := gsub("LN_Examined", "LN: ", Clean_Var)]
love_data[, Clean_Var := gsub("Race", "Race: ", Clean_Var)]
love_data[, Clean_Var := gsub("Sex", "Sex: ", Clean_Var)]

# 提取关键协变量展示
setorder(love_data, -Diff.Un)
plot_love_dt <- head(love_data, 15)

love_long <- melt(
  plot_love_dt,
  id.vars = "Clean_Var",
  measure.vars = c("Diff.Un", "Diff.Adj"),
  variable.name = "Cohort_Weight",
  value.name = "SMD"
)
love_long[, Cohort_Weight := factor(ifelse(Cohort_Weight == "Diff.Un", "Unadjusted", "IPTW-Adjusted"), levels = c("Unadjusted", "IPTW-Adjusted"))]
love_long[, Abs_SMD := abs(SMD)]

p1b <- ggplot(love_long, aes(x = Abs_SMD, y = reorder(Clean_Var, Abs_SMD), color = Cohort_Weight, shape = Cohort_Weight)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_vline(xintercept = 0.10, linetype = "dotted", color = "gray50", linewidth = 0.3) +
  geom_point(size = 2.1, stroke = 0.5) +
  scale_color_manual(values = c("Unadjusted" = "#C4615A", "IPTW-Adjusted" = "#384D7A"), name = "Sample") +
  scale_shape_manual(values = c("Unadjusted" = 16, "IPTW-Adjusted" = 17), name = "Sample") +
  scale_x_continuous(limits = c(0, max(love_long$Abs_SMD) * 1.15), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Absolute Standardized Mean Difference (SMD)", y = NULL, title = "Covariate Balance (Love Plot)") +
  annotate("text", x = 0.055, y = 14.5, label = "Threshold: SMD < 0.05", size = 2.2, color = "#D9534F", hjust = 0, fontface = "italic") +
  theme_mc(width_mm = 90) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 6.8, color = "black"),
    plot.margin = margin(2, 4, 2, 2, "mm")
  )

fig1 <- mc_compose(p1a, p1b, ncol = 2, widths = c(1, 1.1), width_mm = 180, height_mm = 110)
mc_save(fig1, "figures/fig1_consort_iptw.png", width_mm = 180, height_mm = 110)
cat("Figure 1 re-drawn and saved successfully.\n")

# [2026-10-24 08:46:09]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(patchwork)
})

source("scripts/mc_figure.R")

table2_df <- read.csv("results/table2_metastasis_logistic.csv")

plot_targets <- c("Liver Metastasis (Overall)", "Lung Metastasis (Overall)", "Bone Metastasis", "Brain Metastasis", "Multiple Organ Metastases (>=2)")
rate_dt <- table2_df[table2_df$Metastasis_Pattern %in% plot_targets, ]

# 统一精简标签，提升整图美观与排版协调性
site_label_map <- c(
  "Liver Metastasis (Overall)" = "Liver Metastasis",
  "Lung Metastasis (Overall)" = "Lung Metastasis",
  "Bone Metastasis" = "Bone Metastasis",
  "Brain Metastasis" = "Brain Metastasis",
  "Multiple Organ Metastases (>=2)" = "Multiple Organs (>=2)"
)

rate_long <- bind_rows(
  data.frame(
    Site = unname(site_label_map[rate_dt$Metastasis_Pattern]),
    Raw_Pattern = rate_dt$Metastasis_Pattern,
    Cohort = "Early-Onset (<50)",
    Rate = rate_dt$EOCRC_Rate
  ),
  data.frame(
    Site = unname(site_label_map[rate_dt$Metastasis_Pattern]),
    Raw_Pattern = rate_dt$Metastasis_Pattern,
    Cohort = "Late-Onset (>=50)",
    Rate = rate_dt$LOCRC_Rate
  )
)
plot_site_levels <- rev(unname(site_label_map[plot_targets]))
rate_long$Site <- factor(rate_long$Site, levels = plot_site_levels)
rate_long$Cohort <- factor(rate_long$Cohort, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))

# Panel a: 转移发生率对比条形图 (带呼吸感的间距与数值标注)
p2a <- ggplot(rate_long, aes(x = Rate, y = Site, fill = Cohort)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", Rate)),
            position = position_dodge(width = 0.75),
            hjust = -0.15, size = 2.2, color = "#1E293B", fontface = "plain") +
  scale_fill_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 20, by = 5), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Initial Metastatic Rate (%)", y = NULL, title = "Metastatic Organ Rates") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 7.2, face = "bold", color = "black"),
    plot.margin = margin(2, 3, 2, 2, "mm")
  )

# Panel b: 多变量 aOR 森林图 (专业右侧对齐列，杜绝浮动重叠)
forest_dt <- table2_df[table2_df$Metastasis_Pattern %in% plot_targets, ]
forest_dt$Site <- unname(site_label_map[forest_dt$Metastasis_Pattern])
forest_dt$Site <- factor(forest_dt$Site, levels = plot_site_levels)
forest_dt$Label_Str <- sprintf("%.2f (%.2f-%.2f)", forest_dt$Adjusted_OR, forest_dt$Adjusted_OR_Lower, forest_dt$Adjusted_OR_Upper)

p2b <- ggplot(forest_dt, aes(y = Site)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_errorbarh(aes(xmin = Adjusted_OR_Lower, xmax = Adjusted_OR_Upper),
                 height = 0.22, linewidth = 0.5, color = "#384D7A") +
  geom_point(aes(x = Adjusted_OR), size = 2.6, shape = 18, color = "#384D7A") +
  # 右侧规整数据列展示数值与置信区间，避免压线或叠字
  geom_text(aes(x = 1.95, label = Label_Str),
            hjust = 0, size = 2.2, color = "#1E293B") +
  annotate("text", x = 1.95, y = 5.45, label = "aOR (95% CI)",
           hjust = 0, size = 2.2, fontface = "bold", color = "#1E293B") +
  scale_x_continuous(
    limits = c(0.75, 2.5),
    breaks = seq(0.8, 1.8, by = 0.2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(x = "Adjusted Odds Ratio (aOR with 95% CI)", y = NULL, title = "Multivariable Odds of Distant Metastasis") +
  theme_mc(width_mm = 95) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = margin(2, 4, 2, 0, "mm")
  )

fig2 <- mc_compose(p2a, p2b, ncol = 2, widths = c(1.05, 1.15), width_mm = 180, height_mm = 95)
mc_save(fig2, "figures/fig2_metastasis_network.png", width_mm = 180, height_mm = 95)
cat("Figure 2 re-drawn and saved successfully.\n")

# [2026-10-24 08:47:04]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(patchwork)
})

source("scripts/mc_figure.R")

table2_df <- read.csv("results/table2_metastasis_logistic.csv")

plot_targets <- c("Liver Metastasis (Overall)", "Lung Metastasis (Overall)", "Bone Metastasis", "Brain Metastasis", "Multiple Organ Metastases (>=2)")
rate_dt <- table2_df[table2_df$Metastasis_Pattern %in% plot_targets, ]

site_label_map <- c(
  "Liver Metastasis (Overall)" = "Liver Metastasis",
  "Lung Metastasis (Overall)" = "Lung Metastasis",
  "Bone Metastasis" = "Bone Metastasis",
  "Brain Metastasis" = "Brain Metastasis",
  "Multiple Organ Metastases (>=2)" = "Multiple Organs (>=2)"
)

rate_long <- bind_rows(
  data.frame(
    Site = unname(site_label_map[rate_dt$Metastasis_Pattern]),
    Raw_Pattern = rate_dt$Metastasis_Pattern,
    Cohort = "Early-Onset (<50)",
    Rate = rate_dt$EOCRC_Rate
  ),
  data.frame(
    Site = unname(site_label_map[rate_dt$Metastasis_Pattern]),
    Raw_Pattern = rate_dt$Metastasis_Pattern,
    Cohort = "Late-Onset (>=50)",
    Rate = rate_dt$LOCRC_Rate
  )
)
plot_site_levels <- rev(unname(site_label_map[plot_targets]))
rate_long$Site <- factor(rate_long$Site, levels = plot_site_levels)
rate_long$Cohort <- factor(rate_long$Cohort, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))

p2a <- ggplot(rate_long, aes(x = Rate, y = Site, fill = Cohort)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(aes(label = sprintf("%.1f%%", Rate)),
            position = position_dodge(width = 0.75),
            hjust = -0.15, size = 2.5, color = "#1E293B", fontface = "plain") +
  scale_fill_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_x_continuous(limits = c(0, 24), breaks = seq(0, 20, by = 5), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Initial Metastatic Rate (%)", y = NULL, title = "Metastatic Organ Rates") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 7.5, face = "bold", color = "black"),
    plot.margin = margin(2, 3, 2, 2, "mm")
  )

forest_dt <- table2_df[table2_df$Metastasis_Pattern %in% plot_targets, ]
forest_dt$Site <- unname(site_label_map[forest_dt$Metastasis_Pattern])
forest_dt$Site <- factor(forest_dt$Site, levels = plot_site_levels)
forest_dt$Label_Str <- sprintf("%.2f (%.2f-%.2f)", forest_dt$Adjusted_OR, forest_dt$Adjusted_OR_Lower, forest_dt$Adjusted_OR_Upper)

p2b <- ggplot(forest_dt, aes(y = Site)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_errorbar(aes(xmin = Adjusted_OR_Lower, xmax = Adjusted_OR_Upper),
                width = 0.22, linewidth = 0.5, color = "#384D7A", orientation = "y") +
  geom_point(aes(x = Adjusted_OR), size = 2.8, shape = 18, color = "#384D7A") +
  geom_text(aes(x = 1.95, label = Label_Str),
            hjust = 0, size = 2.5, color = "#1E293B") +
  annotate("text", x = 1.95, y = 5.45, label = "aOR (95% CI)",
           hjust = 0, size = 2.5, fontface = "bold", color = "#1E293B") +
  scale_x_continuous(
    limits = c(0.75, 2.55),
    breaks = seq(0.8, 1.8, by = 0.2),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(x = "Adjusted Odds Ratio (aOR with 95% CI)", y = NULL, title = "Multivariable Odds of Distant Metastasis") +
  theme_mc(width_mm = 95) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = margin(2, 4, 2, 0, "mm")
  )

fig2 <- mc_compose(p2a, p2b, ncol = 2, widths = c(1.05, 1.15), width_mm = 180, height_mm = 95)
mc_save(fig2, "figures/fig2_metastasis_network.png", width_mm = 180, height_mm = 95)
cat("Figure 2 re-run complete.\n")

# [2026-10-24 08:48:05]
exists("km_os")

# [2026-10-24 08:48:40]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(survival)
  library(patchwork)
})

source("scripts/mc_figure.R")

cat("Reading weighted data for KM fit...\n")
df <- readRDS("data/cohort_full_weighted.rds")

km_os <- survfit(Surv(Survival_Months, OS_Status) ~ Age_Group, data = df, weights = iptw_w)
km_css <- survfit(Surv(Survival_Months, CSS_Status) ~ Age_Group, data = df, weights = iptw_w)

get_surv_df <- function(km_fit) {
  d <- data.frame(
    time = km_fit$time,
    surv = km_fit$surv,
    strata = rep(names(km_fit$strata), km_fit$strata)
  )
  d$strata <- gsub("Age_Group=", "", d$strata)
  d
}

os_df <- get_surv_df(km_os)
css_df <- get_surv_df(km_css)

t3 <- read.csv("results/table3_survival_models.csv")
dyn_rows <- t3[grep("CSS Dynamic HR", t3$Model), ]

time_hr_dt <- data.frame(
  Interval = c("0-12 months", "12-36 months", "36-60 months", "60-120 months", ">120 months"),
  Mid_Time = c(6, 24, 48, 90, 150),
  HR = dyn_rows$HR,
  HR_Lower = dyn_rows$HR_Lower,
  HR_Upper = dyn_rows$HR_Upper
)

p3a <- ggplot(os_df[os_df$time <= 120, ], aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 0.6) +
  scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_x_continuous(breaks = seq(0, 120, by = 24), limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Follow-up (Months)", y = "Overall Survival", title = "IPTW Weighted OS") +
  annotate("text", x = 6, y = 0.15, label = "Weighted HR = 0.46 (0.46-0.47)\nLog-rank P < 0.001", size = 2.4, hjust = 0, color = "#1E293B") +
  theme_mc(width_mm = 58) +
  theme(legend.position = "none", plot.margin = margin(2, 3, 2, 2, "mm"))

p3b <- ggplot(css_df[css_df$time <= 120, ], aes(x = time, y = surv, color = strata)) +
  geom_step(linewidth = 0.6) +
  scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_x_continuous(breaks = seq(0, 120, by = 24), limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, by = 0.2), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Follow-up (Months)", y = "Cancer-Specific Survival", title = "IPTW Weighted CSS") +
  annotate("text", x = 6, y = 0.15, label = "Weighted HR = 0.73 (0.72-0.75)\nLog-rank P < 0.001", size = 2.4, hjust = 0, color = "#1E293B") +
  theme_mc(width_mm = 58) +
  theme(
    legend.position = c(0.48, 0.38),
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.key.size = unit(3.5, "mm"),
    plot.margin = margin(2, 3, 2, 2, "mm")
  )

p3c <- ggplot(time_hr_dt, aes(x = Mid_Time, y = HR)) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_ribbon(aes(ymin = HR_Lower, ymax = HR_Upper), fill = "#384D7A", alpha = 0.18) +
  geom_line(color = "#384D7A", linewidth = 0.65) +
  geom_point(color = "#384D7A", size = 2.2, shape = 16) +
  geom_text(aes(label = sprintf("%.2f", HR)), vjust = -1.0, size = 2.5, color = "#1E293B", fontface = "bold") +
  scale_x_continuous(breaks = c(6, 24, 48, 90, 150), labels = c("0-1y", "1-3y", "3-5y", "5-10y", ">10y"), limits = c(0, 165)) +
  scale_y_continuous(limits = c(0.3, 1.25), breaks = seq(0.4, 1.2, by = 0.2)) +
  labs(x = "Follow-up Interval", y = "Adjusted HR (95% CI)", title = "Time-Varying HR (CSS)") +
  annotate("text", x = 6, y = 1.08, label = "Ref (HR = 1.0)", size = 2.5, color = "#D9534F", hjust = 0, fontface = "italic") +
  theme_mc(width_mm = 64) +
  theme(plot.margin = margin(2, 4, 2, 2, "mm"))

fig3 <- mc_compose(p3a, p3b, p3c, ncol = 3, widths = c(1, 1, 1.15), width_mm = 180, height_mm = 85)
mc_save(fig3, "figures/fig3_survival_rcs.png", width_mm = 180, height_mm = 85)
cat("Figure 3 re-drawn and saved successfully.\n")

# [2026-10-24 08:49:31]
time_hr_dt$X_Index <- 1:nrow(time_hr_dt)
time_hr_dt$Interval_Label <- c("0-1y", "1-3y", "3-5y", "5-10y", ">10y")

p3c <- ggplot(time_hr_dt, aes(x = X_Index, y = HR)) +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_ribbon(aes(ymin = HR_Lower, ymax = HR_Upper), fill = "#384D7A", alpha = 0.18) +
  geom_line(color = "#384D7A", linewidth = 0.65) +
  geom_point(color = "#384D7A", size = 2.4, shape = 16) +
  geom_text(aes(label = sprintf("%.2f", HR)), vjust = -1.1, size = 2.5, color = "#1E293B", fontface = "bold") +
  scale_x_continuous(breaks = 1:5, labels = time_hr_dt$Interval_Label, limits = c(0.7, 5.3)) +
  scale_y_continuous(limits = c(0.3, 1.25), breaks = seq(0.4, 1.2, by = 0.2)) +
  labs(x = "Follow-up Interval", y = "Adjusted HR (95% CI)", title = "Time-Varying HR (CSS)") +
  annotate("text", x = 0.8, y = 1.08, label = "Ref (HR = 1.0)", size = 2.5, color = "#D9534F", hjust = 0, fontface = "italic") +
  theme_mc(width_mm = 64) +
  theme(plot.margin = margin(2, 4, 2, 2, "mm"))

fig3 <- mc_compose(p3a, p3b, p3c, ncol = 3, widths = c(1, 1, 1.15), width_mm = 180, height_mm = 85)
mc_save(fig3, "figures/fig3_survival_rcs.png", width_mm = 180, height_mm = 85)
cat("Figure 3 optimized.\n")

# [2026-10-24 08:50:58]
exists("calc_cs_trajectory")

# [2026-10-24 08:51:37]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(survival)
  library(patchwork)
})

source("scripts/mc_figure.R")

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

cs_overall_dt <- as.data.table(read.csv("results/table_conditional_survival.csv"))

stages <- c("Stage I", "Stage II", "Stage III", "Stage IV")
matrix_rows <- list()

for (stg in stages) {
  for (eo in c(1, 0)) {
    grp_name <- ifelse(eo == 1, "EOCRC", "LOCRC")
    sub_data <- df[Stage == stg & EOCRC == eo]
    if (nrow(sub_data) > 50) {
      cs_res <- calc_cs_trajectory(sub_data, "Survival_Months", "OS_Status")
      cs_res$Stage <- stg
      cs_res$Cohort <- grp_name
      cs_res$Row_Label <- paste(stg, grp_name)
      matrix_rows[[paste(stg, grp_name)]] <- cs_res
    }
  }
}

heatmap_dt <- bind_rows(matrix_rows)
setDT(heatmap_dt)

# Panel a: 5-Year Conditional OS (5y-COS) (错开上下标注，彻底解决字叠字)
os_curve_dt <- cs_overall_dt[Outcome == "5-Year Overall Survival"]
os_curve_dt[, Cohort := factor(Cohort, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))]
os_curve_dt[, VJust := ifelse(Cohort == "Early-Onset (<50)", -0.9, 1.8)]

p4a <- ggplot(os_curve_dt, aes(x = Survived_Years, y = CS_Rate, color = Cohort, shape = Cohort)) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 2.2) +
  geom_text(aes(label = sprintf("%.1f%%", CS_Rate * 100), vjust = VJust), size = 2.4, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_shape_manual(values = c("Early-Onset (<50)" = 16, "Late-Onset (>=50)" = 17), name = "Cohort") +
  scale_x_continuous(breaks = 0:5, labels = paste0(0:5, "y"), limits = c(-0.25, 5.25)) +
  scale_y_continuous(limits = c(0.48, 1.0), breaks = seq(0.5, 1.0, by = 0.1), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Years Already Survived", y = "5-Year Conditional OS Rate", title = "5-Year Conditional OS") +
  theme_mc(width_mm = 56) +
  theme(legend.position = "none", plot.margin = margin(2, 2, 2, 2, "mm"))

# Panel b: 5-Year Conditional CSS (5y-CCSS)
css_curve_dt <- cs_overall_dt[Outcome == "5-Year Cancer-Specific Survival"]
css_curve_dt[, Cohort := factor(Cohort, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))]
css_curve_dt[, VJust := ifelse(Cohort == "Early-Onset (<50)", -0.9, 1.8)]

p4b <- ggplot(css_curve_dt, aes(x = Survived_Years, y = CS_Rate, color = Cohort, shape = Cohort)) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 2.2) +
  geom_text(aes(label = sprintf("%.1f%%", CS_Rate * 100), vjust = VJust), size = 2.4, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_shape_manual(values = c("Early-Onset (<50)" = 16, "Late-Onset (>=50)" = 17), name = "Cohort") +
  scale_x_continuous(breaks = 0:5, labels = paste0(0:5, "y"), limits = c(-0.25, 5.25)) +
  scale_y_continuous(limits = c(0.60, 1.02), breaks = seq(0.7, 1.0, by = 0.1), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Years Already Survived", y = "5-Year Conditional CSS Rate", title = "5-Year Conditional CSS") +
  theme_mc(width_mm = 56) +
  theme(
    legend.position = c(0.48, 0.22),
    legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.key.size = unit(3.5, "mm"),
    plot.margin = margin(2, 2, 2, 2, "mm")
  )

# Panel c: 分期条件生存热图
order_rows <- c(
  "Stage IV LOCRC", "Stage IV EOCRC",
  "Stage III LOCRC", "Stage III EOCRC",
  "Stage II LOCRC", "Stage II EOCRC",
  "Stage I LOCRC", "Stage I EOCRC"
)
heatmap_dt[, Row_Label := factor(Row_Label, levels = order_rows)]
heatmap_dt[, Survived_Col := factor(paste0("+", Survived_Years, " yr"), levels = paste0("+", 0:5, " yr"))]

p4c <- ggplot(heatmap_dt, aes(x = Survived_Col, y = Row_Label, fill = CS_Rate)) +
  geom_tile(color = "white", linewidth = 0.45) +
  geom_text(aes(label = sprintf("%.1f%%", CS_Rate * 100)), size = 2.3, color = ifelse(heatmap_dt$CS_Rate > 0.65, "white", "#1E293B"), fontface = "plain") +
  scale_fill_gradient2(low = "#C4615A", mid = "#E7D894", high = "#384D7A", midpoint = 0.55,
                       limits = c(0.1, 1.0), labels = scales::percent_format(accuracy = 1), name = "5y-COS") +
  labs(x = "Years Already Survived", y = NULL, title = "Stage-Specific 5-Year Conditional OS") +
  theme_mc(width_mm = 72) +
  theme(
    axis.text.y = element_text(size = 6.8, color = "black"),
    legend.position = "right",
    legend.key.height = unit(14, "mm"),
    legend.key.width = unit(3, "mm"),
    plot.margin = margin(2, 3, 2, 1, "mm")
  )

# 组合 Figure 4
fig4 <- mc_compose(p4a, p4b, p4c, ncol = 3, widths = c(1, 1, 1.35), width_mm = 180, height_mm = 88)
mc_save(fig4, "figures/fig4_conditional_survival.png", width_mm = 180, height_mm = 88)
cat("Figure 4 re-drawn and saved successfully.\n")

# [2026-10-24 08:53:50]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(cmprsk)
  library(patchwork)
})

source("scripts/mc_figure.R")

cat("Loading data for CIF...\n")
c_fit <- cuminc(ftime = df$Survival_Months, fstatus = df$Comp_Risk_Status, group = df$Age_Group)

get_cif_df <- function(c_obj, curve_name, cohort_lbl, cause_lbl) {
  dt <- c_obj[[curve_name]]
  data.frame(
    Time = dt$time,
    CIF = dt$est,
    Var = dt$var,
    Cohort = cohort_lbl,
    Cause = cause_lbl,
    stringsAsFactors = FALSE
  )
}

cif_eo_ccsm <- get_cif_df(c_fit, "Early-Onset (<50) 1", "Early-Onset (<50)", "Cancer-Specific Death (CCSM)")
cif_lo_ccsm <- get_cif_df(c_fit, "Late-Onset (>=50) 1", "Late-Onset (>=50)", "Cancer-Specific Death (CCSM)")
cif_eo_ncsm <- get_cif_df(c_fit, "Early-Onset (<50) 2", "Early-Onset (<50)", "Non-Cancer Death (NCSM)")
cif_lo_ncsm <- get_cif_df(c_fit, "Late-Onset (>=50) 2", "Late-Onset (>=50)", "Non-Cancer Death (NCSM)")

cif_all <- bind_rows(cif_eo_ccsm, cif_lo_ccsm, cif_eo_ncsm, cif_lo_ncsm)
setDT(cif_all)

table_cr_all <- read.csv("results/table_competing_risks.csv")
fg_comparison_dt <- table_cr_all[1:4, ]

# 死亡病例死因构成比演变
deceased_df <- df[Comp_Risk_Status %in% c(1, 2)]
deceased_df[, Time_Bin := fcase(
  Survival_Months <= 12, "0-1y",
  Survival_Months <= 36, "1-3y",
  Survival_Months <= 60, "3-5y",
  Survival_Months <= 120, "5-10y",
  default = ">10y"
)]
deceased_df[, Time_Bin := factor(Time_Bin, levels = c("0-1y", "1-3y", "3-5y", "5-10y", ">10y"))]

prop_dt <- deceased_df[, .(
  Total_Deaths = .N,
  CCSM_N = sum(Comp_Risk_Status == 1),
  NCSM_N = sum(Comp_Risk_Status == 2),
  CCSM_Prop = sum(Comp_Risk_Status == 1) / .N,
  NCSM_Prop = sum(Comp_Risk_Status == 2) / .N
), by = .(Age_Group, Time_Bin)][order(Age_Group, Time_Bin)]

prop_long <- melt(
  prop_dt,
  id.vars = c("Age_Group", "Time_Bin"),
  measure.vars = c("CCSM_Prop", "NCSM_Prop"),
  variable.name = "Cause_Type",
  value.name = "Proportion"
)
prop_long[, Cause_Label := factor(ifelse(Cause_Type == "CCSM_Prop", "Cancer-Specific", "Non-Cancer Competing"),
                                  levels = c("Cancer-Specific", "Non-Cancer Competing"))]

# Gray test labels
gray_row_ccsm <- table_cr_all[table_cr_all$Endpoint == "Cancer-Specific Mortality (CCSM)" & table_cr_all$Model == "Gray's Equality Test (CIF)", ]
gray_row_ncsm <- table_cr_all[table_cr_all$Endpoint == "Non-Cancer Competing Mortality (NCSM)" & table_cr_all$Model == "Gray's Equality Test (CIF)", ]

lbl_p5a <- sprintf("Gray's test P = %s", gray_row_ccsm$P_Value[1])
lbl_p5b <- sprintf("Gray's test P %s", ifelse(grepl("^<", gray_row_ncsm$P_Value[1]), gray_row_ncsm$P_Value[1], paste0("= ", gray_row_ncsm$P_Value[1])))

# Panel a: 肿瘤特异死因累积发生率 (CCSM CIF)
ccsm_dt <- cif_all[Cause == "Cancer-Specific Death (CCSM)" & Time <= 120]
ccsm_dt[, Cohort := factor(Cohort, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))]

p5a <- ggplot(ccsm_dt, aes(x = Time, y = CIF, color = Cohort)) +
  geom_step(linewidth = 0.6) +
  scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_x_continuous(breaks = seq(0, 120, by = 24), limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.4, by = 0.1), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Follow-up (Months)", y = "Cumulative Incidence (CIF)", title = "Cancer-Specific Mortality (CCSM)") +
  annotate("text", x = 115, y = 0.05, label = lbl_p5a, size = 2.4, hjust = 1, color = "#1E293B", fontface = "italic") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = c(0.35, 0.82),
    legend.background = element_rect(fill = alpha("white", 0.85), color = NA),
    legend.key.size = unit(3.5, "mm"),
    plot.margin = margin(2, 3, 2, 2, "mm")
  )

# Panel b: 非肿瘤竞争死因累积发生率 (NCSM CIF)
ncsm_dt <- cif_all[Cause == "Non-Cancer Death (NCSM)" & Time <= 120]
ncsm_dt[, Cohort := factor(Cohort, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))]

p5b <- ggplot(ncsm_dt, aes(x = Time, y = CIF, color = Cohort)) +
  geom_step(linewidth = 0.6) +
  scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = "Cohort") +
  scale_x_continuous(breaks = seq(0, 120, by = 24), limits = c(0, 120)) +
  scale_y_continuous(limits = c(0, 0.45), breaks = seq(0, 0.4, by = 0.1), labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Follow-up (Months)", y = "Cumulative Incidence (CIF)", title = "Non-Cancer Mortality (NCSM)") +
  annotate("text", x = 6, y = 0.38, label = lbl_p5b, size = 2.4, hjust = 0, color = "#1E293B", fontface = "italic") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "none",
    plot.margin = margin(2, 3, 2, 2, "mm")
  )

# Panel c: 死因构成比演变堆叠图
p5c <- ggplot(prop_long, aes(x = Time_Bin, y = Proportion, fill = Cause_Label)) +
  geom_bar(stat = "identity", position = "stack", width = 0.65) +
  facet_wrap(~ Age_Group) +
  scale_fill_manual(values = c("Cancer-Specific" = "#C4615A", "Non-Cancer Competing" = "#E7D894"), name = "Cause of Death") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  labs(x = "Time Since Diagnosis", y = "Proportion of Deaths", title = "Cause-of-Death Composition over Time") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    strip.text = element_text(size = 7.5, face = "bold"),
    plot.margin = margin(2, 3, 2, 2, "mm")
  )

# Panel d: Fine-Gray sdHR vs Cause-Specific Cox csHR 对比森林图
fg_plot_dt <- copy(fg_comparison_dt)
fg_plot_dt$Label <- c(
  "CCSM: Cause-Specific Cox",
  "CCSM: Fine-Gray sdHR",
  "NCSM: Cause-Specific Cox",
  "NCSM: Fine-Gray sdHR"
)
fg_plot_dt$Label <- factor(fg_plot_dt$Label, levels = rev(fg_plot_dt$Label))
fg_plot_dt$Label_Str <- sprintf("%.2f (%.2f-%.2f)", fg_plot_dt$HR, fg_plot_dt$HR_Lower, fg_plot_dt$HR_Upper)

p5d <- ggplot(fg_plot_dt, aes(x = HR, y = Label, color = Model, shape = Model)) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_errorbar(aes(xmin = HR_Lower, xmax = HR_Upper), width = 0.22, linewidth = 0.5) +
  geom_point(size = 2.6) +
  geom_text(aes(x = 1.45, label = Label_Str),
            hjust = 0, size = 2.4, color = "#1E293B", show.legend = FALSE) +
  annotate("text", x = 1.45, y = 4.45, label = "HR (95% CI)",
           hjust = 0, size = 2.4, fontface = "bold", color = "#1E293B") +
  scale_color_manual(values = c("Cause-Specific Cox (csHR)" = "#384D7A", "Fine-Gray Subdistribution (sdHR)" = "#C4615A"), name = "Model") +
  scale_shape_manual(values = c("Cause-Specific Cox (csHR)" = 16, "Fine-Gray Subdistribution (sdHR)" = 18), name = "Model") +
  scale_x_continuous(limits = c(0.1, 2.0), breaks = seq(0.2, 1.2, by = 0.2), expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Hazard Ratio (95% CI)", y = NULL, title = "Competing Risk Models Comparison") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 6.8, face = "bold", color = "black"),
    plot.margin = margin(2, 3, 2, 2, "mm")
  )

fig5 <- mc_compose(p5a, p5b, p5c, p5d, ncol = 2, nrow = 2, width_mm = 180, height_mm = 145)
mc_save(fig5, "figures/fig5_competing_risks.png", width_mm = 180, height_mm = 145)
cat("Figure 5 re-drawn successfully.\n")

# [2026-10-24 08:55:01]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(survival)
})

source("scripts/mc_figure.R")

df_stg4 <- df[df$Stage == "Stage IV", ]
patterns_stg4 <- c("Right colon", "Left colon", "Rectum")
stg4_res <- list()

for (ps_item in patterns_stg4) {
  sub_s4 <- df_stg4[df_stg4$Subsite == ps_item, ]
  if (nrow(sub_s4) > 100) {
    fit_s4 <- coxph(Surv(Survival_Months, CSS_Status) ~ EOCRC, data = sub_s4, weights = iptw_w, robust = TRUE)
    coef_s4 <- summary(fit_s4)$coefficients["EOCRC", ]
    stg4_res[[ps_item]] <- data.frame(
      Site = ps_item,
      HR = exp(coef_s4["coef"]),
      HR_Lower = exp(coef_s4["coef"] - 1.96 * coef_s4["robust se"]),
      HR_Upper = exp(coef_s4["coef"] + 1.96 * coef_s4["robust se"])
    )
  }
}
stg4_plot_dt <- bind_rows(stg4_res)
stg4_plot_dt$Label_Str <- sprintf("%.2f (%.2f-%.2f)", stg4_plot_dt$HR, stg4_plot_dt$HR_Lower, stg4_plot_dt$HR_Upper)

p_s4 <- ggplot(stg4_plot_dt, aes(y = reorder(Site, HR))) +
  geom_vline(xintercept = 1.0, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_errorbar(aes(xmin = HR_Lower, xmax = HR_Upper), width = 0.2, linewidth = 0.5, color = "#384D7A", orientation = "y") +
  geom_point(aes(x = HR), size = 2.6, shape = 18, color = "#384D7A") +
  geom_text(aes(x = 1.25, label = Label_Str),
            hjust = 0, size = 2.5, color = "#1E293B") +
  annotate("text", x = 1.25, y = 3.45, label = "HR (95% CI)",
           hjust = 0, size = 2.5, fontface = "bold", color = "#1E293B") +
  scale_x_continuous(limits = c(0.7, 1.6), breaks = seq(0.7, 1.2, by = 0.1), expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = "Hazard Ratio in Stage IV (95% CI)", y = NULL, title = "Stage IV Prognostic Hazard by Primary Site") +
  theme_mc(width_mm = 120) +
  theme(
    axis.text.y = element_text(size = 7.5, face = "bold", color = "black"),
    plot.margin = margin(2, 4, 2, 2, "mm")
  )

mc_save(p_s4, "manuscript/supplementary/figS4_stage4_metastasis_prognosis.png", width_mm = 120, height_mm = 80)
cat("Figure S4 re-drawn successfully.\n")

# [2026-10-24 08:57:21]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(flextable)
  library(officer)
})

# --- Table 1: 基线特征与加权平衡表 ---
t1_raw <- read.csv("results/table1_baseline.csv", stringsAsFactors = FALSE)
t1_clean <- t1_raw[!(t1_raw$Variable == "Race" & t1_raw$Category %in% c("Asian/PI", "Other/Unknown")), ]

t1_display <- data.frame(
  Variable = t1_clean$Variable,
  Characteristic = paste0("  ", t1_clean$Category),
  Unadj_EOCRC = t1_clean$Unweighted_EOCRC,
  Unadj_LOCRC = t1_clean$Unweighted_LOCRC,
  Adj_EOCRC = t1_clean$Weighted_EOCRC,
  Adj_LOCRC = t1_clean$Weighted_LOCRC,
  stringsAsFactors = FALSE
)

t1_rows <- list()
current_var <- ""
for (i in seq_len(nrow(t1_display))) {
  v <- t1_display$Variable[i]
  if (v != current_var) {
    current_var <- v
    var_title <- switch(
      v,
      "Sex" = "Sex, n (%)",
      "Race" = "Race/Ethnicity, n (%)",
      "Year_Group" = "Diagnosis Period, n (%)",
      "Subsite" = "Primary Anatomical Site, n (%)",
      "Histology" = "Histologic Subtype, n (%)",
      "Grade" = "Tumor Differentiation Grade, n (%)",
      "Stage" = "AJCC Stage, n (%)",
      "Surgery" = "Definitive Surgical Resection, n (%)",
      "Chemo" = "Chemotherapy Received, n (%)",
      "Radiation" = "Radiation Therapy Received, n (%)",
      "LN_Examined" = "Regional Lymph Nodes Examined, n (%)",
      v
    )
    t1_rows[[length(t1_rows) + 1]] <- data.frame(
      Characteristic = var_title,
      Unadj_EOCRC = "", Unadj_LOCRC = "",
      Adj_EOCRC = "", Adj_LOCRC = "",
      is_header = TRUE,
      stringsAsFactors = FALSE
    )
  }
  row_data <- t1_display[i, c("Characteristic", "Unadj_EOCRC", "Unadj_LOCRC", "Adj_EOCRC", "Adj_LOCRC")]
  row_data$is_header <- FALSE
  t1_rows[[length(t1_rows) + 1]] <- row_data
}
t1_final <- bind_rows(t1_rows)

ft1 <- flextable(t1_final[, 1:5])
ft1 <- set_header_labels(
  ft1,
  Characteristic = "Patient Characteristic",
  Unadj_EOCRC = "EOCRC (n = 60,819)",
  Unadj_LOCRC = "LOCRC (n = 405,605)",
  Adj_EOCRC = "EOCRC (% wt)",
  Adj_LOCRC = "LOCRC (% wt)"
)
ft1 <- add_header_row(
  ft1,
  values = c("", "Unadjusted Cohort", "IPTW-Adjusted Cohort"),
  colwidths = c(1, 2, 2)
)
ft1 <- theme_booktabs(ft1)
ft1 <- font(ft1, fontname = "Calibri", part = "all")
ft1 <- fontsize(ft1, size = 8.5, part = "body")
ft1 <- fontsize(ft1, size = 9, part = "header")
ft1 <- bold(ft1, part = "header")
for (r in which(t1_final$is_header)) {
  ft1 <- bold(ft1, i = r, part = "body")
  ft1 <- italic(ft1, i = r, part = "body")
}
ft1 <- align(ft1, align = "center", part = "all")
ft1 <- align(ft1, j = 1, align = "left", part = "all")
ft1 <- set_caption(ft1, caption = "Table 1. Baseline demographic, clinical, and pathological characteristics of early-onset vs. late-onset colorectal cancer patients before and after IPTW weighting.")
ft1 <- add_footer_lines(ft1, values = "Abbreviations: EOCRC, Early-Onset Colorectal Cancer (<50 years); LOCRC, Late-Onset Colorectal Cancer (>=50 years); IPTW, Inverse Probability of Treatment Weighting; SMD, Standardized Mean Difference.\n*Stabilized IPTW balanced all 11 baseline covariates to SMD < 0.05.")
ft1 <- fontsize(ft1, size = 8, part = "footer")
ft1 <- italic(ft1, part = "footer")
ft1 <- autofit(ft1)
save_as_docx(ft1, path = "manuscript/tables/table1_baseline.docx")
cat("Table 1 exported successfully.\n")

# --- Table 2: 转移模式与多变量回归表 ---
t2_raw <- read.csv("results/table2_metastasis_logistic.csv", stringsAsFactors = FALSE)
t2_display <- data.frame(
  Metastatic_Site = t2_raw$Metastasis_Pattern,
  LOCRC_Events = t2_raw$LOCRC_Events,
  EOCRC_Events = t2_raw$EOCRC_Events,
  Unadj_OR = t2_raw$Crude_OR_Str,
  Adj_aOR = t2_raw$Adjusted_OR_Str,
  P_Val = t2_raw$P_Value,
  stringsAsFactors = FALSE
)

ft2 <- flextable(t2_display)
ft2 <- set_header_labels(
  ft2,
  Metastatic_Site = "Metastatic Organ Pattern",
  LOCRC_Events = "LOCRC, No. (%)",
  EOCRC_Events = "EOCRC, No. (%)",
  Unadj_OR = "Unadjusted OR (95% CI)",
  Adj_aOR = "Adjusted aOR (95% CI)*",
  P_Val = "P Value"
)
ft2 <- theme_booktabs(ft2)
ft2 <- font(ft2, fontname = "Calibri", part = "all")
ft2 <- fontsize(ft2, size = 9, part = "body")
ft2 <- fontsize(ft2, size = 9.5, part = "header")
ft2 <- bold(ft2, part = "header")
ft2 <- align(ft2, align = "center", part = "all")
ft2 <- align(ft2, j = 1, align = "left", part = "all")
ft2 <- set_caption(ft2, caption = "Table 2. Organ-specific distant metastatic patterns and multivariable logistic regression in early-onset vs. late-onset colorectal cancer (2010–2022 modern cohort).")
ft2 <- add_footer_lines(ft2, values = "*Multivariable logistic regression adjusted for sex, race/ethnicity, primary anatomical site, histologic grade, adenocarcinoma subtype, and pretreatment CEA status.\nAbbreviations: EOCRC, Early-Onset Colorectal Cancer; LOCRC, Late-Onset Colorectal Cancer; aOR, adjusted odds ratio; CI, confidence interval.")
ft2 <- fontsize(ft2, size = 8, part = "footer")
ft2 <- italic(ft2, part = "footer")
ft2 <- autofit(ft2)
save_as_docx(ft2, path = "manuscript/tables/table2_metastasis.docx")
cat("Table 2 exported successfully.\n")

# --- Table 3: 生存模型与时变分析表 ---
t3_raw <- read.csv("results/table3_survival_models.csv", stringsAsFactors = FALSE)
t3_raw$HR_Str[t3_raw$HR_Str == "NA (NA-NA)"] <- "Reference / NA"

t3_display <- data.frame(
  Model = t3_raw$Model,
  HR_CI = t3_raw$HR_Str,
  P_Val = t3_raw$P_Value,
  stringsAsFactors = FALSE
)

ft3 <- flextable(t3_display)
ft3 <- set_header_labels(
  ft3,
  Model = "Survival Outcome & Regression Model",
  HR_CI = "Hazard Ratio (95% CI)",
  P_Val = "P Value"
)
ft3 <- theme_booktabs(ft3)
ft3 <- font(ft3, fontname = "Calibri", part = "all")
ft3 <- fontsize(ft3, size = 9, part = "body")
ft3 <- fontsize(ft3, size = 9.5, part = "header")
ft3 <- bold(ft3, part = "header")
ft3 <- align(ft3, align = "center", part = "all")
ft3 <- align(ft3, j = 1, align = "left", part = "all")
ft3 <- set_caption(ft3, caption = "Table 3. Multivariable Cox, IPTW-weighted marginal Cox, and dynamic time-stratified regression models for survival outcomes in early-onset vs. late-onset colorectal cancer.")
ft3 <- add_footer_lines(ft3, values = "Abbreviations: HR, hazard ratio; CI, confidence interval; IPTW, inverse probability of treatment weighting; OS, overall survival; CSS, cancer-specific survival.\n*Weighted Cox models incorporated stabilized IPTW with robust sandwich standard errors.")
ft3 <- fontsize(ft3, size = 8, part = "footer")
ft3 <- italic(ft3, part = "footer")
ft3 <- autofit(ft3)
save_as_docx(ft3, path = "manuscript/tables/table3_models.docx")
cat("Table 3 exported successfully.\n")

# [2026-10-24 09:08:38]
library(data.table)
df <- readRDS("data/cleaned_cohort_full.rds")
m_sub <- df[Year_DX >= 2010]
cat("Total Modern:", nrow(m_sub), "\n")
cat("EOCRC Modern:", sum(m_sub$EOCRC == 1), "\n")
cat("LOCRC Modern:", sum(m_sub$EOCRC == 0), "\n")

# [2026-10-24 09:10:34]
clean_var_name <- function(vars) {
  v <- gsub("^Year_Group_", "Period: ", vars)
  v <- gsub("^Subsite_", "Site: ", v)
  v <- gsub("^Histology_", "Histology: ", v)
  v <- gsub("^Grade_", "Grade: ", v)
  v <- gsub("^Stage_", "Stage: ", v)
  v <- gsub("^Surgery_", "Surgery: ", v)
  v <- gsub("^Chemo_", "Chemo: ", v)
  v <- gsub("^Radiation_", "Radiation: ", v)
  v <- gsub("^LN_Examined_", "LN: ", v)
  v <- gsub("^Race_", "Race: ", v)
  v <- gsub("^Sex_", "Sex: ", v)
  v
}
raw_vars <- c("Sex_Male", "Race_Non-Hispanic White", "Stage_Stage I", "Chemo_Yes")
print(clean_var_name(raw_vars))

# [2026-10-24 09:12:15]
suppressPackageStartupMessages({
  library(data.table)
  library(tidyverse)
  library(patchwork)
  library(jsonlite)
})

source("scripts/mc_figure.R")

dir.create(".medcortex/figure_variants/fig1_consort_iptw", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/draft", recursive = TRUE, showWarnings = FALSE)

clean_var_name <- function(vars) {
  v <- gsub("^Year_Group_", "Period: ", vars)
  v <- gsub("^Subsite_", "Site: ", v)
  v <- gsub("^Histology_", "Histology: ", v)
  v <- gsub("^Grade_", "Grade: ", v)
  v <- gsub("^Stage_", "Stage: ", v)
  v <- gsub("^Surgery_", "Surgery: ", v)
  v <- gsub("^Chemo_", "Chemo: ", v)
  v <- gsub("^Radiation_", "Radiation: ", v)
  v <- gsub("^LN_Examined_", "LN: ", v)
  v <- gsub("^Race_", "Race: ", v)
  v <- gsub("^Sex_", "Sex: ", v)
  v
}

raw_bal <- read.csv("results/tableS2_iptw_balance.csv", stringsAsFactors = FALSE)
bal_df <- as.data.table(raw_bal)
love_data <- bal_df[!Variable %in% c("distance", "prop.score")]
love_data$Clean_Var <- clean_var_name(love_data$Variable)
setorder(love_data, -Diff.Un)

# 通用函数：生成 Love Plot
make_love <- function(dt, unadj_col = "#C4615A", adj_col = "#384D7A", unadj_shape = 16, adj_shape = 17, font_sz = 6.8, width_mm = 90) {
  l_long <- melt(dt, id.vars = "Clean_Var", measure.vars = c("Diff.Un", "Diff.Adj"), variable.name = "Cohort_Weight", value.name = "SMD")
  l_long[, Cohort_Weight := factor(ifelse(Cohort_Weight == "Diff.Un", "Unadjusted", "IPTW-Adjusted"), levels = c("Unadjusted", "IPTW-Adjusted"))]
  l_long[, Abs_SMD := abs(SMD)]
  max_x <- max(l_long$Abs_SMD) * 1.15
  
  ggplot(l_long, aes(x = Abs_SMD, y = reorder(Clean_Var, Abs_SMD), color = Cohort_Weight, shape = Cohort_Weight)) +
    geom_vline(xintercept = 0.05, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
    geom_vline(xintercept = 0.10, linetype = "dotted", color = "gray50", linewidth = 0.3) +
    geom_point(size = 2.1, stroke = 0.5) +
    scale_color_manual(values = c("Unadjusted" = unadj_col, "IPTW-Adjusted" = adj_col), name = "Sample") +
    scale_shape_manual(values = c("Unadjusted" = unadj_shape, "IPTW-Adjusted" = adj_shape), name = "Sample") +
    scale_x_continuous(limits = c(0, max_x), expand = expansion(mult = c(0, 0.02))) +
    labs(x = "Absolute Standardized Mean Difference (SMD)", y = NULL, title = "Covariate Balance (Love Plot)") +
    annotate("text", x = 0.055, y = nrow(dt) - 0.5, label = "Threshold: SMD < 0.05", size = 2.2, color = "#D9534F", hjust = 0, fontface = "italic") +
    theme_mc(width_mm = width_mm) +
    theme(
      legend.position = "bottom",
      legend.margin = margin(t = 1, unit = "mm"),
      axis.text.y = element_text(size = font_sz, color = "black"),
      plot.margin = margin(2, 4, 2, 2, "mm")
    )
}

# -------------------------------------------------------------
# 版本 1 (v1): 表达思路维度 —— 队列划分闭环版 (Narrative Complete)
# -------------------------------------------------------------
cb_v1 <- data.frame(
  x = c(5.0, 5.0, 5.0, 5.0, 5.0, 2.6, 7.4),
  y = c(10.5, 8.2, 5.9, 3.6, 1.6, -0.6, -0.6),
  label = c(
    "SEER CRC Raw Cohort (2000-2022)\nN = 654,149 records (631,157 patients)",
    "First Primary Malignancy Only\nN = 530,027",
    "Valid Follow-up Time (> 0 months)\nN = 504,397",
    "Confirmed Colorectal Adenocarcinoma\nN = 466,424",
    "Full Analysis Cohort (N = 466,424)\nEOCRC: 60,819 | LOCRC: 405,605",
    "Long-term Survival Cohort\n(2000-2022, n = 466,424)",
    "Modern Metastasis Cohort\n(2010-2022, n = 256,365)"
  ),
  is_sub = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)
eb_v1 <- data.frame(
  x = c(8.8, 8.8, 8.8),
  y = c(9.35, 7.05, 4.75),
  label = c(
    "Excluded prior malignancies\n(n = 124,122)",
    "Excluded survival <= 0 months\n(n = 25,630)",
    "Excluded non-adenocarcinoma histology\n(n = 37,973)"
  )
)

p1a_v1 <- ggplot() +
  geom_rect(data = cb_v1[!cb_v1$is_sub, ], aes(xmin = x - 2.15, xmax = x + 2.15, ymin = y - 0.62, ymax = y + 0.62),
            fill = "#F8FAFC", color = "#384D7A", linewidth = 0.45) +
  geom_text(data = cb_v1[!cb_v1$is_sub, ], aes(x = x, y = y, label = label), size = 2.3, lineheight = 0.95, color = "#1E293B") +
  geom_rect(data = cb_v1[cb_v1$is_sub, ], aes(xmin = x - 2.05, xmax = x + 2.05, ymin = y - 0.55, ymax = y + 0.55),
            fill = "#EEF2F6", color = "#384D7A", linewidth = 0.4) +
  geom_text(data = cb_v1[cb_v1$is_sub, ], aes(x = x, y = y, label = label), size = 2.05, lineheight = 0.9, color = "#1E293B", fontface = "bold") +
  geom_rect(data = eb_v1, aes(xmin = x - 2.05, xmax = x + 2.05, ymin = y - 0.48, ymax = y + 0.48),
            fill = "#FEF2F2", color = "#C4615A", linewidth = 0.35) +
  geom_text(data = eb_v1, aes(x = x, y = y, label = label), size = 2.1, lineheight = 0.9, color = "#991B1B") +
  geom_segment(aes(x = 5.0, xend = 5.0, y = 9.88, yend = 8.82), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 5.0, xend = 5.0, y = 7.58, yend = 6.52), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 5.0, xend = 5.0, y = 5.28, yend = 4.22), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 5.0, xend = 5.0, y = 2.98, yend = 2.22), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 5.0, xend = 6.72, y = 9.35, yend = 9.35), arrow = arrow(length = unit(1.4, "mm")), linewidth = 0.32, color = "#C4615A") +
  geom_segment(aes(x = 5.0, xend = 6.72, y = 7.05, yend = 7.05), arrow = arrow(length = unit(1.4, "mm")), linewidth = 0.32, color = "#C4615A") +
  geom_segment(aes(x = 5.0, xend = 6.72, y = 4.75, yend = 4.75), arrow = arrow(length = unit(1.4, "mm")), linewidth = 0.32, color = "#C4615A") +
  geom_segment(aes(x = 4.2, xend = 2.6, y = 0.98, yend = -0.05), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 5.8, xend = 7.4, y = 0.98, yend = -0.05), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.35, color = "#384D7A") +
  scale_x_continuous(limits = c(0.4, 11.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-1.4, 11.5), expand = c(0, 0)) +
  labs(title = "Study Cohort Selection & Subsets (CONSORT)") +
  theme_void() +
  theme(plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5, margin = margin(b = 2.5, unit = "mm")))

p1b_v1 <- make_love(head(love_data, 15), font_sz = 6.8)
fig_v1 <- mc_compose(p1a_v1, p1b_v1, ncol = 2, widths = c(1.1, 1), width_mm = 180, height_mm = 112)
mc_save(fig_v1, ".medcortex/figure_variants/fig1_consort_iptw/v1_narrative.png", width_mm = 180, height_mm = 112)

# -------------------------------------------------------------
# 版本 2 (v2): 信息密度维度 —— 全量 20+ 项协变量全谱平衡 (Density Spectrum)
# -------------------------------------------------------------
cb_base <- data.frame(
  x = c(3.6, 3.6, 3.6, 3.6, 3.6),
  y = c(10, 7.5, 5.0, 2.5, 0.2),
  label = c(
    "SEER CRC Raw Cohort (2000-2022)\nN = 654,149 records (631,157 patients)",
    "First Primary Malignancy Only\nN = 530,027",
    "Valid Follow-up Time (> 0 months)\nN = 504,397",
    "Confirmed Colorectal Adenocarcinoma\nN = 466,424",
    "Full Analysis Cohort (N = 466,424)\nEOCRC: 60,819 | LOCRC: 405,605"
  )
)
eb_base <- data.frame(
  x = c(8.8, 8.8, 8.8),
  y = c(8.75, 6.25, 3.75),
  label = c(
    "Excluded prior malignancies\n(n = 124,122)",
    "Excluded survival <= 0 months\n(n = 25,630)",
    "Excluded non-adenocarcinoma histology\n(n = 37,973)"
  )
)

make_consort_plot <- function(box_col = "#384D7A", excl_col = "#C4615A") {
  ggplot() +
    geom_rect(data = cb_base, aes(xmin = x - 2.25, xmax = x + 2.25, ymin = y - 0.68, ymax = y + 0.68),
              fill = "#F8FAFC", color = box_col, linewidth = 0.45) +
    geom_text(data = cb_base, aes(x = x, y = y, label = label), size = 2.45, lineheight = 0.95, color = "#1E293B") +
    geom_rect(data = eb_base, aes(xmin = x - 2.2, xmax = x + 2.2, ymin = y - 0.52, ymax = y + 0.52),
              fill = "#FEF2F2", color = excl_col, linewidth = 0.35) +
    geom_text(data = eb_base, aes(x = x, y = y, label = label), size = 2.2, lineheight = 0.9, color = "#991B1B") +
    geom_segment(aes(x = 3.6, xend = 3.6, y = 9.32, yend = 8.18), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.35, color = box_col) +
    geom_segment(aes(x = 3.6, xend = 3.6, y = 6.82, yend = 5.68), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.35, color = box_col) +
    geom_segment(aes(x = 3.6, xend = 3.6, y = 4.32, yend = 3.18), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.35, color = box_col) +
    geom_segment(aes(x = 3.6, xend = 3.6, y = 1.82, yend = 0.88), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.35, color = box_col) +
    geom_segment(aes(x = 3.6, xend = 6.55, y = 8.75, yend = 8.75), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = excl_col) +
    geom_segment(aes(x = 3.6, xend = 6.55, y = 6.25, yend = 6.25), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = excl_col) +
    geom_segment(aes(x = 3.6, xend = 6.55, y = 3.75, yend = 3.75), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = excl_col) +
    scale_x_continuous(limits = c(0.8, 11.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-0.8, 11.2), expand = c(0, 0)) +
    labs(title = "Study Cohort Selection (CONSORT)") +
    theme_void() +
    theme(plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5, margin = margin(b = 3, unit = "mm")))
}

p1a_v2 <- make_consort_plot()
p1b_v2 <- make_love(love_data, font_sz = 5.4, width_mm = 90) # 展示全量变量
fig_v2 <- mc_compose(p1a_v2, p1b_v2, ncol = 2, widths = c(1, 1.15), width_mm = 180, height_mm = 115)
mc_save(fig_v2, ".medcortex/figure_variants/fig1_consort_iptw/v2_density.png", width_mm = 180, height_mm = 115)

# -------------------------------------------------------------
# 版本 3 (v3): 布局维度 —— 上下堆叠通栏排布版 (Stacked Layout)
# -------------------------------------------------------------
consort_h_boxes <- data.frame(
  x = c(1.8, 4.8, 7.8, 10.8, 13.8),
  y = c(3.0, 3.0, 3.0, 3.0, 3.0),
  label = c(
    "SEER CRC Raw Cohort\n(2000-2022)\nN = 654,149",
    "First Primary Only\nN = 530,027",
    "Valid Follow-up > 0 mo\nN = 504,397",
    "Adenocarcinoma\nN = 466,424",
    "Full Analysis Cohort\nEOCRC: 60,819\nLOCRC: 405,605"
  )
)
excl_h_boxes <- data.frame(
  x = c(3.3, 6.3, 9.3),
  y = c(1.0, 1.0, 1.0),
  label = c(
    "Prior malignancies\n(n = 124,122)",
    "Survival <= 0 mo\n(n = 25,630)",
    "Other histologies\n(n = 37,973)"
  )
)

p1a_v3 <- ggplot() +
  geom_rect(data = consort_h_boxes, aes(xmin = x - 1.25, xmax = x + 1.25, ymin = y - 0.75, ymax = y + 0.75),
            fill = "#F8FAFC", color = "#384D7A", linewidth = 0.45) +
  geom_text(data = consort_h_boxes, aes(x = x, y = y, label = label), size = 2.3, lineheight = 0.95, color = "#1E293B") +
  geom_rect(data = excl_h_boxes, aes(xmin = x - 1.1, xmax = x + 1.1, ymin = y - 0.55, ymax = y + 0.55),
            fill = "#FEF2F2", color = "#C4615A", linewidth = 0.35) +
  geom_text(data = excl_h_boxes, aes(x = x, y = y, label = label), size = 2.05, lineheight = 0.9, color = "#991B1B") +
  geom_segment(aes(x = 3.05, xend = 3.55, y = 3.0, yend = 3.0), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 6.05, xend = 6.55, y = 3.0, yend = 3.0), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 9.05, xend = 9.55, y = 3.0, yend = 3.0), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 12.05, xend = 12.55, y = 3.0, yend = 3.0), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.35, color = "#384D7A") +
  geom_segment(aes(x = 3.3, xend = 3.3, y = 3.0, yend = 1.6), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = "#C4615A") +
  geom_segment(aes(x = 6.3, xend = 6.3, y = 3.0, yend = 1.6), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = "#C4615A") +
  geom_segment(aes(x = 9.3, xend = 9.3, y = 3.0, yend = 1.6), arrow = arrow(length = unit(1.5, "mm")), linewidth = 0.32, color = "#C4615A") +
  scale_x_continuous(limits = c(0.4, 15.2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0.2, 4.2), expand = c(0, 0)) +
  labs(title = "Study Cohort Selection (CONSORT Flowchart)") +
  theme_void() +
  theme(plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5, margin = margin(b = 2, unit = "mm")))

# 下排 Love Plot 通栏展开 (绝对不截断 Chemo_Yes 等高点)
p1b_v3 <- make_love(head(love_data, 15), font_sz = 7.0, width_mm = 180)
fig_v3 <- mc_compose(p1a_v3, p1b_v3, ncol = 1, heights = c(1, 1.4), width_mm = 180, height_mm = 145)
mc_save(fig_v3, ".medcortex/figure_variants/fig1_consort_iptw/v3_stacked.png", width_mm = 180, height_mm = 145)

# -------------------------------------------------------------
# 版本 4 (v4): 配色维度 —— Lancet 经典深海蓝与权威深红 (Lancet Palette)
# -------------------------------------------------------------
p1a_v4 <- make_consort_plot(box_col = "#00468B", excl_col = "#ED0000")
p1b_v4 <- make_love(head(love_data, 15), unadj_col = "#ED0000", adj_col = "#00468B", unadj_shape = 15, adj_shape = 18, font_sz = 6.8)
fig_v4 <- mc_compose(p1a_v4, p1b_v4, ncol = 2, widths = c(1, 1.1), width_mm = 180, height_mm = 110)
mc_save(fig_v4, ".medcortex/figure_variants/fig1_consort_iptw/v4_palette.png", width_mm = 180, height_mm = 110)

# 将最推荐版另存至 draft
file.copy(".medcortex/figure_variants/fig1_consort_iptw/v1_narrative.png", "figures/draft/fig1_consort_iptw.png", overwrite = TRUE)

# 写入 manifest.json
manifest <- list(
  figure = "fig1_consort_iptw.png",
  rationale = "Figure 1 在叙事闭环(后续两队列分流)、信息密度(全量协变量平衡)、排版布局(上下通栏)与权威期刊配色四个层面各有显著优化空间，四版各攻一维以便精准比选",
  current = "",
  variants = list(
    list(
      id = "v1",
      file = "v1_narrative.png",
      dimension = "表达思路",
      label = "队列分流闭环版 (推荐)",
      note = "Panel a 底部增设长期生存(46.6万)与现代转移(25.6万)分流，实现全文分析架构逻辑闭环"
    ),
    list(
      id = "v2",
      file = "v2_density.png",
      dimension = "信息密度",
      label = "全量协变量平衡谱",
      note = "Panel b 展示全量 20+ 项协变量水平，完整展示所有混杂因素加权后 SMD 均 < 0.05 的证据链"
    ),
    list(
      id = "v3",
      file = "v3_stacked.png",
      dimension = "布局",
      label = "上下通栏堆叠版",
      note = "上排纳排横向流、下排 Love 图大开大合展开，适合宽版期刊版面，主次呼吸感强"
    ),
    list(
      id = "v4",
      file = "v4_palette.png",
      dimension = "配色",
      label = "Lancet 权威色相",
      note = "采用 Lancet 经典深海蓝(#00468B)与权威深红(#ED0000)，方块与菱形双重编码，黑白打印极易区分"
    )
  )
)

write_json(manifest, ".medcortex/figure_variants/fig1_consort_iptw/manifest.json", auto_unbox = TRUE, pretty = TRUE)
cat("ALL_DONE_SUCCESSFULLY\n")

# [2026-10-24 09:56:54]
library(data.table)
library(ggplot2)

bal_df <- fread("results/tableS2_iptw_balance.csv")
print(head(bal_df))

# [2026-10-24 09:57:33]
source("scripts/mc_figure.R")
library(data.table)
library(ggplot2)
library(patchwork)

bal_df <- fread("results/tableS2_iptw_balance.csv")

# Let's inspect the exact love plot data
love_data <- copy(bal_df[!Variable %in% c("distance", "prop.score")])

# Clean variable labels
var_map <- c(
  "Chemo_Yes" = "Chemotherapy received",
  "Subsite_Right colon" = "Right colon",
  "Race_Non-Hispanic White" = "Non-Hispanic White",
  "Radiation_Yes" = "Radiation received",
  "Race_Hispanic" = "Hispanic",
  "Stage_Stage IV" = "Stage IV",
  "LN_Examined_>=12 nodes" = ">=12 LNs examined",
  "Subsite_Rectum" = "Rectum",
  "LN_Examined_<12 nodes" = "<12 LNs examined",
  "Stage_Stage II" = "Stage II",
  "Stage_Stage I" = "Stage I",
  "Stage_Stage III" = "Stage III",
  "Subsite_Left colon" = "Left colon",
  "Year_Group_2016-2022" = "Period 2016-2022",
  "Year_Group_2000-2005" = "Period 2000-2005"
)

love_data[, Clean_Var := var_map[Variable]]
love_data[is.na(Clean_Var), Clean_Var := Variable]

setorder(love_data, -Diff.Un)
plot_love_dt <- head(love_data, 15)

love_long <- melt(
  plot_love_dt,
  id.vars = "Clean_Var",
  measure.vars = c("Diff.Un", "Diff.Adj"),
  variable.name = "Cohort_Weight",
  value.name = "SMD"
)
love_long[, Cohort_Weight := factor(ifelse(Cohort_Weight == "Diff.Un", "Unadjusted", "IPTW-Adjusted"), levels = c("Unadjusted", "IPTW-Adjusted"))]
love_long[, Abs_SMD := abs(SMD)]

# Check ordering
love_long[, Clean_Var := factor(Clean_Var, levels = rev(plot_love_dt$Clean_Var))]
print(levels(love_long$Clean_Var))

# [2026-10-24 09:58:38]
# Let's test a refined version of p1a and p1b
source("scripts/mc_figure.R")
library(data.table)
library(ggplot2)
library(patchwork)

# 1. CONSORT Diagram (Panel a)
# Design clean coordinate system from 0 to 100 on X and Y
# X: 0 to 100.
# Main box: center x = 30, width = 50 (from 5 to 55).
# Exclusion box: center x = 78, width = 40 (from 58 to 98).
# Y: 0 to 100.
# Main boxes at y = 90, 70, 50, 30, 10. Height = 13.
# Excl boxes at y = 80, 60, 40. Height = 11.

consort_boxes <- data.frame(
  x = c(30, 30, 30, 30, 30),
  y = c(90, 70, 50, 30, 10),
  w = c(50, 50, 50, 50, 50),
  h = c(13, 11, 11, 11, 13),
  label = c(
    "SEER Colorectal Cancer (2000–2022)\n654,149 records (631,157 patients)",
    "First Primary Colorectal Cancer\nN = 530,027",
    "Known Survival Time > 0 Months\nN = 504,397",
    "Colorectal Adenocarcinoma\nN = 466,424",
    "Full Analysis Cohort (N = 466,424)\nEarly-onset: 60,819 | Late-onset: 405,605"
  )
)

excl_boxes <- data.frame(
  x = c(78, 78, 78),
  y = c(80, 60, 40),
  w = c(40, 40, 40),
  h = c(10, 10, 10),
  label = c(
    "Excluded prior cancer\n(n = 124,122)",
    "Excluded survival <= 0\n(n = 25,630)",
    "Non-adenocarcinoma histology\n(n = 37,973)"
  )
)

p1a <- ggplot() +
  # Main flow boxes (light blue fill, dark blue border)
  geom_rect(data = consort_boxes, aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#F8FAFC", color = "#384D7A", linewidth = 0.5) +
  geom_text(data = consort_boxes, aes(x = x, y = y, label = label),
            size = 2.5, lineheight = 1.1, color = "#1E293B") +
  # Exclusion boxes (light red fill, dark red border)
  geom_rect(data = excl_boxes, aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#FEF2F2", color = "#C4615A", linewidth = 0.45) +
  geom_text(data = excl_boxes, aes(x = x, y = y, label = label),
            size = 2.4, lineheight = 1.1, color = "#991B1B") +
  # Vertical arrows (box bottom to next box top)
  # 90-6.5=83.5 to 70+5.5=75.5
  geom_segment(aes(x = 30, xend = 30, y = 83.5, yend = 76.2), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  # 70-5.5=64.5 to 50+5.5=55.5
  geom_segment(aes(x = 30, xend = 30, y = 64.5, yend = 56.2), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  # 50-5.5=44.5 to 30+5.5=35.5
  geom_segment(aes(x = 30, xend = 30, y = 44.5, yend = 36.2), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  # 30-5.5=24.5 to 10+6.5=16.5
  geom_segment(aes(x = 30, xend = 30, y = 24.5, yend = 17.2), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  # Horizontal branch arrows
  # from stem x=30 to excl box left edge x=58
  geom_segment(aes(x = 30, xend = 57.5, y = 80, yend = 80), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  geom_segment(aes(x = 30, xend = 57.5, y = 60, yend = 60), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  geom_segment(aes(x = 30, xend = 57.5, y = 40, yend = 40), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  scale_x_continuous(limits = c(0, 102), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = "Study Cohort Selection (CONSORT)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5, margin = margin(b = 2, unit = "mm")),
    plot.margin = margin(t = 2, r = 3, b = 2, l = 2, "mm")
  )

# 2. Panel b Love Plot
bal_df <- fread("results/tableS2_iptw_balance.csv")
love_data <- copy(bal_df[!Variable %in% c("distance", "prop.score")])
love_data[, Abs_Diff_Un := abs(Diff.Un)]
setorder(love_data, -Abs_Diff_Un)
top_vars <- head(love_data, 15)

# Clean, professional, succinct labels
var_labels <- c(
  "Chemo_Yes" = "Chemotherapy received",
  "Subsite_Right colon" = "Right colon subsite",
  "Race_Non-Hispanic White" = "Non-Hispanic White",
  "Radiation_Yes" = "Radiation received",
  "Race_Hispanic" = "Hispanic ethnicity",
  "Stage_Stage IV" = "AJCC Stage IV",
  "LN_Examined_>=12 nodes" = ">=12 LNs examined",
  "Subsite_Rectum" = "Rectum subsite",
  "LN_Examined_<12 nodes" = "<12 LNs examined",
  "Stage_Stage II" = "AJCC Stage II",
  "Stage_Stage I" = "AJCC Stage I",
  "Stage_Stage III" = "AJCC Stage III",
  "Subsite_Left colon" = "Left colon subsite",
  "Year_Group_2016-2022" = "Diagnosis 2016-2022",
  "Year_Group_2000-2005" = "Diagnosis 2000-2005"
)
top_vars[, Clean_Var := var_labels[Variable]]
top_vars[is.na(Clean_Var), Clean_Var := Variable]

love_long <- melt(
  top_vars,
  id.vars = "Clean_Var",
  measure.vars = c("Diff.Un", "Diff.Adj"),
  variable.name = "Cohort_Weight",
  value.name = "SMD"
)
love_long[, Cohort_Weight := factor(ifelse(Cohort_Weight == "Diff.Un", "Unadjusted", "IPTW-Adjusted"), levels = c("Unadjusted", "IPTW-Adjusted"))]
love_long[, Abs_SMD := abs(SMD)]
love_long[, Clean_Var := factor(Clean_Var, levels = rev(top_vars$Clean_Var))]

p1b <- ggplot(love_long, aes(x = Abs_SMD, y = Clean_Var, color = Cohort_Weight, shape = Cohort_Weight)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_vline(xintercept = 0.10, linetype = "dotted", color = "gray60", linewidth = 0.3) +
  geom_point(size = 2.2, stroke = 0.5) +
  scale_color_manual(values = c("Unadjusted" = "#C4615A", "IPTW-Adjusted" = "#384D7A"), name = "Sample") +
  scale_shape_manual(values = c("Unadjusted" = 16, "IPTW-Adjusted" = 17), name = "Sample") +
  scale_x_continuous(
    limits = c(0, 0.30),
    breaks = seq(0, 0.30, by = 0.05),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(x = "Absolute Standardized Mean Difference (SMD)", y = NULL, title = "Covariate Balance (Love Plot)") +
  annotate("text", x = 0.055, y = 14.8, label = "Threshold: SMD < 0.05", size = 2.4, color = "#D9534F", hjust = 0, fontface = "italic") +
  theme_mc(width_mm = 88) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 7.2, color = "black"),
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 2, r = 4, b = 2, l = 2, "mm")
  )

# Combine
test_fig1 <- mc_compose(p1a, p1b, ncol = 2, widths = c(1, 1.08), width_mm = 180, height_mm = 110)
mc_save(test_fig1, "figures/test_fig1.png", width_mm = 180, height_mm = 110)
cat("Test fig1 saved!\n")

# [2026-10-24 10:02:58]
source("scripts/mc_figure.R")
library(data.table)
library(ggplot2)
library(patchwork)

# 1. Panel a: CONSORT Flowchart
consort_boxes <- data.frame(
  x = c(28, 28, 28, 28, 28),
  y = c(90, 70, 50, 30, 10),
  w = c(52, 52, 52, 52, 52),
  h = c(13.5, 11.5, 11.5, 11.5, 13.5),
  label = c(
    "SEER CRC (2000-2022)\n654,149 records\n(631,157 patients)",
    "First Primary CRC\nN = 530,027",
    "Survival Time > 0 mo\nN = 504,397",
    "Colorectal Adenocarcinoma\nN = 466,424",
    "Full Analysis Cohort (N = 466,424)\nEarly-onset: 60,819\nLate-onset: 405,605"
  )
)

excl_boxes <- data.frame(
  x = c(77, 77, 77),
  y = c(80, 60, 40),
  w = c(42, 42, 42),
  h = c(10.5, 10.5, 10.5),
  label = c(
    "Excluded prior cancer\n(n = 124,122)",
    "Excluded survival <= 0\n(n = 25,630)",
    "Non-adenocarcinoma\n(n = 37,973)"
  )
)

p1a <- ggplot() +
  # 主流程方框
  geom_rect(data = consort_boxes, aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#F8FAFC", color = "#384D7A", linewidth = 0.5) +
  geom_text(data = consort_boxes, aes(x = x, y = y, label = label),
            size = 2.5, lineheight = 1.08, color = "#1E293B", fontface = "plain") +
  # 排除分支方框
  geom_rect(data = excl_boxes, aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#FEF2F2", color = "#C4615A", linewidth = 0.45) +
  geom_text(data = excl_boxes, aes(x = x, y = y, label = label),
            size = 2.45, lineheight = 1.08, color = "#991B1B") +
  # 垂直主干箭头
  geom_segment(aes(x = 28, xend = 28, y = 83.25, yend = 76.2), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 28, xend = 28, y = 64.25, yend = 56.2), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 28, xend = 28, y = 44.25, yend = 36.2), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 28, xend = 28, y = 24.25, yend = 17.2), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  # 水平分支箭头
  geom_segment(aes(x = 28, xend = 55.5, y = 80, yend = 80), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  geom_segment(aes(x = 28, xend = 55.5, y = 60, yend = 60), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  geom_segment(aes(x = 28, xend = 55.5, y = 40, yend = 40), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = "Study Cohort Selection (CONSORT)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5, margin = margin(b = 2.5, unit = "mm")),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2, "mm")
  )

# 2. Panel b: Love Plot
bal_df <- fread("results/tableS2_iptw_balance.csv")
love_data <- copy(bal_df[!Variable %in% c("distance", "prop.score")])
love_data[, Abs_Diff_Un := abs(Diff.Un)]
setorder(love_data, -Abs_Diff_Un)
top_vars <- head(love_data, 15)

var_labels <- c(
  "Chemo_Yes" = "Chemotherapy received",
  "Subsite_Right colon" = "Right colon subsite",
  "Race_Non-Hispanic White" = "Non-Hispanic White",
  "Radiation_Yes" = "Radiation received",
  "Race_Hispanic" = "Hispanic ethnicity",
  "Stage_Stage IV" = "AJCC Stage IV",
  "LN_Examined_>=12 nodes" = ">=12 LNs examined",
  "Subsite_Rectum" = "Rectum subsite",
  "LN_Examined_<12 nodes" = "<12 LNs examined",
  "Stage_Stage II" = "AJCC Stage II",
  "Stage_Stage I" = "AJCC Stage I",
  "Stage_Stage III" = "AJCC Stage III",
  "Subsite_Left colon" = "Left colon subsite",
  "Year_Group_2016-2022" = "Diagnosis 2016-2022",
  "Year_Group_2000-2005" = "Diagnosis 2000-2005"
)
top_vars[, Clean_Var := var_labels[Variable]]
top_vars[is.na(Clean_Var), Clean_Var := Variable]

love_long <- melt(
  top_vars,
  id.vars = "Clean_Var",
  measure.vars = c("Diff.Un", "Diff.Adj"),
  variable.name = "Cohort_Weight",
  value.name = "SMD"
)
love_long[, Cohort_Weight := factor(ifelse(Cohort_Weight == "Diff.Un", "Unadjusted", "IPTW-Adjusted"), levels = c("Unadjusted", "IPTW-Adjusted"))]
love_long[, Abs_SMD := abs(SMD)]
love_long[, Clean_Var := factor(Clean_Var, levels = rev(top_vars$Clean_Var))]

p1b <- ggplot(love_long, aes(x = Abs_SMD, y = Clean_Var, color = Cohort_Weight, shape = Cohort_Weight)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_vline(xintercept = 0.10, linetype = "dotted", color = "gray60", linewidth = 0.3) +
  geom_point(size = 2.2, stroke = 0.5) +
  scale_color_manual(values = c("Unadjusted" = "#C4615A", "IPTW-Adjusted" = "#384D7A"), name = "Sample") +
  scale_shape_manual(values = c("Unadjusted" = 16, "IPTW-Adjusted" = 17), name = "Sample") +
  scale_x_continuous(
    limits = c(0, 0.30),
    breaks = seq(0, 0.30, by = 0.05),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(x = "Absolute Standardized Mean Difference (SMD)", y = NULL, title = "Covariate Balance (Love Plot)") +
  annotate("text", x = 0.055, y = 14.8, label = "Threshold: SMD < 0.05", size = 2.45, color = "#D9534F", hjust = 0, fontface = "italic") +
  theme_mc(width_mm = 90) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 7.2, color = "black"),
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 2, r = 4, b = 2, l = 2, "mm")
  )

fig1 <- mc_compose(p1a, p1b, ncol = 2, widths = c(1, 1.05), width_mm = 180, height_mm = 110)
mc_save(fig1, "figures/fig1_consort_iptw.png", width_mm = 180, height_mm = 110)
cat("Figure 1 re-drawn and saved successfully!\n")

# [2026-10-24 10:03:46]
source("scripts/mc_figure.R")
library(data.table)
library(ggplot2)
library(patchwork)

# 1. Panel a: CONSORT Flowchart
# Coordinate system 0 to 100
# Main boxes: center at x = 28, w = 53 (from 1.5 to 54.5)
# Exclusion boxes: center at x = 77, w = 44 (from 55.0 to 99.0)

consort_boxes <- data.frame(
  x = c(28, 28, 28, 28, 28),
  y = c(90, 70, 50, 30, 9.5),
  w = c(53, 53, 53, 53, 53),
  h = c(13.5, 11.5, 11.5, 11.5, 15.5),
  label = c(
    "SEER CRC (2000-2022)\n654,149 records\n(631,157 patients)",
    "First Primary CRC\nN = 530,027",
    "Survival Time > 0 mo\nN = 504,397",
    "Colorectal Adenocarcinoma\nN = 466,424",
    "Full Analysis Cohort\nTotal N = 466,424\nEarly: 60,819 | Late: 405,605"
  )
)

excl_boxes <- data.frame(
  x = c(77, 77, 77),
  y = c(80, 60, 40),
  w = c(44, 44, 44),
  h = c(11, 11, 11),
  label = c(
    "Excluded prior cancer\n(n = 124,122)",
    "Excluded survival <= 0\n(n = 25,630)",
    "Non-adenocarcinoma\n(n = 37,973)"
  )
)

p1a <- ggplot() +
  # 主流程方框 (浅蓝底、深蓝描边、圆角)
  geom_rect(data = consort_boxes, aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#F8FAFC", color = "#384D7A", linewidth = 0.5) +
  geom_text(data = consort_boxes, aes(x = x, y = y, label = label),
            size = 2.55, lineheight = 1.05, color = "#1E293B", fontface = "plain") +
  # 排除分支方框 (浅红底、红描边)
  geom_rect(data = excl_boxes, aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#FEF2F2", color = "#C4615A", linewidth = 0.45) +
  geom_text(data = excl_boxes, aes(x = x, y = y, label = label),
            size = 2.55, lineheight = 1.05, color = "#991B1B") +
  # 垂直主干箭头 (从上一框底连至下一框顶)
  geom_segment(aes(x = 28, xend = 28, y = 83.25, yend = 76.5), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 28, xend = 28, y = 64.25, yend = 56.5), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 28, xend = 28, y = 44.25, yend = 36.5), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 28, xend = 28, y = 24.25, yend = 18.0), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  # 水平分支箭头 (从主干 x=28 连至排除框左边缘 x=55)
  geom_segment(aes(x = 28, xend = 54.5, y = 80, yend = 80), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  geom_segment(aes(x = 28, xend = 54.5, y = 60, yend = 60), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  geom_segment(aes(x = 28, xend = 54.5, y = 40, yend = 40), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = "Study Cohort Selection (CONSORT)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5, margin = margin(b = 2.5, unit = "mm")),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2, "mm")
  )

# 2. Panel b: Love Plot
bal_df <- fread("results/tableS2_iptw_balance.csv")
love_data <- copy(bal_df[!Variable %in% c("distance", "prop.score")])
love_data[, Abs_Diff_Un := abs(Diff.Un)]
setorder(love_data, -Abs_Diff_Un)
top_vars <- head(love_data, 15)

var_labels <- c(
  "Chemo_Yes" = "Chemotherapy received",
  "Subsite_Right colon" = "Right colon subsite",
  "Race_Non-Hispanic White" = "Non-Hispanic White",
  "Radiation_Yes" = "Radiation received",
  "Race_Hispanic" = "Hispanic ethnicity",
  "Stage_Stage IV" = "AJCC Stage IV",
  "LN_Examined_>=12 nodes" = ">=12 LNs examined",
  "Subsite_Rectum" = "Rectum subsite",
  "LN_Examined_<12 nodes" = "<12 LNs examined",
  "Stage_Stage II" = "AJCC Stage II",
  "Stage_Stage I" = "AJCC Stage I",
  "Stage_Stage III" = "AJCC Stage III",
  "Subsite_Left colon" = "Left colon subsite",
  "Year_Group_2016-2022" = "Diagnosis 2016-2022",
  "Year_Group_2000-2005" = "Diagnosis 2000-2005"
)
top_vars[, Clean_Var := var_labels[Variable]]
top_vars[is.na(Clean_Var), Clean_Var := Variable]

love_long <- melt(
  top_vars,
  id.vars = "Clean_Var",
  measure.vars = c("Diff.Un", "Diff.Adj"),
  variable.name = "Cohort_Weight",
  value.name = "SMD"
)
love_long[, Cohort_Weight := factor(ifelse(Cohort_Weight == "Diff.Un", "Unadjusted", "IPTW-Adjusted"), levels = c("Unadjusted", "IPTW-Adjusted"))]
love_long[, Abs_SMD := abs(SMD)]
love_long[, Clean_Var := factor(Clean_Var, levels = rev(top_vars$Clean_Var))]

p1b <- ggplot(love_long, aes(x = Abs_SMD, y = Clean_Var, color = Cohort_Weight, shape = Cohort_Weight)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_vline(xintercept = 0.10, linetype = "dotted", color = "gray60", linewidth = 0.3) +
  geom_point(size = 2.2, stroke = 0.5) +
  scale_color_manual(values = c("Unadjusted" = "#C4615A", "IPTW-Adjusted" = "#384D7A"), name = "Sample") +
  scale_shape_manual(values = c("Unadjusted" = 16, "IPTW-Adjusted" = 17), name = "Sample") +
  scale_x_continuous(
    limits = c(0, 0.30),
    breaks = seq(0, 0.30, by = 0.05),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  labs(x = "Absolute Standardized Mean Difference (SMD)", y = NULL, title = "Covariate Balance (Love Plot)") +
  annotate("text", x = 0.055, y = 14.8, label = "Threshold: SMD < 0.05", size = 2.55, color = "#D9534F", hjust = 0, fontface = "italic") +
  theme_mc(width_mm = 90) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 7.2, color = "black"),
    plot.title = element_text(size = 8.5, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 2, r = 4, b = 2, l = 2, "mm")
  )

fig1 <- mc_compose(p1a, p1b, ncol = 2, widths = c(1, 1.05), width_mm = 180, height_mm = 110)
mc_save(fig1, "figures/fig1_consort_iptw.png", width_mm = 180, height_mm = 110)
cat("Figure 1 re-drawn and saved successfully!\n")

# [2026-10-24 10:04:55]
p1b_test <- p1b + theme(plot.margin = margin(t = 2, r = 3, b = 2, l = 1, "mm"))
fig1_test2 <- mc_compose(p1a, p1b_test, ncol = 2, widths = c(1, 1.15), width_mm = 180, height_mm = 110)
mc_save(fig1_test2, "figures/fig1_consort_iptw.png", width_mm = 180, height_mm = 110)
cat("Test 2 saved successfully!\n")

# [2026-10-24 10:05:33]
# Let's inspect the gtable of fig1_test2
library(ggplot2)
library(patchwork)
gt <- ggplotGrob(fig1_test2)
print("gtable widths:")
print(gt$widths)

# [2026-10-24 10:06:02]
fig1_wrapped <- mc_compose(wrap_elements(full = p1a), p1b, ncol = 2, widths = c(1, 1), width_mm = 180, height_mm = 110)
mc_save(fig1_wrapped, "figures/fig1_consort_iptw.png", width_mm = 180, height_mm = 110)
cat("Wrapped composition saved successfully!\n")

# [2026-10-24 11:58:33]
source("scripts/mc_figure.R")
library(data.table)
library(ggplot2)
library(patchwork)

# 1. Panel a: CONSORT Flowchart
# Coordinate system 0 to 100 on X and Y
# In a 105mm wide panel:
# Main column center at x = 27, width = 50 (from 2 to 52)
# Gap: 52 to 58 (width 6)
# Exclusion column center at x = 78, width = 40 (from 58 to 98)

consort_boxes <- data.frame(
  x = c(27, 27, 27, 27, 27),
  y = c(90, 70, 50, 30, 9.5),
  w = c(50, 50, 50, 50, 50),
  h = c(13.5, 11.5, 11.5, 11.5, 15.5),
  label = c(
    "SEER CRC (2000-2022)\n654,149 records\n(631,157 patients)",
    "First Primary CRC\nN = 530,027",
    "Survival Time > 0 mo\nN = 504,397",
    "Colorectal Adenocarcinoma\nN = 466,424",
    "Full Analysis Cohort\nTotal N = 466,424\nEarly: 60,819 | Late: 405,605"
  )
)

excl_boxes <- data.frame(
  x = c(78, 78, 78),
  y = c(80, 60, 40),
  w = c(40, 40, 40),
  h = c(11, 11, 11),
  label = c(
    "Excluded prior cancer\n(n = 124,122)",
    "Excluded survival <= 0\n(n = 25,630)",
    "Non-adenocarcinoma\n(n = 37,973)"
  )
)

p1a <- ggplot() +
  # 主流程方框
  geom_rect(data = consort_boxes, aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#F8FAFC", color = "#384D7A", linewidth = 0.5) +
  geom_text(data = consort_boxes, aes(x = x, y = y, label = label),
            size = 2.55, lineheight = 1.05, color = "#1E293B", fontface = "plain") +
  # 排除分支方框
  geom_rect(data = excl_boxes, aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
            fill = "#FEF2F2", color = "#C4615A", linewidth = 0.45) +
  geom_text(data = excl_boxes, aes(x = x, y = y, label = label),
            size = 2.55, lineheight = 1.05, color = "#991B1B") +
  # 垂直主干箭头
  geom_segment(aes(x = 27, xend = 27, y = 83.25, yend = 76.5), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 27, xend = 27, y = 64.25, yend = 56.5), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 27, xend = 27, y = 44.25, yend = 36.5), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  geom_segment(aes(x = 27, xend = 27, y = 24.25, yend = 18.0), arrow = arrow(length = unit(1.8, "mm")), linewidth = 0.45, color = "#384D7A") +
  # 水平分支箭头
  geom_segment(aes(x = 27, xend = 57.5, y = 80, yend = 80), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  geom_segment(aes(x = 27, xend = 57.5, y = 60, yend = 60), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  geom_segment(aes(x = 27, xend = 57.5, y = 40, yend = 40), arrow = arrow(length = unit(1.6, "mm")), linewidth = 0.4, color = "#C4615A") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(title = "Study Cohort Selection (CONSORT)") +
  theme_void() +
  theme(
    plot.title = element_text(size = 8.8, face = "bold", hjust = 0.5, margin = margin(b = 2.5, unit = "mm")),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2, "mm")
  )

# 2. Panel b: Love Plot
bal_df <- fread("results/tableS2_iptw_balance.csv")
love_data <- copy(bal_df[!Variable %in% c("distance", "prop.score")])
love_data[, Abs_Diff_Un := abs(Diff.Un)]
setorder(love_data, -Abs_Diff_Un)
top_vars <- head(love_data, 15)

var_labels <- c(
  "Chemo_Yes" = "Chemotherapy received",
  "Subsite_Right colon" = "Right colon subsite",
  "Race_Non-Hispanic White" = "Non-Hispanic White",
  "Radiation_Yes" = "Radiation received",
  "Race_Hispanic" = "Hispanic ethnicity",
  "Stage_Stage IV" = "AJCC Stage IV",
  "LN_Examined_>=12 nodes" = ">=12 LNs examined",
  "Subsite_Rectum" = "Rectum subsite",
  "LN_Examined_<12 nodes" = "<12 LNs examined",
  "Stage_Stage II" = "AJCC Stage II",
  "Stage_Stage I" = "AJCC Stage I",
  "Stage_Stage III" = "AJCC Stage III",
  "Subsite_Left colon" = "Left colon subsite",
  "Year_Group_2016-2022" = "Diagnosis 2016-2022",
  "Year_Group_2000-2005" = "Diagnosis 2000-2005"
)
top_vars[, Clean_Var := var_labels[Variable]]
top_vars[is.na(Clean_Var), Clean_Var := Variable]

love_long <- melt(
  top_vars,
  id.vars = "Clean_Var",
  measure.vars = c("Diff.Un", "Diff.Adj"),
  variable.name = "Cohort_Weight",
  value.name = "SMD"
)
love_long[, Cohort_Weight := factor(ifelse(Cohort_Weight == "Diff.Un", "Unadjusted", "IPTW-Adjusted"), levels = c("Unadjusted", "IPTW-Adjusted"))]
love_long[, Abs_SMD := abs(SMD)]
love_long[, Clean_Var := factor(Clean_Var, levels = rev(top_vars$Clean_Var))]

p1b <- ggplot(love_long, aes(x = Abs_SMD, y = Clean_Var, color = Cohort_Weight, shape = Cohort_Weight)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_vline(xintercept = 0.10, linetype = "dotted", color = "gray60", linewidth = 0.3) +
  geom_point(size = 2.1, stroke = 0.5) +
  scale_color_manual(values = c("Unadjusted" = "#C4615A", "IPTW-Adjusted" = "#384D7A"), name = "Sample") +
  scale_shape_manual(values = c("Unadjusted" = 16, "IPTW-Adjusted" = 17), name = "Sample") +
  # x 轴上限设为 0.28，让最大值 0.262 贴近右边界，彻底消除右侧大面积留白！
  scale_x_continuous(
    limits = c(0, 0.28),
    breaks = seq(0, 0.25, by = 0.05),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  labs(x = "Absolute Standardized Mean Difference (SMD)", y = NULL, title = "Covariate Balance (Love Plot)") +
  annotate("text", x = 0.055, y = 14.8, label = "Threshold: SMD < 0.05", size = 2.45, color = "#D9534F", hjust = 0, fontface = "italic") +
  theme_mc(width_mm = 75) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    axis.text.y = element_text(size = 7.0, color = "black"),
    plot.title = element_text(size = 8.8, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 1, "mm")
  )

# 采用 widths = c(1.4, 1.0) 权重组合，Panel a 占 ~105mm，Panel b 占 ~75mm
fig1_opt <- mc_compose(wrap_elements(full = p1a), p1b, ncol = 2, widths = c(1.4, 1.0), width_mm = 180, height_mm = 110)
mc_save(fig1_opt, "figures/fig1_consort_iptw.png", width_mm = 180, height_mm = 110)
cat("Optimized Figure 1 saved!\n")

# [2026-10-25 12:21:10]
if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown", repos = "https://cloud.r-project.org")
}

output_path <- rmarkdown::render(
  input = "manuscript/main.md",
  output_format = rmarkdown::word_document(),
  output_file = "main.docx"
)

cat("Successfully rendered to:", output_path, "\n")
file_info <- file.info("manuscript/main.docx")
cat("File size (bytes):", file_info$size, "\n")

# [2026-10-25 12:39:51]
output_path <- rmarkdown::render(
  input = "manuscript/main.md",
  output_format = rmarkdown::word_document(),
  output_file = "main.docx"
)
cat("Successfully re-rendered to:", output_path, "\n")
file_info <- file.info("manuscript/main.docx")
cat("File size (bytes):", file_info$size, "\n")

# [2026-10-25 14:14:27]
if (requireNamespace("pdftools", quietly = TRUE)) {
  txt <- pdftools::pdf_text("refs/style/Tislelizumab_induced_psoriasis_in_a_patient_with_hepatocellular_carcinoma_A_case_report.pdf")
  cat("pdftools worked! Pages:", length(txt), "\n")
  cat("Page 1 preview:\n", substr(txt[1], 1, 500), "\n")
} else {
  cat("pdftools not available\n")
}

# [2026-10-25 14:14:41]
library(pdftools)
library(stringr)

txt_pages <- pdftools::pdf_text("refs/style/Tislelizumab_induced_psoriasis_in_a_patient_with_hepatocellular_carcinoma_A_case_report.pdf")
full_text <- paste(txt_pages, collapse = "\n\n")

# 去除参考文献部分，避免参考文献标题和作者干扰正文句法统计
text_no_ref <- str_split(full_text, "References\n|REFERENCES\n")[[1]][1]

# 分句（以句号、问号、叹号后跟空格或换行作为分界，处理缩写如 e.g., i.e., Fig., vs. 等）
clean_t <- gsub("(?:[A-Z]\\.|e\\.g\\.|i\\.e\\.|Fig\\.|vs\\.|Dr\\.|al\\.)", "abbr", text_no_ref)
sentences <- unlist(str_extract_all(clean_t, "[A-Z][^.!?]*[.!?]"))
sentences <- str_trim(gsub("\n", " ", sentences))
sentences <- sentences[nchar(sentences) > 15]

# 统计词数
word_counts <- vapply(sentences, function(s) length(str_split(str_trim(s), "\\s+")[[1]]), integer(1))

cat("Total sentences:", length(word_counts), "\n")
cat("Mean words per sentence:", round(mean(word_counts), 1), "\n")
cat("SD words per sentence:", round(sd(word_counts), 1), "\n")
cat("Min words:", min(word_counts), "Max words:", max(word_counts), "\n")

# 检查连接词
connectives_check <- c("Moreover", "Furthermore", "Additionally", "In addition", "However", 
                       "Although", "In contrast", "Nevertheless", "Therefore", "Thus", 
                       "Consequently", "Notably", "Specifically", "Meanwhile")
conn_counts <- vapply(connectives_check, function(w) {
  sum(str_count(text_no_ref, regex(paste0("\\b", w, "\\b"), ignore_case = TRUE)))
}, integer(1))

print(conn_counts)

# 检查人称 (We, our, us)
we_count <- sum(str_count(text_no_ref, regex("\\b(we|our|us)\\b", ignore_case = TRUE)))
cat("First person pronouns count:", we_count, "\n")

# 检查 Hedging
hedging_words <- c("may", "might", "suggest", "suggests", "suggested", "appear", "appears", 
                   "appeared", "likely", "potential", "indicate", "indicates", "indicated")
hedge_counts <- vapply(hedging_words, function(w) {
  sum(str_count(text_no_ref, regex(paste0("\\b", w, "\\b"), ignore_case = TRUE)))
}, integer(1))
print(hedge_counts)

# [2026-10-25 14:14:48]
cat("Sample sentences from Introduction & Discussion:\n")
# 打印几句真实的句子
set.seed(42)
sample_s <- sample(sentences[word_counts > 10 & word_counts < 40], 8)
for (i in seq_along(sample_s)) {
  cat(paste0(i, ". ", sample_s[i], "\n"))
}

# [2026-10-25 14:14:53]
# 提取 Introduction 和 Discussion
intro_match <- str_extract(full_text, "(?s)Introduction.*?Case report")
disc_match <- str_extract(full_text, "(?s)Discussion.*?References")

cat("=== INTRODUCTION PREVIEW ===\n")
cat(substr(intro_match, 1, 1500), "\n\n")

cat("=== DISCUSSION PREVIEW ===\n")
cat(substr(disc_match, 1, 1500), "\n")

# [2026-10-25 14:15:13]
library(jsonlite)

profile <- list(
  samples = list(
    list(
      file = "refs/style/Tislelizumab_induced_psoriasis_in_a_patient_with_hepatocellular_carcinoma_A_case_report.pdf",
      note = "Experimental and Therapeutic Medicine 32: 319, 2026; 第一作者/通讯作者临床论著，真实代表作者团队原生学术写作风格"
    )
  ),
  analyzedAt = "2026-10-25",
  sentence = list(
    meanWords = 19.7,
    sdWords = 13.9,
    min = 8,
    max = 48,
    note = "长短句交错极为鲜明：关键发现与转折用10-15词精炼短句直给，机制展开用25-35词复合句，完全避免匀速22词的AI机器节奏"
  ),
  connectives = list(
    used = c("However", "Although", "Thus", "Therefore", "Nevertheless", "First", "Second"),
    avoided = c("Moreover", "Furthermore", "Additionally", "In addition", "Consequently"),
    position = "极少在段首直接堆砌副词，转折多在句首用 However/Although，逻辑因果常用介词短语(To minimize..., Because...)或句中连接，段首绝不连续使用递进副词"
  ),
  tenseVoice = list(
    background = "现在时陈述既有医学事实与生物学机制",
    observationMethods = "过去时为主，客观被动句与主动动作交错(was discontinued, developed, supported, received)",
    discussion = "现在时陈述普遍规律，过去时述本病例/本队列结果，推测使用情态动词"
  ),
  person = "第三人称客观叙述为主，全文极少出现第一人称(无密集we performed)，主语多为研究对象、干预措施或具体生物学实体",
  hedging = list(
    level = "克制且严密",
    words = c("may", "suggest", "indicates", "potential", "likely"),
    where = "仅出现在Discussion解释机制或权衡归因处，事实陈述段落一律客观坚决、不滥用hedging"
  ),
  terminology = list(
    list(concept = "证据与论述", use = "Several lines of evidence suggest/indicate", avoid = "plays a vital/crucial role"),
    list(concept = "研究对象", use = "patients / individuals", avoid = "subjects / cases (作为主语)"),
    list(concept = "使用/采取", use = "used / received / administered", avoid = "utilized / leveraged"),
    list(concept = "表现与呈现", use = "developed / demonstrated / showed", avoid = "sheds light on / showcases")
  ),
  paragraph = list(
    sentencesPerPara = "4-7句，长短不一",
    opening = "段首直接给出核心事实或结论，杜绝'It is worth noting that'或'It is well known that'等空心铺垫"
  ),
  sectionPatterns = list(
    Discussion = "直陈本研究核心表现与事实 -> 排除竞争性假说(Several lines of evidence argue against...) -> 机制与文献对照 -> 临床处置建议与总结"
  ),
  notes = "单篇高水准医学案例及论述分析，完全契合用户自身发表习惯；润色严格限制在专业词汇与句式骨架，坚决杜绝搬运原句(连续重合<=8词)且事实数据零变动"
)

write_json(profile, ".medcortex/style_profile.json", auto_unbox = TRUE, pretty = TRUE)
cat("style_profile.json created successfully.\n")

# [2026-10-25 15:35:45]
cat("Pandoc available:", rmarkdown::pandoc_available(), "\n")
cat("Pandoc version:", as.character(rmarkdown::pandoc_version()), "\n")

rmarkdown::render(
  input = "manuscript/main.md",
  output_format = rmarkdown::word_document(),
  output_file = "main.docx",
  output_dir = "manuscript",
  quiet = FALSE
)
cat("Word document generated successfully!\n")

# [2026-10-26 12:09:47]
rmarkdown::render(
  input = "manuscript/main.md",
  output_format = rmarkdown::word_document(),
  output_file = "main.docx",
  output_dir = "manuscript",
  quiet = FALSE
)
cat("Word document successfully re-compiled!\n")

# [2026-10-26 12:51:28]
csl_home <- path.expand("~/.medcortex/csl")
cat("CSL home exists:", dir.exists(csl_home), "\n")
if (dir.exists(csl_home)) {
  print(list.files(csl_home, pattern = "springer|vancouver"))
}

# [2026-10-26 13:03:00]
dir.create("submission/snapshots", recursive = TRUE, showWarnings = FALSE)
dest_path <- "submission/snapshots/main-ijcd-20261026.md"
success <- file.copy("manuscript/main.md", dest_path, overwrite = FALSE)
cat("Snapshot created:", success, "\n")
cat("Snapshot file size:", file.size(dest_path), "bytes\n")

# [2026-10-29 12:47:22]
suppressPackageStartupMessages({library(data.table)})
df <- readRDS("data/cleaned_cohort_full.rds")
cat("class:", class(df)[1], " dim:", dim(df), "\n")
cat("key vars present:\n")
print(intersect(c("Patient_ID","Age_Num","Age_Group","EOCRC","Stage","Subsite","Histology","Grade","Surgery","Chemo","Radiation","LN_Examined","Survival_Months","OS_Status","Comp_Risk_Status","Year_DX","Sex","Race","CEA"), names(df)))
cat("\nAge missing (original Age_Num):", sum(is.na(df$Age_Num)), "\n")
cat("Age distribution:\n"); print(summary(df$Age_Num))
cat("\nStage:\n"); print(table(df$Stage, useNA="ifany"))
cat("\nAge_Group:\n"); print(table(df$Age_Group, useNA="ifany"))
cat("\nYear range:", range(df$Year_DX, na.rm=TRUE), "\n")
cat("\nOS events:", sum(df$OS_Status==1), " CCSM:", sum(df$Comp_Risk_Status==1), " NCSM:", sum(df$Comp_Risk_Status==2), "\n")
w <- readRDS("data/cohort_full_weighted.rds")
cat("\nweighted cohort dim:", dim(w), "\n")
cat("weight vars:", intersect(c("iptw_w","ps","Age_Group"), names(w)), "\n")
if ("iptw_w" %in% names(w)) print(summary(w$iptw_w))

# [2026-10-29 12:47:32]
cat("Age <18:", sum(df$Age_Num < 18, na.rm=TRUE), " Age 18-49:", sum(df$Age_Num>=18 & df$Age_Num<50, na.rm=TRUE), "\n")
cat("Age <18 by stage:\n"); print(table(df$Stage[df$Age_Num<18], useNA="ifany"))
cat("\nStage I-III by age group:\n")
print(table(df$Age_Group, df$Stage %in% c("Stage I","Stage II","Stage III"), useNA="ifany"))
cat("\nStage I-III counts by age group:\n")
print(table(df$Age_Group[df$Stage %in% c("Stage I","Stage II","Stage III")], useNA="ifany"))
cat("\nYear distribution:\n"); print(table(cut(df$Year_DX, breaks=c(1999,2009,2019,2022), labels=c("2000-2009","2010-2019","2020-2022"))))
cat("\nAge 0-17 survival events:", sum(df$OS_Status[df$Age_Num<18]), "\n")

# [2026-10-29 13:11:15]
suppressPackageStartupMessages({library(data.table); library(splines)})
df <- readRDS("data/cleaned_cohort_modern_v2.rds")
df[, Year_Group := fcase(Year_DX <= 2005, "2000-2005", Year_DX <= 2010, "2006-2010", Year_DX <= 2015, "2011-2015", default = "2016-2022")]
df[, Year_Group := factor(Year_Group, levels = c("2000-2005","2006-2010","2011-2015","2016-2022"))]
df[, CEA := factor(CEA, levels = c("Normal","Elevated","Unknown/Not documented"))]
covars <- c("Sex","Race","Year_Group","Subsite","Histology","Grade","CEA")
v <- "Any_Mets"
fml <- as.formula(paste0(v, " ~ ns(Age_Num, df = 4) + ", paste(covars, collapse=" + ")))
cat("formula:", deparse(fml), "\n")
fit <- tryCatch(glm(fml, data=df, family=binomial, maxit=50), error=function(e){cat("FIT ERROR:", conditionMessage(e), "\n"); NULL})
if (!is.null(fit)) {
  cat("fit ok; coefficients:\n"); print(head(coef(fit)))
  ref_cov <- data.frame(lapply(df[, ..covars], function(x) if (is.factor(x)) factor(levels(x)[1], levels=levels(x)) else median(x, na.rm=TRUE)))
  names(ref_cov) <- covars
  newd <- cbind(data.frame(Age_Num=c(30,50,70)), ref_cov[rep(1,3),,drop=FALSE])
  cat("newd:\n"); print(newd)
  X <- tryCatch(model.matrix(fml, data=newd), error=function(e){cat("MM ERROR:", conditionMessage(e), "\n"); NULL})
  if (!is.null(X)) { cat("model matrix dim:", dim(X), "\n"); print(colnames(X)) }
}

# [2026-10-29 13:11:27]
suppressPackageStartupMessages({library(data.table); library(splines)})
df <- readRDS("data/cleaned_cohort_modern_v2.rds")
invisible(df[, Year_Group := fcase(Year_DX <= 2005, "2000-2005", Year_DX <= 2010, "2006-2010", Year_DX <= 2015, "2011-2015", default = "2016-2022")])
invisible(df[, Year_Group := factor(Year_Group, levels = c("2000-2005","2006-2010","2011-2015","2016-2022"))])
invisible(df[, CEA := factor(CEA, levels = c("Normal","Elevated","Unknown/Not documented"))])
covars <- c("Sex","Race","Year_Group","Subsite","Histology","Grade","CEA")
v <- "Any_Mets"
fml <- as.formula(paste0(v, " ~ ns(Age_Num, df = 4) + ", paste(covars, collapse=" + ")))
cat("formula:", deparse(fml), "\n")
fit <- tryCatch(glm(fml, data=df, family=binomial, maxit=50), error=function(e) e)
cat("class:", class(fit)[1], "\n")
if (inherits(fit, "error")) { cat("ERROR:", conditionMessage(fit), "\n") } else {
  cat("converged:", fit$converged, " coefs:", length(coef(fit)), "\n")
  ref_cov <- data.frame(lapply(df[, ..covars], function(x) if (is.factor(x)) factor(levels(x)[1], levels=levels(x)) else median(x, na.rm=TRUE)))
  names(ref_cov) <- covars
  newd <- cbind(data.frame(Age_Num=c(30,50,70)), ref_cov[rep(1,3),,drop=FALSE])
  X <- tryCatch(model.matrix(fml, data=newd), error=function(e) e)
  cat("model.matrix class:", class(X)[1], "\n")
  if (inherits(X, "error")) cat("MM ERROR:", conditionMessage(X), "\n") else cat("MM dim:", paste(dim(X), collapse="x"), "\n")
}

# [2026-10-29 13:11:58]
suppressPackageStartupMessages({library(data.table); library(splines)})
df <- readRDS("data/cleaned_cohort_modern_v2.rds")
invisible(df[, Year_Group := factor(fcase(Year_DX <= 2005, "2000-2005", Year_DX <= 2010, "2006-2010", Year_DX <= 2015, "2011-2015", default = "2016-2022"), levels=c("2000-2005","2006-2010","2011-2015","2016-2022"))])
invisible(df[, CEA := factor(CEA, levels = c("Normal","Elevated","Unknown/Not documented"))])
covars <- c("Sex","Race","Year_Group","Subsite","Histology","Grade","CEA")
fml <- as.formula(paste0("Any_Mets ~ ns(Age_Num, df = 4) + ", paste(covars, collapse=" + ")))
fit <- glm(fml, data=df, family=binomial, maxit=50)
ref_cov <- data.frame(lapply(df[, ..covars], function(x) if (is.factor(x)) factor(levels(x)[1], levels=levels(x)) else median(x, na.rm=TRUE)))
names(ref_cov) <- covars
newd <- cbind(data.frame(Age_Num=c(30,50,70)), ref_cov[rep(1,3),,drop=FALSE])
refd <- cbind(data.frame(Age_Num=50), ref_cov)
X <- model.matrix(delete.response(terms(fit)), data=newd)
Xref <- model.matrix(delete.response(terms(fit)), data=refd)
cat("X dim:", paste(dim(X),collapse="x"), " Xref:", paste(dim(Xref),collapse="x"), " coef:", length(coef(fit)), "\n")
cat("X cols not in coef:", setdiff(colnames(X), names(coef(fit))), "\n")
cat("coef names not in X:", setdiff(names(coef(fit)), colnames(X)), "\n")
beta <- coef(fit); common <- intersect(colnames(X), names(beta))
common <- common[!is.na(beta[common])]
L <- X[, common, drop=FALSE] - matrix(rep(Xref[, common, drop=FALSE], each=nrow(X)), nrow=nrow(X))
V <- vcov(fit)[common, common, drop=FALSE]
logor <- as.vector(L %*% beta[common]); se <- sqrt(rowSums((L %*% V) * L))
cat("OR:", paste(round(exp(logor),3), collapse=", "), "\n")
cat("95CI:", paste(round(exp(logor-1.96*se),3), round(exp(logor+1.96*se),3), collapse=" / "), "\n")

# [2026-10-29 13:25:13]
suppressPackageStartupMessages({library(data.table); library(survival); library(splines)})
df <- readRDS("data/cohort_iii_weighted_v2.rds")
covars <- c("Sex","Race","Year_Group","Subsite","Histology","Grade","Stage","Surgery","Chemo","Radiation","LN_Examined")
cf <- paste(covars, collapse=" + ")
f_rcs <- as.formula(paste("Surv(Survival_Months, OS_Status) ~ ns(Age_Num, df=4) +", cf))
f_lin <- as.formula(paste("Surv(Survival_Months, OS_Status) ~ Age_Num +", cf))
f_no  <- as.formula(paste("Surv(Survival_Months, OS_Status) ~", cf))
m_rcs <- coxph(f_rcs, data=df, weights=iptw_w, robust=TRUE, x=TRUE)
m_lin <- coxph(f_lin, data=df, weights=iptw_w, robust=TRUE, x=TRUE)
m_no  <- coxph(f_no, data=df, weights=iptw_w, robust=TRUE, x=TRUE)
a1 <- tryCatch(anova(m_no, m_rcs, test="Chisq"), error=function(e) e)
cat("class a1:", class(a1)[1], "\n"); print(a1)
a2 <- tryCatch(anova(m_lin, m_rcs, test="Chisq"), error=function(e) e)
cat("class a2:", class(a2)[1], "\n"); print(a2)

# [2026-10-29 13:31:39]
suppressPackageStartupMessages({library(data.table); library(survival); library(splines)})
df <- readRDS("data/cohort_iii_weighted_v2.rds")
covars <- c("Sex","Race","Year_Group","Subsite","Histology","Grade","Stage","Surgery","Chemo","Radiation","LN_Examined")
cf <- paste(covars, collapse=" + ")
fml <- as.formula(paste("Surv(Survival_Months, OS_Status) ~ ns(Age_Num, df=4) +", cf))
fit <- coxph(fml, data=df, weights=iptw_w, robust=TRUE, x=TRUE)
b <- coef(fit); V <- vcov(fit); idx <- grep("^ns\\(Age_Num", names(b))
cat("age terms:", length(idx), " names:", paste(names(b)[idx], collapse=" | "), "\n")
wald <- function(ix){ix<-ix[!is.na(b[ix])]; W<-as.numeric(t(b[ix])%*%solve(V[ix,ix,drop=FALSE])%*%b[ix]); c(W=W, df=length(ix), P=pchisq(W,length(ix),lower.tail=FALSE))}
print(wald(idx)); print(wald(idx[-1]))

# [2026-10-29 13:36:16]
suppressPackageStartupMessages({library(data.table); library(survival)})
df <- readRDS("data/cohort_iii_weighted_v2.rds")
set.seed(1); s <- df[sample(.N, 20000)]
s[, cr_f := factor(Comp_Risk_Status, levels=c(0,1,2), labels=c("censor","ccsm","ncsm"))]
fit <- survfit(Surv(Survival_Months, cr_f) ~ Age_Group, data=s, weights=iptw_w)
sm <- summary(fit, times=c(60,120))
cat("strata:\n"); print(sm$strata)
cat("time:\n"); print(sm$time)
cat("pstate:\n"); print(round(sm$pstate,4))
cat("std.err dim:", paste(dim(sm$std.err), collapse="x"), "\n"); print(round(sm$std.err,4))
cat("lower dim:", paste(dim(sm$lower), collapse="x"), "\n")
cat("names sm:", paste(names(sm), collapse=", "), "\n")
# finegray without weights
s[, iptw_copy := iptw_w]
fg <- finegray(Surv(Survival_Months, cr_f) ~ EOCRC + Sex + Stage, data=s, etype="ccsm")
cat("\nfinegray rows:", nrow(fg), " names:", paste(grep("^fg|iptw", names(fg), value=TRUE), collapse=","), "\n")
cat("fgwt summary:\n"); print(summary(fg$fgwt))

# [2026-10-29 13:38:02]
suppressPackageStartupMessages({library(data.table); library(survival)})
df <- readRDS("data/cohort_iii_weighted_v2.rds")
set.seed(1); s <- df[sample(.N, 5000)]
s[, cr_f := factor(Comp_Risk_Status, levels=c(0,1,2), labels=c("censor","ccsm","ncsm"))]
s[, iptw_copy := iptw_w]
fg <- finegray(Surv(Survival_Months, cr_f) ~ ., data=s, etype="ccsm", weights=iptw_w)
cat("rows:", nrow(fg), "\n")
cat("has Patient_ID:", "Patient_ID" %in% names(fg), " has iptw_copy:", "iptw_copy" %in% names(fg), "\n")
cat("fgwt mean:", mean(fg$fgwt), " iptw mean:", mean(s$iptw_w), " ratio:", mean(fg$fgwt)/mean(s$iptw_w), "\n")
cat("fgwt range:", paste(round(range(fg$fgwt),4), collapse=" - "), "\n")
cat("fg names head:", paste(head(names(fg), 15), collapse=", "), "\n")

# [2026-10-29 16:11:25]
inp <- normalizePath("manuscript/main.md", winslash="/", mustWork=TRUE)
out <- normalizePath("manuscript/main.docx", winslash="/", mustWork=FALSE)
rmarkdown::pandoc_convert(inp, to="docx", output=out)
cat("exists:", file.exists(out), "size:", file.size(out), "\n")

# [2026-10-30 11:14:43]
inp <- normalizePath("manuscript/main.md", winslash="/", mustWork=TRUE)
out <- normalizePath("manuscript/main.docx", winslash="/", mustWork=FALSE)
rmarkdown::pandoc_convert(inp, to="docx", output=out)
cat("docx rebuilt:", file.exists(out), file.size(out), "\n")

# [2026-10-30 11:16:05]
files <- c("cover_letter", "title_page")
for (f in files) {
  inp <- normalizePath(file.path("manuscript/submission", paste0(f, ".md")), winslash="/", mustWork=TRUE)
  out <- normalizePath(file.path("manuscript/submission", paste0(f, ".docx")), winslash="/", mustWork=FALSE)
  rmarkdown::pandoc_convert(inp, to="docx", output=out)
  cat(f, "->", file.exists(out), file.size(out), "\n")
}

# [2026-10-30 11:27:02]
files <- c("cover_letter", "title_page")
for (f in files) {
  inp <- normalizePath(file.path("manuscript/submission", paste0(f, ".md")), winslash="/", mustWork=TRUE)
  out <- normalizePath(file.path("manuscript/submission", paste0(f, ".docx")), winslash="/", mustWork=FALSE)
  rmarkdown::pandoc_convert(inp, to="docx", output=out)
}
inp <- normalizePath("manuscript/main.md", winslash="/", mustWork=TRUE)
out <- normalizePath("manuscript/main.docx", winslash="/", mustWork=FALSE)
rmarkdown::pandoc_convert(inp, to="docx", output=out)
cat("main.docx", file.size(out), "| cover", file.size("manuscript/submission/cover_letter.docx"),
    "| title", file.size("manuscript/submission/title_page.docx"), "\n")

# [2026-10-30 11:30:11]
inp <- normalizePath("manuscript/main.md", winslash="/", mustWork=TRUE)
out <- normalizePath("manuscript/main.docx", winslash="/", mustWork=FALSE)
rmarkdown::pandoc_convert(inp, to="docx", output=out)
cat("main.docx", file.size(out), "\n")

# [2026-10-30 11:41:57]
inp <- normalizePath("manuscript/main.md", winslash="/", mustWork=TRUE)
rmarkdown::pandoc_convert(inp, to="docx", output=normalizePath("manuscript/main.docx", winslash="/", mustWork=FALSE))
inp2 <- normalizePath("manuscript/submission/title_page.md", winslash="/", mustWork=TRUE)
rmarkdown::pandoc_convert(inp2, to="docx", output=normalizePath("manuscript/submission/title_page.docx", winslash="/", mustWork=FALSE))
cat("main:", file.size("manuscript/main.docx"), "| title:", file.size("manuscript/submission/title_page.docx"), "\n")

# [2026-10-30 14:27:40]
source("scripts/23_fig1_v2.R")

# [2026-10-31 02:29:27]
inp <- normalizePath("manuscript/main.md", winslash="/", mustWork=TRUE)
rmarkdown::pandoc_convert(inp, to="docx", output=normalizePath("manuscript/main.docx", winslash="/", mustWork=FALSE))
cat("main.docx", file.size("manuscript/main.docx"), "\n")
