# ==============================================================================
# Script: 11_cohort_selection_v2.R
# Purpose: v2 深化方案 S1 - 成人年龄限制与主分析人群划分
# Inputs:
#   - data/cleaned_cohort_full.rds (旧版已清洗队列，上游脚本 01_cohort_selection.R)
#   - results/tableS1_attrition.csv (旧版流失表)
# Outputs:
#   - data/cleaned_cohort_full_v2.rds
#   - data/cohort_iii_v2.rds
#   - data/cohort_iv_v2.rds
#   - data/cleaned_cohort_modern_v2.rds
#   - results/tableS1_attrition_v2.csv
#   - results/cohort_counts_v2.csv
# Notes:
#   - 旧版脚本曾把年龄缺失填为 65 岁；本版不再填充，改为排除年龄缺失。
#   - 旧版队列包含 102 例 <18 岁患者；本版按成人研究口径排除并记录。
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

source("scripts/mc_trace.R")

set.seed(20261029)

cat("Loading legacy cleaned cohort...\n")
df <- readRDS("data/cleaned_cohort_full.rds")
df <- mc_checkpoint(df, "纳入：旧版已清洗结直肠癌队列 (2000–2022)", id = "Patient_ID")

n_before <- nrow(df)
n_before_eo <- sum(df$EOCRC == 1, na.rm = TRUE)
n_before_lo <- sum(df$EOCRC == 0, na.rm = TRUE)

# ------------------------------------------------------------------------------
# 1. 成人年龄限制：排除 <18 岁与年龄缺失
# ------------------------------------------------------------------------------
df <- df[!is.na(Age_Num) & Age_Num >= 18]
df <- mc_checkpoint(df, "排除年龄 <18 岁或年龄缺失（成人研究口径）", id = "Patient_ID")

n_after_age <- nrow(df)

# ------------------------------------------------------------------------------
# 2. 分期人群划分
# ------------------------------------------------------------------------------
df[, Stage_Group := fcase(
  Stage %in% c("Stage I", "Stage II", "Stage III"), "Stage I-III",
  Stage == "Stage IV", "Stage IV",
  default = "Unknown"
)]
df[, Stage_Group := factor(Stage_Group, levels = c("Stage I-III", "Stage IV", "Unknown"))]

df_iii <- df[Stage_Group == "Stage I-III"]
df_iv  <- df[Stage_Group == "Stage IV"]
df_unknown <- df[Stage_Group == "Unknown"]

# 现代转移子队列：2010–2022，全部分期（器官转移字段自 2010 年起）
df_modern <- df[Year_DX >= 2010]

# ------------------------------------------------------------------------------
# 3. 保存数据集
# ------------------------------------------------------------------------------
saveRDS(df, "data/cleaned_cohort_full_v2.rds")
saveRDS(df_iii, "data/cohort_iii_v2.rds")
saveRDS(df_iv, "data/cohort_iv_v2.rds")
saveRDS(df_modern, "data/cleaned_cohort_modern_v2.rds")

# ------------------------------------------------------------------------------
# 4. 流失表（在旧版基础上追加成人年龄限制与分期划分）
# ------------------------------------------------------------------------------
att <- fread("results/tableS1_attrition.csv")

# 旧表含 Retention_Pct 列；新行先补齐同名列，避免列数不一致
att <- rbindlist(list(
  att,
  data.frame(
    Step = "Age >= 18 years (adult cohort)",
    Records = n_after_age,
    Patients = uniqueN(df$Patient_ID),
    Excluded_Records = n_before - n_after_age,
    Excluded_Patients = n_before - n_after_age
  ),
  data.frame(
    Step = "Primary survival set: Stage I-III",
    Records = nrow(df_iii),
    Patients = uniqueN(df_iii$Patient_ID),
    Excluded_Records = n_after_age - nrow(df_iii),
    Excluded_Patients = n_after_age - nrow(df_iii)
  ),
  data.frame(
    Step = "Secondary set: Stage IV",
    Records = nrow(df_iv),
    Patients = uniqueN(df_iv$Patient_ID),
    Excluded_Records = 0,
    Excluded_Patients = 0
  ),
  data.frame(
    Step = "Unknown stage (not in main models)",
    Records = nrow(df_unknown),
    Patients = uniqueN(df_unknown$Patient_ID),
    Excluded_Records = 0,
    Excluded_Patients = 0
  ),
  data.frame(
    Step = "Modern metastatic subcohort: 2010-2022 (all stages)",
    Records = nrow(df_modern),
    Patients = uniqueN(df_modern$Patient_ID),
    Excluded_Records = 0,
    Excluded_Patients = 0
  )
), fill = TRUE)

att$Retention_Pct <- round(att$Records / att$Records[1] * 100, 2)
fwrite(att, "results/tableS1_attrition_v2.csv")

# ------------------------------------------------------------------------------
# 5. 关键计数表（供 CONSORT 图与正文核对）
# ------------------------------------------------------------------------------
counts <- rbind(
  data.frame(Set = "Full adult cohort", Group = c("EOCRC", "LOCRC", "Total"),
             N = c(sum(df$EOCRC == 1), sum(df$EOCRC == 0), nrow(df))),
  data.frame(Set = "Stage I-III", Group = c("EOCRC", "LOCRC", "Total"),
             N = c(sum(df_iii$EOCRC == 1), sum(df_iii$EOCRC == 0), nrow(df_iii))),
  data.frame(Set = "Stage IV", Group = c("EOCRC", "LOCRC", "Total"),
             N = c(sum(df_iv$EOCRC == 1), sum(df_iv$EOCRC == 0), nrow(df_iv))),
  data.frame(Set = "Modern 2010-2022", Group = c("EOCRC", "LOCRC", "Total"),
             N = c(sum(df_modern$EOCRC == 1), sum(df_modern$EOCRC == 0), nrow(df_modern)))
)
fwrite(counts, "results/cohort_counts_v2.csv")

cat("\nCohort counts:\n")
print(counts)
cat("\nStage distribution (adult cohort):\n")
print(table(df$Stage, useNA = "ifany"))
cat("\nAge range:", range(df$Age_Num), "\n")
cat("S1 v2 completed successfully.\n")
