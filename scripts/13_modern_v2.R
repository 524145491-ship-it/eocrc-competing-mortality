# ==============================================================================
# Script: 13_modern_v2.R
# Purpose: v2 S1c - 重建含器官转移字段的现代转移子队列
# Inputs:
#   - data/cleaned_cohort_full_v2.rds (成人 v2 全队列，用于 ID 衔接)
#   - data/cleaned_cohort_modern.rds (旧版现代子队列，含肝/肺/骨/脑转移字段)
# Outputs:
#   - data/cleaned_cohort_modern_v2.rds
#   - results/cohort_counts_modern_v2.csv
# Notes:
#   - 旧版 cleaned_cohort_modern.rds 含 Met_Liver/Met_Lung/Met_Bone/Met_Brain/
#     Met_Organs_Count/Met_Pattern/CEA；v2 全队列文件不含这些列，故必须承接该文件。
#   - 排除 53 例 <18 岁，最终 256,312 例。
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

source("scripts/mc_trace.R")

cat("Loading adult v2 full cohort...\n")
df_full <- readRDS("data/cleaned_cohort_full_v2.rds")
df_full <- mc_checkpoint(df_full, "纳入：S1 成人全队列 (2000-2022)", id = "Patient_ID")

cat("Loading legacy modern cohort with organ-metastasis fields...\n")
df_m <- readRDS("data/cleaned_cohort_modern.rds")
df_m <- mc_checkpoint(df_m, "并入：旧版现代转移子队列 (2010–2022，含器官转移字段)",
                      id = "Patient_ID", side = TRUE)

# 成人年龄限制
df_m <- df_m[!is.na(Age_Num) & Age_Num >= 18]
df_m <- mc_checkpoint(df_m, "纳入：成人现代转移子队列 (2010–2022)", id = "Patient_ID")

stopifnot(all(df_m$Patient_ID %in% df_full$Patient_ID))

# 分期分组
df_m[, Stage_Group := fcase(
  Stage %in% c("Stage I", "Stage II", "Stage III"), "Stage I-III",
  Stage == "Stage IV", "Stage IV",
  default = "Unknown"
)]
df_m[, Stage_Group := factor(Stage_Group, levels = c("Stage I-III", "Stage IV", "Unknown"))]

# 关键衍生变量（与旧版一致）
df_m[, Any_Mets := as.integer(Met_Organs_Count > 0 | Stage == "Stage IV")]
df_m[, Multiple_Mets := as.integer(Met_Organs_Count >= 2)]
df_m[, Isolated_Liver := as.integer(Met_Pattern == "Isolated Liver")]
df_m[, Isolated_Lung := as.integer(Met_Pattern == "Isolated Lung")]

saveRDS(df_m, "data/cleaned_cohort_modern_v2.rds")

counts <- df_m[, .(
  N = .N,
  Any_Mets = sum(Any_Mets),
  Liver = sum(Met_Liver),
  Lung = sum(Met_Lung),
  Bone = sum(Met_Bone),
  Brain = sum(Met_Brain),
  Multiple = sum(Multiple_Mets)
), by = .(Age_Group)]
fwrite(counts, "results/cohort_counts_modern_v2.csv")

cat("\nModern v2 counts:\n"); print(counts)
cat("Modern v2 N =", nrow(df_m), " EOCRC =", sum(df_m$EOCRC == 1), "\n")
cat("S1c completed successfully.\n")
