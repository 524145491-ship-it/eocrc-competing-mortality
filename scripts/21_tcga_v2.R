# ==============================================================================
# Script: 21_tcga_v2.R
# Purpose: v2 S10 - TCGA COAD/READ 方向性分子印证（探索性）
# Inputs:
#   - 结直肠癌数据/TCGA 结直肠临床数据/结肠腺癌_clinical.csv
#   - 结直肠癌数据/TCGA 结直肠临床数据/直肠腺癌_clinical.csv
#   - 结直肠癌数据/TCGA 结直肠癌数据/结肠癌COAD/tpms01A_log2.txt
#   - 结直肠癌数据/TCGA 结直肠癌数据/直肠癌READ/tpms01A_log2.txt
# Outputs:
#   - results/table6_tcga_m1_v2.csv
#   - results/table6_tcga_scores_v2.csv
#   - results/fig6_tcga_scores_v2.csv
# Notes:
#   - 早发有表达 70 例、早发 M1 约 15 例；只能做方向性印证，不能做早发专属生存验证。
#   - 转录评分: 基因 z 分数均值；线性模型校正分期、部位、性别。
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

source("scripts/mc_trace.R")

set.seed(20261029)

# ------------------------------------------------------------------------------
# 1. 临床数据
# ------------------------------------------------------------------------------
clin1 <- fread("结直肠癌数据/TCGA 结直肠临床数据/结肠腺癌_clinical.csv", na.strings = c("NA", ""))
clin2 <- fread("结直肠癌数据/TCGA 结直肠临床数据/直肠腺癌_clinical.csv", na.strings = c("NA", ""))
clin <- rbind(
  clin1[, .(submitter_id, age_at_index, ajcc_pathologic_stage, ajcc_pathologic_m,
            ajcc_pathologic_t, ajcc_pathologic_n, gender, project)],
  clin2[, .(submitter_id, age_at_index, ajcc_pathologic_stage, ajcc_pathologic_m,
            ajcc_pathologic_t, ajcc_pathologic_n, gender, project)],
  fill = TRUE
)
clin[, age := as.numeric(age_at_index)]
clin[, EOCRC := as.integer(age < 50)]
clin[, Site := ifelse(project == "TCGA-READ", "Rectum", "Colon")]
clin[, Sex := ifelse(gender == "male", "Male", "Female")]
clin[, Stage := fcase(
  grepl("^Stage I", ajcc_pathologic_stage), "I",
  grepl("^Stage II", ajcc_pathologic_stage), "II",
  grepl("^Stage III", ajcc_pathologic_stage), "III",
  grepl("^Stage IV", ajcc_pathologic_stage), "IV",
  default = NA_character_
)]
clin[, T_bin := fcase(
  grepl("^T[0-2]|^Tis", ajcc_pathologic_t), "T1-2",
  grepl("^T[34]", ajcc_pathologic_t), "T3-4",
  default = NA_character_
)]
clin[, N_bin := fcase(
  grepl("^N0", ajcc_pathologic_n), "N0",
  grepl("^N[12]", ajcc_pathologic_n), "N+",
  default = NA_character_
)]
clin[, M1 := fcase(
  grepl("^M1", ajcc_pathologic_m), 1L,
  grepl("^M0", ajcc_pathologic_m), 0L,
  default = NA_integer_
)]

# ------------------------------------------------------------------------------
# 2. M1 方向性关联
# ------------------------------------------------------------------------------
m1_dt <- clin[!is.na(M1) & !is.na(age) & !is.na(T_bin) & !is.na(N_bin) & !is.na(Sex) & !is.na(Site)]
m1_dt[, `:=`(EOCRC = factor(EOCRC, levels = c(0, 1)),
             Sex = factor(Sex), Site = factor(Site),
             T_bin = factor(T_bin), N_bin = factor(N_bin))]
fit_m1 <- glm(M1 ~ EOCRC + Sex + Site + T_bin + N_bin, data = m1_dt, family = binomial)
s_m1 <- summary(fit_m1)$coefficients
est <- s_m1["EOCRC1", "Estimate"]; se <- s_m1["EOCRC1", "Std. Error"]; p <- s_m1["EOCRC1", "Pr(>|z|)"]
m1_tab <- data.table(
  Analysis = "TCGA M1 vs M0",
  N = nrow(m1_dt),
  Early_M1 = sum(m1_dt$M1 == 1 & m1_dt$EOCRC == 1),
  Early_Total = sum(m1_dt$EOCRC == 1),
  Late_M1 = sum(m1_dt$M1 == 1 & m1_dt$EOCRC == 0),
  Late_Total = sum(m1_dt$EOCRC == 0),
  OR = exp(est), Lower = exp(est - 1.96 * se), Upper = exp(est + 1.96 * se), P = p
)
m1_tab[, OR_Str := sprintf("%.2f (%.2f-%.2f)", OR, Lower, Upper)]
m1_tab[, P_Str := ifelse(P < 0.001, "<0.001", sprintf("%.3f", P))]
fwrite(m1_tab, "results/table6_tcga_m1_v2.csv")
print(m1_tab)

# ------------------------------------------------------------------------------
# 3. 转录程序评分
# ------------------------------------------------------------------------------
read_expr <- function(file) {
  x <- fread(file, data.table = FALSE, showProgress = FALSE)
  genes <- x[[1]]
  mat <- as.matrix(x[, -1, drop = FALSE])
  rownames(mat) <- genes
  if (anyDuplicated(genes)) {
    ord <- order(rowMeans(mat, na.rm = TRUE), decreasing = TRUE)
    mat <- mat[ord, , drop = FALSE]
    genes <- genes[ord]
    keep <- !duplicated(genes)
    mat <- mat[keep, , drop = FALSE]
    rownames(mat) <- genes[keep]
  }
  mat
}

cat("Loading TCGA expression matrices...\n")
mat_coad <- read_expr("结直肠癌数据/TCGA 结直肠癌数据/结肠癌COAD/tpms01A_log2.txt")
mat_read <- read_expr("结直肠癌数据/TCGA 结直肠癌数据/直肠癌READ/tpms01A_log2.txt")
mat <- cbind(mat_coad, mat_read)

sets <- list(
  Epithelial = c("CDH1", "EPCAM", "KRT8", "KRT18", "KRT19", "KRT20", "CLDN3", "CLDN4", "MUC1"),
  Mesenchymal = c("VIM", "CDH2", "FN1", "ZEB1", "ZEB2", "SNAI1", "SNAI2", "TWIST1", "MMP2", "MMP9", "COL1A1", "COL1A2", "FAP", "ACTA2"),
  Wnt = c("AXIN2", "LGR5", "MYC", "CCND1", "ASCL2", "RNF43", "NKD1", "SP5", "LEF1"),
  Proliferation = c("MKI67", "PCNA", "CCNB1", "CDK1", "TOP2A", "BUB1", "AURKA"),
  Immune = c("CD8A", "GZMA", "PRF1", "CD274", "CXCL9", "CXCL10", "IFNG")
)

score_list <- list()
for (nm in names(sets)) {
  g <- intersect(sets[[nm]], rownames(mat))
  cat(nm, "genes found:", length(g), "/", length(sets[[nm]]), "\n")
  if (length(g) < 3) next
  z <- t(scale(t(mat[g, , drop = FALSE])))
  score_list[[nm]] <- colMeans(z, na.rm = TRUE)
}
score_mat <- as.data.table(score_list)
score_mat[, barcode := colnames(mat)]
score_mat[, submitter_id := sub("-01A$", "", barcode)]

score_cols <- names(score_list)
score_clin <- clin[match(score_mat$submitter_id, clin$submitter_id)]
score_dt <- cbind(score_mat[, ..score_cols], score_clin[, .(submitter_id, EOCRC, Stage, Site, Sex, age)])
score_dt <- score_dt[!is.na(EOCRC) & !is.na(Stage) & !is.na(Sex) & !is.na(Site)]
score_dt[, `:=`(EOCRC = factor(EOCRC, levels = c(0, 1)),
                Stage = droplevels(factor(Stage, levels = c("I", "II", "III", "IV"))),
                Site = droplevels(factor(Site)),
                Sex = droplevels(factor(Sex)))]
cat("Samples with expression + clinical:", nrow(score_dt), " early:", sum(score_dt$EOCRC == 1), "\n")
cat("Factor levels: Stage", nlevels(score_dt$Stage), " Site", nlevels(score_dt$Site),
    " Sex", nlevels(score_dt$Sex), "\n")

cov_terms <- c("Stage", "Site", "Sex")
cov_terms <- cov_terms[sapply(score_dt[, ..cov_terms], function(x) nlevels(droplevels(x)) >= 2)]
cat("Covariates used in score models:", paste(cov_terms, collapse = ", "), "\n")

score_res <- list()
for (nm in names(sets)) {
  fml <- as.formula(paste0("`", nm, "` ~ EOCRC + ", paste(cov_terms, collapse = " + ")))
  fit <- lm(fml, data = score_dt)
  s <- summary(fit)$coefficients
  est <- s["EOCRC1", "Estimate"]; se <- s["EOCRC1", "Std. Error"]; p <- s["EOCRC1", "Pr(>|t|)"]
  sdv <- sd(score_dt[[nm]], na.rm = TRUE)
  score_res[[nm]] <- data.table(
    Score = nm, N = nrow(score_dt),
    Early_Mean = mean(score_dt[[nm]][score_dt$EOCRC == 1], na.rm = TRUE),
    Late_Mean = mean(score_dt[[nm]][score_dt$EOCRC == 0], na.rm = TRUE),
    Beta = est, Beta_Lower = est - 1.96 * se, Beta_Upper = est + 1.96 * se,
    Std_Difference = est / sdv, P = p
  )
}
score_tab <- rbindlist(score_res)
score_tab[, P_FDR := p.adjust(P, method = "BH")]
score_tab[, Beta_Str := sprintf("%.2f (%.2f-%.2f)", Beta, Beta_Lower, Beta_Upper)]
score_tab[, P_Str := ifelse(P < 0.001, "<0.001", sprintf("%.3f", P))]
fwrite(score_tab, "results/table6_tcga_scores_v2.csv")
fwrite(score_tab, "results/fig6_tcga_scores_v2.csv")
print(score_tab)

cat("\nS10 v2 completed successfully.\n")
