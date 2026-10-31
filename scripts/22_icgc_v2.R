# ==============================================================================
# Script: 22_icgc_v2.R
# Purpose: v2 S11 - ICGC COCA-CN 基因组方向性印证（探索性）
# Inputs:
#   - 结直肠癌数据/[ICGC-COCA-CN] 结直肠癌-中国/供者_COCA-CN.csv
#   - 结直肠癌数据/[ICGC-COCA-CN] 结直肠癌-中国/体细胞突变_公开_COCA-CN.csv
# Outputs:
#   - results/table7_icgc_genomic_v2.csv
# Notes:
#   - 突变负荷用体细胞突变数（非严格 TMB/Mb）；超突变阈值 >5000。
#   - 驱动基因用 Ensembl ID 映射；早发 M1 比例与 SEER 方向可能不一致，仅作描述。
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
})

source("scripts/mc_trace.R")

set.seed(20261029)

mut_file <- "结直肠癌数据/[ICGC-COCA-CN] 结直肠癌-中国/体细胞突变_公开_COCA-CN.csv"
donor_file <- "结直肠癌数据/[ICGC-COCA-CN] 结直肠癌-中国/供者_COCA-CN.csv"

cat("Reading ICGC donors...\n")
donors <- fread(donor_file, na.strings = c("", "NA"))
donors[, age := as.numeric(donor_age_at_diagnosis)]
donors[, early := as.integer(age < 50)]
donors[, sex := factor(donor_sex)]
donors[, M1 := as.integer(grepl("M1", donor_tumour_stage_at_diagnosis))]

cat("Reading ICGC somatic mutations (this may take a few minutes)...\n")
mut <- fread(mut_file, select = c("icgc_donor_id", "gene_affected"), showProgress = TRUE)

# 每位供者突变数
agg <- mut[, .(n_mut = .N), by = icgc_donor_id]

# 驱动基因（Ensembl ID）
gene_map <- c(
  APC = "ENSG00000134982",
  TP53 = "ENSG00000141510",
  KRAS = "ENSG00000133703",
  SMAD4 = "ENSG00000141646",
  BRAF = "ENSG00000157764",
  PIK3CA = "ENSG00000121879",
  ARID1A = "ENSG00000116044"
)
mut[, gene := NA_character_]
for (g in names(gene_map)) mut[gene_affected == gene_map[[g]], gene := g]
drv <- mut[!is.na(gene), .(mutated = 1L), by = .(icgc_donor_id, gene)]
drv_wide <- dcast(drv, icgc_donor_id ~ gene, value.var = "mutated", fill = 0)
for (g in names(gene_map)) if (!g %in% names(drv_wide)) drv_wide[, (g) := 0L]

x <- merge(donors, agg, by = "icgc_donor_id", all.x = TRUE)
x <- merge(x, drv_wide, by = "icgc_donor_id", all.x = TRUE)
for (g in names(gene_map)) x[is.na(get(g)), (g) := 0L]

x_mut <- x[!is.na(n_mut) & !is.na(early)]
cat("Donors with mutation data:", nrow(x_mut), " early:", sum(x_mut$early == 1), "\n")

res <- list()

# 1. 突变负荷
fit_burden <- lm(log10(n_mut + 1) ~ early + sex, data = x_mut)
s <- summary(fit_burden)$coefficients
res[["burden"]] <- data.table(
  Analysis = "Somatic mutation count (log10)",
  Early_Value = median(x_mut$n_mut[x_mut$early == 1]),
  Late_Value = median(x_mut$n_mut[x_mut$early == 0]),
  Effect = s["early", "Estimate"], Lower = s["early", "Estimate"] - 1.96 * s["early", "Std. Error"],
  Upper = s["early", "Estimate"] + 1.96 * s["early", "Std. Error"],
  P = s["early", "Pr(>|t|)"], N_Early = sum(x_mut$early == 1), N_Late = sum(x_mut$early == 0)
)

# 2. 超突变比例
x_mut[, hyper := as.integer(n_mut > 5000)]
tb <- table(x_mut$early, x_mut$hyper)
ft <- fisher.test(tb)
res[["hyper"]] <- data.table(
  Analysis = "Hypermutator (>5000 mutations), n (%)",
  Early_Value = sum(x_mut$early == 1 & x_mut$hyper == 1),
  Late_Value = sum(x_mut$early == 0 & x_mut$hyper == 1),
  Effect = unname(ft$estimate), Lower = ft$conf.int[1], Upper = ft$conf.int[2],
  P = ft$p.value, N_Early = sum(x_mut$early == 1), N_Late = sum(x_mut$early == 0)
)

# 3. 驱动基因频率
for (g in names(gene_map)) {
  tb <- table(x_mut$early, x_mut[[g]])
  if (nrow(tb) < 2 || ncol(tb) < 2) next
  ft <- fisher.test(tb)
  res[[g]] <- data.table(
    Analysis = paste0(g, " mutation, n (%)"),
    Early_Value = sum(x_mut$early == 1 & x_mut[[g]] == 1),
    Late_Value = sum(x_mut$early == 0 & x_mut[[g]] == 1),
    Effect = unname(ft$estimate), Lower = ft$conf.int[1], Upper = ft$conf.int[2],
    P = ft$p.value, N_Early = sum(x_mut$early == 1), N_Late = sum(x_mut$early == 0)
  )
}

# 4. M1 比例（全部有分期供者；描述性）
m1_dt <- x[!is.na(M1) & !is.na(early)]
tb <- table(m1_dt$early, m1_dt$M1)
ft <- fisher.test(tb)
res[["M1"]] <- data.table(
  Analysis = "M1 at diagnosis, n (%)",
  Early_Value = sum(m1_dt$early == 1 & m1_dt$M1 == 1),
  Late_Value = sum(m1_dt$early == 0 & m1_dt$M1 == 1),
  Effect = unname(ft$estimate), Lower = ft$conf.int[1], Upper = ft$conf.int[2],
  P = ft$p.value, N_Early = sum(m1_dt$early == 1), N_Late = sum(m1_dt$early == 0)
)

tab7 <- rbindlist(res, fill = TRUE)
tab7[, P_FDR := p.adjust(P, method = "BH")]
tab7[, Early_Pct := 100 * Early_Value / N_Early]
tab7[, Late_Pct := 100 * Late_Value / N_Late]
tab7[Analysis == "Somatic mutation count (log10)", `:=`(Early_Pct = NA_real_, Late_Pct = NA_real_)]
fwrite(tab7, "results/table7_icgc_genomic_v2.csv")
print(tab7[, .(Analysis, N_Early, N_Late, Early_Pct, Late_Pct, Effect, P, P_FDR)], digits = 3)

cat("\nS11 v2 completed successfully.\n")
