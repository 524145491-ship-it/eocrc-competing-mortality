# ==============================================================================
# Table 5 v2: Exploratory molecular and genomic findings
# Inputs:
#   - results/table6_tcga_m1_v2.csv
#   - results/table6_tcga_scores_v2.csv
#   - results/table7_icgc_genomic_v2.csv
# Output:
#   - manuscript/tables/table5_molecular.docx
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(flextable)
  library(officer)
})

m1 <- fread("results/table6_tcga_m1_v2.csv")
sc <- fread("results/table6_tcga_scores_v2.csv")
icgc <- fread("results/table7_icgc_genomic_v2.csv")

fmt_p <- function(p) ifelse(is.na(p), "-", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
fmt_or <- function(est, lo, hi) sprintf("%.2f (%.2f to %.2f)", est, lo, hi)

rows <- list()
rows[[1]] <- data.frame(Analysis = "TCGA: M1 at diagnosis", Early = "", Late = "", Effect = "", P = "", FDR = "", Bold = TRUE)
rows[[length(rows) + 1]] <- data.frame(
  Analysis = "M1 vs M0",
  Early = sprintf("%d/%d (%.1f%%)", m1$Early_M1, m1$Early_Total, 100 * m1$Early_M1 / m1$Early_Total),
  Late = sprintf("%d/%d (%.1f%%)", m1$Late_M1, m1$Late_Total, 100 * m1$Late_M1 / m1$Late_Total),
  Effect = m1$OR_Str, P = m1$P_Str, FDR = "-", Bold = FALSE
)

rows[[length(rows) + 1]] <- data.frame(Analysis = "TCGA: transcriptional programs", Early = "", Late = "", Effect = "", P = "", FDR = "", Bold = TRUE)
for (i in seq_len(nrow(sc))) {
  r <- sc[i]
  sd_factor <- r$Beta / r$Std_Difference
  lo <- r$Beta_Lower / sd_factor
  hi <- r$Beta_Upper / sd_factor
  rows[[length(rows) + 1]] <- data.frame(
    Analysis = r$Score,
    Early = sprintf("%.2f", r$Early_Mean), Late = sprintf("%.2f", r$Late_Mean),
    Effect = sprintf("%.2f (%.2f to %.2f)", r$Std_Difference, lo, hi),
    P = fmt_p(r$P), FDR = sprintf("%.3f", r$P_FDR), Bold = FALSE
  )
}

rows[[length(rows) + 1]] <- data.frame(Analysis = "ICGC: mutation burden and M1", Early = "", Late = "", Effect = "", P = "", FDR = "", Bold = TRUE)
burden <- icgc[Analysis == "Somatic mutation count (log10)"]
rows[[length(rows) + 1]] <- data.frame(
  Analysis = "Median somatic mutation count",
  Early = format(burden$Early_Value, big.mark = ","), Late = format(burden$Late_Value, big.mark = ","),
  Effect = fmt_or(burden$Effect, burden$Lower, burden$Upper),
  P = fmt_p(burden$P), FDR = sprintf("%.3f", burden$P_FDR), Bold = FALSE
)
for (a in c("Hypermutator (>5000 mutations), n (%)", "M1 at diagnosis, n (%)")) {
  r <- icgc[Analysis == a]
  lab <- ifelse(grepl("Hypermutator", a), "Hypermutator", "M1 at diagnosis")
  rows[[length(rows) + 1]] <- data.frame(
    Analysis = lab,
    Early = sprintf("%d/%d (%.1f%%)", r$Early_Value, r$N_Early, r$Early_Pct),
    Late = sprintf("%d/%d (%.1f%%)", r$Late_Value, r$N_Late, r$Late_Pct),
    Effect = fmt_or(r$Effect, r$Lower, r$Upper),
    P = fmt_p(r$P), FDR = sprintf("%.3f", r$P_FDR), Bold = FALSE
  )
}

rows[[length(rows) + 1]] <- data.frame(Analysis = "ICGC: driver mutations", Early = "", Late = "", Effect = "", P = "", FDR = "", Bold = TRUE)
drv <- icgc[grepl(" mutation, n", Analysis)]
drv[, Gene := sub(" mutation, n.*", "", Analysis)]
drv <- drv[order(Effect, decreasing = TRUE)]
for (i in seq_len(nrow(drv))) {
  r <- drv[i]
  rows[[length(rows) + 1]] <- data.frame(
    Analysis = r$Gene,
    Early = sprintf("%d/%d (%.1f%%)", r$Early_Value, r$N_Early, r$Early_Pct),
    Late = sprintf("%d/%d (%.1f%%)", r$Late_Value, r$N_Late, r$Late_Pct),
    Effect = fmt_or(r$Effect, r$Lower, r$Upper),
    P = fmt_p(r$P), FDR = sprintf("%.3f", r$P_FDR), Bold = FALSE
  )
}

out <- rbindlist(rows)
out[, Bold := as.logical(Bold)]

ft <- flextable(out[, .(Analysis, Early, Late, Effect, P, FDR)])
ft <- theme_booktabs(ft)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 8.0, part = "body")
ft <- fontsize(ft, size = 8.0, part = "header")
ft <- bold(ft, part = "header")
ft <- set_header_labels(ft, Analysis = "Analysis", Early = "Early-onset", Late = "Late-onset",
                       Effect = "Effect (95% CI)", P = "P", FDR = "FDR")
ft <- align(ft, j = 2:6, align = "center", part = "all")
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- width(ft, j = 1, width = 2.05)
ft <- width(ft, j = 2:3, width = 1.00)
ft <- width(ft, j = 4, width = 1.35)
ft <- width(ft, j = 5:6, width = 0.50)
bold_idx <- which(out$Bold)
if (length(bold_idx) > 0) ft <- bold(ft, i = bold_idx, part = "body")
ft <- set_caption(ft, "Table 5. Exploratory molecular and genomic comparison of early-onset versus late-onset colorectal cancer.")
ft <- add_footer_lines(ft, paste0(
  "TCGA M1 model adjusted for sex, site, T stage, and N stage; transcriptional models adjusted for stage, site, and sex. ",
  "ICGC comparisons are exploratory and unadjusted except where indicated; P values are two-sided and FDR is Benjamini-Hochberg. ",
  "For transcriptional scores, early/late values are mean z-scores and the effect is the standardized difference. ",
  "For ICGC mutation burden, the effect is the log10 mutation-count difference. ",
  "TCGA, The Cancer Genome Atlas; ICGC, International Cancer Genome Consortium; M1, distant metastasis; FDR, false discovery rate."
))
ft <- fontsize(ft, size = 7.5, part = "footer")

save_as_docx(ft, path = "manuscript/tables/table5_molecular.docx")
cat("Table 5 saved to manuscript/tables/table5_molecular.docx\n")
