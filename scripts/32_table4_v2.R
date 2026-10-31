# ==============================================================================
# Table 4 v2: Competing mortality in Stage I-III
# Inputs:
#   - results/table4b_cif_absolute_differences_v2.csv
#   - results/table4_competing_risks_v2.csv
#   - results/table4d_evalues_v2.csv
#   - results/table4e_gray_unweighted_v2.csv
# Output:
#   - manuscript/tables/table4_competing.docx
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(flextable)
  library(officer)
})

abs_dt <- fread("results/table4b_cif_absolute_differences_v2.csv")
model_dt <- fread("results/table4_competing_risks_v2.csv")[Model != "Absolute CIF difference"]
ev <- fread("results/table4d_evalues_v2.csv")
gray <- fread("results/table4e_gray_unweighted_v2.csv")

fmt_p <- function(p) ifelse(is.na(p), "-", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
ep_lab <- c(CCSM = "Cancer-specific mortality", NCSM = "Non-cancer competing mortality")
model_lab <- c("Cause-specific Cox" = "Cause-specific HR", "Fine-Gray subdistribution" = "Fine-Gray sdHR")

rows <- list()
rows[[1]] <- data.frame(Analysis = "Weighted cumulative incidence", Early = "", Late = "", Effect = "", P = "", Bold = TRUE)
for (i in seq_len(nrow(abs_dt))) {
  r <- abs_dt[i]
  rows[[length(rows) + 1]] <- data.frame(
    Analysis = sprintf("%s, %d-year", ep_lab[[r$Endpoint]], r$Time_Months / 12),
    Early = sprintf("%.2f%%", r$EOCRC * 100),
    Late = sprintf("%.2f%%", r$LOCRC * 100),
    Effect = sprintf("%.2f pp (%.2f to %.2f)", r$Difference * 100, r$Diff_Lower * 100, r$Diff_Upper * 100),
    P = fmt_p(r$P), Bold = FALSE
  )
}
rows[[length(rows) + 1]] <- data.frame(Analysis = "Hazard ratios", Early = "", Late = "", Effect = "", P = "", Bold = TRUE)
for (i in seq_len(nrow(model_dt))) {
  r <- model_dt[i]
  rows[[length(rows) + 1]] <- data.frame(
    Analysis = sprintf("%s, %s", ep_lab[[r$Endpoint]], model_lab[[r$Model]]),
    Early = "-", Late = "-", Effect = r$HR_Str, P = r$P_Str, Bold = FALSE
  )
}
rows[[length(rows) + 1]] <- data.frame(Analysis = "E-values", Early = "", Late = "", Effect = "", P = "", Bold = TRUE)
for (i in seq_len(nrow(ev))) {
  r <- ev[i]
  rows[[length(rows) + 1]] <- data.frame(
    Analysis = sprintf("%s, %s", ep_lab[[r$Endpoint]], model_lab[[r$Model]]),
    Early = "-", Late = "-",
    Effect = sprintf("%.2f (%.2f)", r$Evalue_Point, r$Evalue_CI), P = "-", Bold = FALSE
  )
}

out <- rbindlist(rows)
out[, Bold := as.logical(Bold)]

ft <- flextable(out[, .(Analysis, Early, Late, Effect, P)])
ft <- theme_booktabs(ft)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 8.5, part = "body")
ft <- fontsize(ft, size = 8.5, part = "header")
ft <- bold(ft, part = "header")
ft <- set_header_labels(ft, Analysis = "Analysis", Early = "Early-onset", Late = "Late-onset",
                       Effect = "Effect (95% CI)", P = "P")
ft <- align(ft, j = 2:5, align = "center", part = "all")
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- width(ft, j = 1, width = 2.65)
ft <- width(ft, j = 2:3, width = 0.85)
ft <- width(ft, j = 4, width = 1.75)
ft <- width(ft, j = 5, width = 0.60)
bold_idx <- which(out$Bold)
if (length(bold_idx) > 0) ft <- bold(ft, i = bold_idx, part = "body")

gray_txt <- paste0(
  "Unweighted Gray's test: CCSM chi-square ",
  sprintf("%.2f", gray[Endpoint == "CCSM", Chisq]), ", P ", ifelse(gray[Endpoint == "CCSM", P] < 0.001, "<0.001", sprintf("%.3f", gray[Endpoint == "CCSM", P])),
  "; NCSM chi-square ", sprintf("%.2f", gray[Endpoint == "NCSM", Chisq]), ", P ", ifelse(gray[Endpoint == "NCSM", P] < 0.001, "<0.001", sprintf("%.3f", gray[Endpoint == "NCSM", P])), "."
)
ft <- set_caption(ft, "Table 4. Competing mortality in the Stage I-III cohort.")
ft <- add_footer_lines(ft, paste0(
  "Weighted cumulative incidence and IPTW-weighted hazard models; absolute differences in percentage points (pp). ",
  "Cause-specific HR from cause-specific Cox models; sdHR from adjusted Fine-Gray subdistribution models. ",
  gray_txt, " ",
  "E-values quantify robustness to unmeasured confounding. ",
  "CCSM, cancer-specific mortality; NCSM, non-cancer competing mortality; HR, hazard ratio; sdHR, subdistribution hazard ratio; IPTW, inverse probability of treatment weighting."
))
ft <- fontsize(ft, size = 7.5, part = "footer")

save_as_docx(ft, path = "manuscript/tables/table4_competing.docx")
cat("Table 4 saved to manuscript/tables/table4_competing.docx\n")
