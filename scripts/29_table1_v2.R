# ==============================================================================
# Table 1 v2: Baseline characteristics of the Stage I-III cohort
# Inputs:
#   - results/table1_baseline_v2.csv
#   - data/cohort_iii_weighted_v2.rds (age mean/SD, group N)
# Output:
#   - manuscript/tables/table1_baseline.docx
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(flextable)
  library(officer)
})

tab <- fread("results/table1_baseline_v2.csv")
bal <- fread("results/tableS2_iptw_balance_v2.csv")
max_smd <- max(abs(bal$Diff.Adj), na.rm = TRUE)
df <- readRDS("data/cohort_iii_weighted_v2.rds")

n_eo <- sum(df$EOCRC == 1)
n_lo <- sum(df$EOCRC == 0)
age <- df[, .(mean = mean(Age_Num), sd = sd(Age_Num)), by = EOCRC]
age_eo <- age[EOCRC == 1]; age_lo <- age[EOCRC == 0]

var_labels <- c(
  Sex = "Sex",
  Race = "Race/ethnicity",
  Year_Group = "Year of diagnosis",
  Subsite = "Primary site",
  Histology = "Histology",
  Grade = "Histological grade",
  Stage = "AJCC stage",
  Surgery = "Surgical resection",
  Chemo = "Chemotherapy",
  Radiation = "Radiation therapy",
  LN_Examined = "Lymph nodes examined"
)

cat_labels <- c(
  "Non-Hispanic White" = "Non-Hispanic White",
  "Non-Hispanic Black" = "Non-Hispanic Black",
  "Asian/PI" = "Asian/Pacific Islander",
  "Other/Unknown" = "Other/unknown",
  "Classic adenocarcinoma" = "Classic adenocarcinoma",
  "Mucinous/Signet ring" = "Mucinous/signet-ring",
  "Well/Moderate" = "Well/moderate",
  "Poor/Undifferentiated" = "Poor/undifferentiated",
  "No/Unknown" = "No/unknown",
  ">=12 nodes" = ">=12 nodes",
  "<12 nodes" = "<12 nodes",
  "0 nodes" = "0 nodes"
)

rows <- list()
rows[[1]] <- data.table(
  Characteristic = "Age at diagnosis, years, mean (SD)",
  EOCRC = sprintf("%.1f (%.1f)", age_eo$mean, age_eo$sd),
  LOCRC = sprintf("%.1f (%.1f)", age_lo$mean, age_lo$sd),
  EOCRC_w = "-",
  LOCRC_w = "-"
)

for (v in unique(tab$Variable)) {
  rows[[length(rows) + 1]] <- data.table(
    Characteristic = var_labels[[v]],
    EOCRC = "", LOCRC = "", EOCRC_w = "", LOCRC_w = ""
  )
  sub <- tab[Variable == v]
  for (i in seq_len(nrow(sub))) {
    cat_lab <- sub$Category[i]
    if (cat_lab %in% names(cat_labels)) cat_lab <- cat_labels[[cat_lab]]
    rows[[length(rows) + 1]] <- data.table(
      Characteristic = paste0("    ", cat_lab),
      EOCRC = sub$Unweighted_EOCRC[i],
      LOCRC = sub$Unweighted_LOCRC[i],
      EOCRC_w = sub$Weighted_EOCRC[i],
      LOCRC_w = sub$Weighted_LOCRC[i]
    )
  }
}

out <- rbindlist(rows)
setnames(out, c("Characteristic",
                sprintf("EOCRC (n=%s)", format(n_eo, big.mark = ",")),
                sprintf("LOCRC (n=%s)", format(n_lo, big.mark = ",")),
                "EOCRC, weighted %", "LOCRC, weighted %"))

ft <- flextable(out)
ft <- theme_booktabs(ft)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 8.5, part = "body")
ft <- fontsize(ft, size = 9, part = "header")
ft <- bold(ft, part = "header")
ft <- align(ft, j = 2:5, align = "center", part = "all")
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- width(ft, j = 1, width = 2.15)
ft <- width(ft, j = 2:5, width = 1.10)
ft <- bold(ft, i = which(out$Characteristic %in% var_labels), part = "body")
ft <- set_caption(ft, "Table 1. Baseline characteristics of the Stage I-III cohort before and after IPTW.")
ft <- add_footer_lines(ft, paste0(
  "Values are n (%) unless otherwise indicated. Weighted columns show IPTW-adjusted percentages. ",
  sprintf("Maximum absolute standardized mean difference after IPTW was %.4f. ", max_smd),
  "EOCRC, early-onset colorectal cancer; LOCRC, late-onset colorectal cancer; ",
  "IPTW, inverse probability of treatment weighting; LN, lymph node."
))
ft <- fontsize(ft, size = 7.5, part = "footer")

if (!dir.exists("manuscript/tables")) dir.create("manuscript/tables", recursive = TRUE)
save_as_docx(ft, path = "manuscript/tables/table1_baseline.docx")
cat("Table 1 saved to manuscript/tables/table1_baseline.docx\n")
