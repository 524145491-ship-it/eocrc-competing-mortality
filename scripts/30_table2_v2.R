# ==============================================================================
# Table 2 v2: Organ-specific synchronous metastases and age nonlinearity
# Inputs:
#   - results/table2_metastasis_logistic_v2.csv
#   - results/table2b_age_rcs_metastasis.csv
# Output:
#   - manuscript/tables/table2_metastasis.docx
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(flextable)
  library(officer)
})

tab <- fread("results/table2_metastasis_logistic_v2.csv")
rcs <- fread("results/table2b_age_rcs_metastasis.csv")

tab[, Endpoint_Short := c(
  "Any distant metastasis",
  "Liver metastasis (overall)",
  "Isolated liver metastasis",
  "Lung metastasis (overall)",
  "Isolated lung metastasis",
  "Bone metastasis",
  "Brain metastasis",
  "Multiple organ metastases (>=2)"
)[match(Endpoint, c(
  "Any Distant Metastasis",
  "Liver Metastasis (Overall)",
  "Isolated Liver Metastasis",
  "Lung Metastasis (Overall)",
  "Isolated Lung Metastasis",
  "Bone Metastasis",
  "Brain Metastasis",
  "Multiple Organ Metastases (>=2)"
))]]

tab[, P_nonlinear := rcs$P_nonlinear[match(Endpoint, rcs$Endpoint)]]
tab[, P_Nonlin_Str := ifelse(is.na(P_nonlinear), "-",
                             ifelse(P_nonlinear < 0.001, "<0.001", sprintf("%.3f", P_nonlinear)))]

out <- data.frame(
  Metastasis = tab$Endpoint_Short,
  `EOCRC, n (%)` = tab$EOCRC_Events,
  `LOCRC, n (%)` = tab$LOCRC_Events,
  `Crude OR (95% CI)` = tab$Crude_OR_Str,
  `Adjusted OR (95% CI)` = tab$Adjusted_OR_Str,
  `P` = tab$P_Str,
  `P for nonlinearity` = tab$P_Nonlin_Str,
  check.names = FALSE
)

ft <- flextable(out)
ft <- theme_booktabs(ft)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 8.5, part = "body")
ft <- fontsize(ft, size = 8.5, part = "header")
ft <- bold(ft, part = "header")
ft <- align(ft, j = 2:7, align = "center", part = "all")
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- width(ft, j = 1, width = 1.85)
ft <- width(ft, j = 2:3, width = 0.95)
ft <- width(ft, j = 4:5, width = 1.25)
ft <- width(ft, j = 6, width = 0.55)
ft <- width(ft, j = 7, width = 0.80)
ft <- set_caption(ft, "Table 2. Synchronous organ-specific metastases in early-onset versus late-onset colorectal cancer, 2010-2022.")
ft <- add_footer_lines(ft, paste0(
  "Adjusted for sex, race/ethnicity, year of diagnosis, primary site, histology, grade, and baseline CEA. ",
  "P for nonlinearity was obtained from a 5-knot restricted cubic spline for continuous age; ",
  "selected-age ORs are shown in Figure 2C and Table S2b. ",
  "EOCRC, early-onset colorectal cancer; LOCRC, late-onset colorectal cancer; OR, odds ratio; CI, confidence interval."
))
ft <- fontsize(ft, size = 7.5, part = "footer")

if (!dir.exists("manuscript/tables")) dir.create("manuscript/tables", recursive = TRUE)
save_as_docx(ft, path = "manuscript/tables/table2_metastasis.docx")
cat("Table 2 saved to manuscript/tables/table2_metastasis.docx\n")
