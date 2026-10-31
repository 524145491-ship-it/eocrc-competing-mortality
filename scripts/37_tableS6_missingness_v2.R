# 37_tableS6_missingness_v2.R
# Purpose: report the number and percentage of patients with SEER-recorded
#          "unknown" categories for each modelled covariate in the stage I-III
#          survival cohort (STROBE items 12c and 14b).
# Input : data/cohort_iii_v2.rds
# Output: results/tableS6_missingness_v2.csv
#         manuscript/supplementary/tableS2_missingness.docx
# Notes : the internal step and results-CSV keep their original "S6" identifier
#         for traceability; the manuscript-facing supplementary table is
#         numbered S2 because it is the second supplementary table cited in
#         main.md (renumbered 2026-10-30).
# Notes : SEER records chemotherapy and radiotherapy as "yes" versus
#         "no or unknown"; these two variables therefore cannot separate
#         confirmed absence of treatment from missing documentation.

suppressPackageStartupMessages({
  library(data.table)
  library(flextable)
  library(officer)
})

set.seed(20261030)

d <- readRDS("data/cohort_iii_v2.rds")
N <- nrow(d)

spec <- data.table(
  Variable = c("Race/ethnicity", "Grade", "Surgery", "Examined lymph nodes",
               "Chemotherapy", "Radiotherapy"),
  Column   = c("Race", "Grade", "Surgery", "LN_Examined", "Chemo", "Radiation"),
  Unknown_levels = c("Other/Unknown", "Unknown", "No/Unknown",
                     "Unknown", "No/Unknown", "No/Unknown")
)

rows <- lapply(seq_len(nrow(spec)), function(i) {
  x <- as.character(d[[spec$Column[i]]])
  n_unknown <- sum(x %in% strsplit(spec$Unknown_levels[i], "\\|")[[1]])
  n_yes <- if (spec$Column[i] %in% c("Chemo", "Radiation")) sum(x == "Yes") else NA_integer_
  data.table(
    Variable = spec$Variable[i],
    Level_recorded_as = spec$Unknown_levels[i],
    N = N,
    N_unknown_or_merged = n_unknown,
    Percent = round(100 * n_unknown / N, 1),
    N_yes = n_yes,
    Percent_yes = if (is.na(n_yes)) NA_real_ else round(100 * n_yes / N, 1),
    Interpretation = if (spec$Column[i] %in% c("Chemo", "Radiation")) {
      "Yes versus no-or-unknown; documented treatment only"
    } else {
      "Explicit unknown level retained in models"
    }
  )
})
out <- rbindlist(rows)

fwrite(out, "results/tableS6_missingness_v2.csv")

ft <- flextable(out[, .(Variable, Level_recorded_as, N_unknown_or_merged, Percent,
                        N_yes, Percent_yes, Interpretation)])
ft <- set_header_labels(ft,
  Variable = "Variable",
  Level_recorded_as = "Level recorded as",
  N_unknown_or_merged = "Patients with level",
  Percent = "%",
  N_yes = "Recorded as yes",
  Percent_yes = "% yes",
  Interpretation = "Interpretation")
ft <- add_header_row(ft, values = c(paste0("Stage I-III cohort (N = ", format(N, big.mark = ","), ")")), colwidths = 7)
ft <- fontsize(ft, size = 9, part = "all")
ft <- bold(ft, part = "header")
ft <- autofit(ft)
ft <- border_remove(ft)
ft <- set_caption(ft, "Table S2. Unknown or merged covariate categories in the Stage I-III cohort.")
ft <- hline_top(ft, part = "header", border = fp_border(width = 1.2))
ft <- hline_bottom(ft, part = "header", border = fp_border(width = 0.8))
ft <- hline_bottom(ft, part = "body", border = fp_border(width = 1.2))
ft <- add_footer_lines(ft, "SEER records chemotherapy and radiotherapy as yes versus no or unknown; these variables capture documented treatment, not confirmed absence of treatment.")
ft <- fontsize(ft, size = 8, part = "footer")
save_as_docx(ft, path = "manuscript/supplementary/tableS2_missingness.docx")

cat("N =", N, "\n")
print(out)
