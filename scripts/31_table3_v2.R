# ==============================================================================
# Table 3 v2: Survival models, time-varying HR, and RMST
# Inputs:
#   - results/table3_survival_models_v2.csv
#   - results/tableS5_rmst_v2.csv
# Output:
#   - manuscript/tables/table3_survival.docx
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(flextable)
  library(officer)
})

surv <- fread("results/table3_survival_models_v2.csv")
rmst <- fread("results/tableS5_rmst_v2.csv")

label_map <- c(
  "OS: multivariable Cox (unweighted)" = "Overall survival: multivariable Cox",
  "OS: IPTW-weighted Cox" = "Overall survival: IPTW-weighted Cox",
  "OS: IPTW + covariate double-adjusted" = "Overall survival: IPTW + double adjustment",
  "CSS: multivariable Cox (unweighted)" = "Cancer-specific survival: multivariable Cox",
  "CSS: IPTW-weighted Cox" = "Cancer-specific survival: IPTW-weighted Cox",
  "CSS: IPTW + covariate double-adjusted" = "Cancer-specific survival: IPTW + double adjustment"
)
surv[, Model_Display := label_map[Model]]
surv[is.na(Model_Display), Model_Display := Model]
surv[, Model_Display := gsub("^OS dynamic HR:", "Overall survival HR,", Model_Display)]
surv[, Model_Display := gsub("^CSS dynamic HR:", "Cancer-specific HR,", Model_Display)]

main <- surv[!grepl("dynamic HR", Model, ignore.case = TRUE)]
dyn <- surv[grepl("dynamic HR", Model, ignore.case = TRUE)]

rows <- list()
rows[[1]] <- data.frame(Model = "Cox proportional hazards models", Estimate = "", P = "", Bold = TRUE)
for (i in seq_len(nrow(main))) {
  rows[[length(rows) + 1]] <- data.frame(
    Model = main$Model_Display[i], Estimate = main$HR_Str[i], P = main$P_Str[i], Bold = FALSE
  )
}
os_dyn <- dyn[Endpoint == "OS"]
css_dyn <- dyn[Endpoint == "CSS"]
rows[[length(rows) + 1]] <- data.frame(Model = "Time-varying overall survival hazard ratios", Estimate = "", P = "", Bold = TRUE)
for (i in seq_len(nrow(os_dyn))) {
  rows[[length(rows) + 1]] <- data.frame(
    Model = os_dyn$Model_Display[i], Estimate = os_dyn$HR_Str[i], P = os_dyn$P_Str[i], Bold = FALSE
  )
}
rows[[length(rows) + 1]] <- data.frame(Model = "Time-varying cancer-specific hazard ratios", Estimate = "", P = "", Bold = TRUE)
for (i in seq_len(nrow(css_dyn))) {
  rows[[length(rows) + 1]] <- data.frame(
    Model = css_dyn$Model_Display[i], Estimate = css_dyn$HR_Str[i], P = css_dyn$P_Str[i], Bold = FALSE
  )
}
rows[[length(rows) + 1]] <- data.frame(Model = "Restricted mean survival time difference (months)", Estimate = "", P = "", Bold = TRUE)
for (i in seq_len(nrow(rmst))) {
  lab <- sprintf("%s, %d-year", rmst$Endpoint[i], rmst$Tau_Months[i] / 12)
  est <- sprintf("%.2f (%.2f to %.2f)", rmst$Difference[i], rmst$Diff_Lower[i], rmst$Diff_Upper[i])
  p <- ifelse(rmst$P[i] < 0.001, "<0.001", sprintf("%.3f", rmst$P[i]))
  rows[[length(rows) + 1]] <- data.frame(Model = lab, Estimate = est, P = p, Bold = FALSE)
}

out <- rbindlist(rows)
out[, Bold := as.logical(Bold)]

ft <- flextable(out[, .(Model, Estimate, P)])
ft <- theme_booktabs(ft)
ft <- font(ft, fontname = "Times New Roman", part = "all")
ft <- fontsize(ft, size = 8.5, part = "body")
ft <- fontsize(ft, size = 8.5, part = "header")
ft <- bold(ft, part = "header")
ft <- set_header_labels(ft, Model = "Model", Estimate = "Estimate (95% CI)", P = "P")
ft <- align(ft, j = 2:3, align = "center", part = "all")
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- width(ft, j = 1, width = 3.70)
ft <- width(ft, j = 2, width = 1.85)
ft <- width(ft, j = 3, width = 0.55)
bold_idx <- which(out$Bold)
if (length(bold_idx) > 0) ft <- bold(ft, i = bold_idx, part = "body")
ft <- set_caption(ft, "Table 3. Survival outcomes in the Stage I-III cohort: Cox models, time-varying hazard ratios, and restricted mean survival time.")
ft <- add_footer_lines(ft, paste0(
  "Cox models were adjusted for sex, race/ethnicity, year of diagnosis, primary site, histology, grade, stage, surgery, chemotherapy, radiotherapy, and examined lymph nodes; ",
  "IPTW-weighted models used stabilized inverse probability of treatment weights. ",
  "Time-varying intervals are 0-12, 12-36, 36-60, 60-120, and >120 months. ",
  "RMST differences are early- minus late-onset in months; positive values favor early-onset. ",
  "OS, overall survival; CSS, cancer-specific survival; HR, hazard ratio; IPTW, inverse probability of treatment weighting; RMST, restricted mean survival time."
))
ft <- fontsize(ft, size = 7.5, part = "footer")

save_as_docx(ft, path = "manuscript/tables/table3_survival.docx")
cat("Table 3 saved to manuscript/tables/table3_survival.docx\n")
