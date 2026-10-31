# ==============================================================================
# Figure S2 v2: Schoenfeld residual diagnostics for weighted Cox models
# Design: two panels, OS and CSS; beta(t) for EOCRC over time; IPTW-weighted models.
# Canvas 180 x 90 mm.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(survival)
  library(survminer)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mc_figure.R")

df <- readRDS("data/cohort_iii_weighted_v2.rds")

fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

make_zph <- function(event_var, title) {
  fit <- coxph(as.formula(paste0("Surv(Survival_Months, ", event_var, ") ~ EOCRC")),
               data = df, weights = iptw_w, robust = TRUE)
  zph <- cox.zph(fit)
  p <- ggcoxzph(zph, point.col = "#C4615A", point.size = 0.6, point.alpha = 0.25)[[1]] +
    labs(title = title,
         subtitle = sprintf("Schoenfeld P %s", fmt_p(zph$table["EOCRC", "p"])),
         x = "Months since diagnosis", y = "Beta(t) for EOCRC") +
    theme_mc(width_mm = 85) +
    theme(
      plot.title = element_text(size = 8.3, face = "bold"),
      plot.subtitle = element_text(size = 7.0)
    )
  p
}

p1 <- make_zph("OS_Status", "Overall survival")
p2 <- make_zph("CSS_Status", "Cancer-specific survival")

fig <- mc_compose(p1, p2, ncol = 2, width_mm = 180, height_mm = 90)
mc_save(fig, "manuscript/supplementary/figS2_schoenfeld.png", width_mm = 180, height_mm = 90)
