# ==============================================================================
# Figure 5 v2: Competing mortality in Stage I-III
# Design:
#   A: weighted CIF for cancer-specific mortality (CCSM).
#   B: weighted CIF for non-cancer competing mortality (NCSM).
#   C: cause-of-death composition over follow-up intervals.
#   D: cause-specific HR vs Fine-Gray subdistribution HR.
#   Canvas 180 x 165 mm; layout AB / CD.
#   Cohort colors EOCRC #C4615A, LOCRC #384D7A; cause colors CCSM #C4615A,
#   NCSM #65B9B7; model colors csHR #384D7A, sdHR #C4615A.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
})

source("scripts/mc_figure.R")

cif <- fread("results/fig5_cif_curves_v2.csv")
cif <- cif[Time <= 120]
# table4b stores absolute differences as proportions; convert to percentage points for plotting
abs_dt <- fread("results/table4b_cif_absolute_differences_v2.csv")
d_ccsm <- abs_dt[Time_Months == 60 & Endpoint == "CCSM", Difference] * 100
d_ncsm <- abs_dt[Time_Months == 60 & Endpoint == "NCSM", Difference] * 100
ann_ccsm <- sprintf("5-year difference
%.2f pp", d_ccsm)
ann_ncsm <- sprintf("5-year difference
%.2f pp", d_ncsm)
cif[, Cohort := factor(Cohort, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))]

make_cif <- function(yvar, sevar, title, annotation, show_legend = FALSE) {
  d <- copy(cif)
  d[, `:=`(lo = pmax(get(yvar) - 1.96 * get(sevar), 0),
           hi = get(yvar) + 1.96 * get(sevar))]
  ggplot(d, aes(x = Time, y = get(yvar), color = Cohort, fill = Cohort)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, color = NA) +
    geom_step(linewidth = 0.65) +
    scale_color_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = NULL) +
    scale_fill_manual(values = c("Early-Onset (<50)" = "#C4615A", "Late-Onset (>=50)" = "#384D7A"), name = NULL) +
    scale_x_continuous(breaks = seq(0, 120, by = 24), limits = c(0, 120)) +
    scale_y_continuous(limits = c(0, 0.30), breaks = seq(0, 0.30, by = 0.05),
                       labels = scales::percent_format(accuracy = 1)) +
    labs(x = "Months since diagnosis", y = "Cumulative incidence",
         title = title, subtitle = "Stage I-III; IPTW-weighted") +
    annotate("text", x = 4, y = 0.27, label = annotation, hjust = 0, size = 2.55,
             color = "#1E293B", lineheight = 1.05) +
    theme_mc(width_mm = 85) +
    theme(
      legend.position = if (show_legend) "bottom" else "none",
      legend.margin = margin(t = 1, unit = "mm"),
      plot.title = element_text(size = 8.2, face = "bold"),
      plot.subtitle = element_text(size = 6.9)
    )
}

p5a <- make_cif("CCSM", "CCSM_SE", "Cancer-specific mortality",
                "5-year difference\n-5.25 pp", TRUE)
p5b <- make_cif("NCSM", "NCSM_SE", "Non-cancer competing mortality",
                "5-year difference\n-10.40 pp", FALSE)

# Panel C: cause-of-death composition
comp <- fread("results/table4c_cause_composition_v2.csv")
comp[, Time_Bin := factor(Time_Bin, levels = c("<=1y", "1-3y", "3-5y", "5-10y", ">10y"),
                          labels = c("<=1", "1-3", "3-5", "5-10", ">10"))]
comp_long <- melt(comp, id.vars = c("Age_Group", "Time_Bin"),
                  measure.vars = c("CCSM_Prop", "NCSM_Prop"),
                  variable.name = "Cause", value.name = "Proportion")
comp_long[, Cause := factor(ifelse(Cause == "CCSM_Prop", "Cancer-specific", "Non-cancer competing"),
                            levels = c("Cancer-specific", "Non-cancer competing"))]
comp_long[, Age_Group := factor(Age_Group, levels = c("Early-Onset (<50)", "Late-Onset (>=50)"))]

p5c <- ggplot(comp_long, aes(x = Time_Bin, y = Proportion, fill = Cause)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  facet_wrap(~ Age_Group) +
  scale_fill_manual(values = c("Cancer-specific" = "#C4615A", "Non-cancer competing" = "#65B9B7"), name = NULL) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0, 0)) +
  labs(x = "Years since diagnosis", y = "Proportion of deaths",
       title = "Cause-of-death composition", subtitle = "Among deaths; weighted proportions") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    strip.text = element_text(size = 7.0, face = "bold"),
    plot.title = element_text(size = 8.2, face = "bold"),
    plot.subtitle = element_text(size = 6.9),
    axis.text.x = element_text(size = 6.9)
  )

# Panel D: csHR vs sdHR forest
fg <- fread("results/fig5_finegray_forest_v2.csv")
fg[, Label := paste0(Endpoint, " ", ifelse(Model == "Cause-specific Cox", "csHR", "sdHR"))]
fg[, Label := factor(Label, levels = c("CCSM csHR", "CCSM sdHR", "NCSM csHR", "NCSM sdHR"))]
fg[, Model := factor(Model, levels = c("Cause-specific Cox", "Fine-Gray subdistribution"),
                     labels = c("Cause-specific", "Fine-Gray"))]

p5d <- ggplot(fg, aes(y = Label, color = Model, shape = Model)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "#D9534F", linewidth = 0.4) +
  geom_segment(aes(x = Lower, xend = Upper, y = Label, yend = Label), linewidth = 0.55) +
  geom_point(aes(x = HR), size = 2.6) +
  geom_text(aes(x = 1.05, label = HR_Str), hjust = 0, size = 2.55, color = "#1E293B") +
  scale_color_manual(values = c("Cause-specific" = "#384D7A", "Fine-Gray" = "#C4615A"), name = NULL) +
  scale_shape_manual(values = c("Cause-specific" = 16, "Fine-Gray" = 18), name = NULL) +
  scale_x_log10(limits = c(0.10, 1.6), breaks = c(0.15, 0.3, 0.6, 1.0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Hazard ratio (log scale)", y = NULL,
       title = "Cause-specific vs subdistribution HR", subtitle = "Adjusted for covariates") +
  theme_mc(width_mm = 85) +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 1, unit = "mm"),
    plot.title = element_text(size = 8.2, face = "bold"),
    plot.subtitle = element_text(size = 6.9),
    plot.margin = margin(t = 2, r = 10, b = 2, l = 1, "mm")
  )

fig5 <- mc_compose(p5a, p5b, p5c, p5d,
                   design = "AB\nCD",
                   width_mm = 180, height_mm = 165)
mc_save(fig5, "figures/fig5_competing_mortality.png", width_mm = 180, height_mm = 165)
